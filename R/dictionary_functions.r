#' @title Match a dictionary to a text column in a data.frame
#'
#' @description
#' dict_match is the most bare bones version of the dict_* functions. It returns for each match the indices of
#' the df and dict data.frames. This can be used for all sorts of filtering and joining, but the most common use
#' cases are also facilitated by the \code{\link{dict_filter}} and \code{\link{dict_add}} functions.
#'
#'
#'
#' @param df          A data.frame (or tibble, data.table, whatever). The column name specified in the text_col argument (default "text") will be matched to the dictionary
#' @param dict        A dictionary data.frame or a character vector. If data.frame, needs to have a column called 'string'. When importing a dictionary (e.g., from quanteda.dictionaries or textdata),
#'                    please check out \code{\link{import_dictionary}}.
#' @param text_col    The column in df with the text to query. Defaults to 'text'.
#' @param context_col Optionally, a column in df with context ids. If used, texts across rows are grouped together so that you can perform Boolean queries across rows.
#'                    The primary use case is if texts are tokens/words, such as produced by tidytext, udpipe or spacyr.
#' @param index_col   Optionally, a column in df with indices for texts within a context. In particular, if texts are tokens, these are the token positions. This is only
#'                    relevant if not all tokens are used, and we therefore don't know these positions. The indices then need to be provided to correctly match multitoken strings and proximity queries.
#' @param mode        There are three modes: "hits" and "terms" and "unique". The "hits" mode prioritizes finding full and unique matches.
#'                    For example, if we query <climate chang*>~10, then in the text "climate change is changing the world" we'll only find
#'                    one unique hit for "climate change". Alternatively, in "terms" mode we would match "climate", "change" and "changing".
#'                    "hits" mode is often what you want for counting occurrences. "terms" mode is especially useful if you are matching a dictionary to
#'                    tokens, and want to match every token that satisfies the query.
#' @param keep_longest If TRUE, then overlapping in case of overlapping queries strings in unique_hits mode, the query with the most separate terms is kept. For example, in the text "mr. Bob Smith", the query [smith OR "bob smith"] would match "Bob" and "Smith". If keep_longest is FALSE, the match that is used is determined by the order in the query itself. The same query would then match only "Smith".
#' @param as_ascii    if TRUE, perform search in ascii. Can be useful if you know text contains things like accents, and these are either used inconsistently or you simply
#'                    can't be bothered to type them in your queries.
#' @param use_wildcards Set to FALSE if you want to disable wildcards. For instance useful if you have a huge dictionary without wildcards that might have ? or * in emoticons and stuff.
#'                      Note that you can also always escape wildcards with a double backslash (\\? or \\*)
#'
#' @return dict_match: A data.table with matches, specifying the index of the df (data_index) and the index of the dict (dict_index). If mode = 'hits', a hit_id column indicates
#'         which matches of the same dict_index are part of the same hit. If mode = 'terms', a 'term' column shows which terms were matched.
#' @export
#' @examples
#' dict = data.frame(string = c('<this is just>', '<a example>~3'))
#'
#' ## full text matches the text twice, once for each query
#' full_text = data.frame(text = c('This is just a simple example', 'Simple is good'))
#' dict_match(full_text, dict)
#'
#' ## tokens in context 'doc_id' matches token 1-3 for query 1, and 4&6 for query 2.
#' ## note that the hit_id also shows that 1-3 belong together (hit_id 1 for query 1)
#' tokens = data.frame(
#'    text = c('This','is','just','a','simple','example', 'Simple', 'is','good'),
#'    doc_id = c(1,1,1,1,1,1,2,2,2))
#' dict_match(tokens, dict, context_col='doc_id')
dict_match <- function(df, dict, text_col='text', context_col=NULL, index_col=NULL, mode=c('hits','terms'), keep_longest=TRUE, as_ascii=FALSE, use_wildcards=TRUE) {
  if (!is.data.frame(dict)) dict = data.table::data.table(string=dict, label=names(dict))
  mode = match.arg(mode)

  matches = search_query(df, dict$string, text_col=text_col,
               context_col=context_col, index_col=index_col,
               mode = mode, keep_longest=keep_longest, as_ascii=as_ascii, use_wildcards=use_wildcards)
  data.table::setnames(matches, 'query_index','dict_index')
  matches
}

#' @title Filter a data.frame using a Boolean query
#'
#' @description
#' This is a convenience function for using dictionary search to filter a data.frame.
#'
#' @param keep_context in dict_filter. If TRUE, then all rows within a context are selected if at least one of the rows matches the dictionary.
#' @param mode not used in dict_filter
#' @inheritParams dict_match
#'
#' @return The input df in the original class, filtered on the matched rows
#' @export
#' @examples
#' dict = data.frame(string = c('<this is just>', '<a example>~3'))
#'
#' full_text = data.frame(text = c('This is just a simple example', 'Simple is good'))
#'
#' ## returns the matched row
#' dict_filter(full_text, dict)
#'
#' ## dict can also be a character vector for a simple lookup
#' dict_filter(full_text, 'simple AND good')
#'
#' tokens = data.frame(
#'    text = c('This','is','just','a','simple','example', 'Simple', 'is','good'),
#'    doc_id = c(1,1,1,1,1,1,2,2,2))
#'
#' ## for rows in a context, by default returns every matched context
#' dict_filter(tokens, dict, context_col='doc_id')
#'
#' ## but can also return just the matched rows
#' dict_filter(tokens, dict, context_col='doc_id', keep_context=FALSE)
dict_filter <- function(df, dict, keep_context=TRUE, text_col='text', context_col=NULL, index_col=NULL, keep_longest=TRUE, as_ascii=FALSE, use_wildcards=TRUE) {
  if (!is.data.frame(dict)) dict = data.table::data.table(string=dict, label=names(dict))
  mode = if (keep_context && !is.null(context_col)) 'contexts' else 'terms'

  matches = search_query(df, dict$string, text_col=text_col,
                         context_col=context_col, index_col=index_col,
                         mode = mode, keep_longest=keep_longest, as_ascii=as_ascii, use_wildcards=use_wildcards)

  if (mode == 'contexts') {
    selected_context = unique(matches$context)
    lookup_table = data.table::data.table(i=1:nrow(df), context=df[[context_col]], key='context')
    df_i = lookup_table[list(selected_context),,on='context']$i
  } else {
    df_i = unique(matches$data_index)
  }

  df[df_i,,drop=F]
}

#' @title Add the dictionary matches to a data.frame
#'
#' @description
#' Adding dictionary matches to a data.frame is not trivial because one row in the data.frame can have multiple matches.
#' This function deals with this issue by summarizing the matches. You can select which of the columns in the dictionary (dict)
#' to add by providing a name-value pair (...) , where the name is the new column name and the value is any expression using the
#' columns in dict that returns a scalar (i.e. single value).
#'
#' For example, if dict has a 'sentiment' column, you can say: avg_sentiment = mean(sentiment). Another common use case would be
#' selecting a single value from a categorical variable (e.g., label). You could for instance use label = label[1] to pick the first match. Expressions
#' can use any column in dict, so if the dict has a label and weight column, you could also use: label = label[which.max(weight)]
#'
#' @param ...         name-value pairs where the name becomes a new column, and the value is an expression for summarizing a column in dict.
#' @param by_label    The name of a column in dict with labels. If given, the summary will be calculated per label, with each summary getting it's own
#'                    column. The name of the column will be [summary_name].[label]
#' @param fill        How to fill NA values.
#' @inheritParams dict_match
#'
#' @return The input df with columns added for the aggregated dictionary matches
#' @export
#' @examples
#' full_text = data.frame(text = c('This is just a simple example', 'Simple is good'))
#' tokens = data.frame(
#'    text = c('This','is','just','a','simple','example', 'Simple', 'is','good'),
#'    doc_id = c(1,1,1,1,1,1,2,2,2))
#'
#' dict = data.frame(string = c('<this is just>', '<a example>~3'),
#'                   label = c('FIRST QUERY','SECOND QUERY'),
#'                   value =  c(1, 100))
#'
#' ## by default just counts
#' dict_add(full_text, dict, fill=0)
#'
#' ## can count 'by_label', for any categorical column in dict
#' dict_add(full_text, dict, by_label='label', fill=0)
#'
#' ## can use custom aggregation function
#' dict_add(full_text, dict, value = sum(value), by_label='label', fill=0)
#'
#' ## works the same for tokens. But if the goal is to annotate tokens, you
#' ## want to set mode to 'terms' so that every match is used
#' dict_add(tokens, dict, context_col='doc_id', mode='terms')
#'
#' ## you could use by_label to label tokens, but if you just want a unique label
#' ## you could use the custom summarize function to select a label. We'll use a
#' ## different dict to show this.
#'
#' dict = data.frame(string = c('simple OR good', 'just OR simple'),
#'                   label = c('FIRST QUERY', 'SECOND QUERY'))
#'
#' ##One option is to just pick the first. Matches are sorted by dict_index, so this
#' ## way queries higher in the dict df get priority.
#' dict_add(tokens, dict, context_col='doc_id', mode='terms',
#'                label = label[1])
dict_add <- function(df, dict, ..., by_label=NULL, fill=NULL, text_col='text', context_col=NULL, index_col=NULL, mode=c('hits','terms'), keep_longest=TRUE, as_ascii=FALSE, use_wildcards=TRUE) {
  if (!is.data.frame(dict)) dict = data.table::data.table(string=dict, label=names(dict))
  mode = match.arg(mode)
  agg_list = substitute(list(...))

  ## if length of substituted list is 1, it's empty
  if (length(agg_list) == 1) agg_list = substitute(list(hits = .N))

  matches = search_query(df, dict$string, text_col=text_col,
                      context_col=context_col, index_col=index_col,
                      mode = mode, keep_longest=keep_longest, as_ascii=as_ascii, use_wildcards=use_wildcards)

  matches = cbind(matches, dict[matches$query_index,])

  if (is.null(by_label)) {
    agg = matches[,eval(agg_list), by='data_index']
  } else {
    if (length(by_label) > 1) stop('by_label can only be a single value')
    agg = matches[,eval(agg_list), by=c(by_label, 'data_index')]
    value.var = colnames(agg)[3:ncol(agg)]
    agg = data.table::dcast(agg, sprintf('data_index ~ %s', by_label), sep='.', value.var=value.var)
  }

  ## expand agg so that it matches length of df
  agg = agg[list(1:nrow(df)),,on='data_index'][,-1]
  if (!is.null(fill)) agg[is.na(agg)] = fill

  ## make sure no names overlap
  unique_names = make.unique(c(colnames(df), colnames(agg)))
  colnames(agg) = unique_names[(ncol(df)+1):length(unique_names)]

  cbind(df, agg)
}
