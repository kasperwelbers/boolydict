#' @name dictionary_function
#' @rdname dictionary_function
#'
#' @title Match a dictionary to a text column in a data.frame
#'
#' @description
#' Different methods for matching a dictionary to a data.frame. dict_match is the most bare bones version, which returns for each match the indices of
#' the df and dict data.frames. This can be used for all sorts of filtering and joining. The other functions are more convenience functions for how to use this.
#' dict_summarize is probably what you need if you're doing text analysis. It joins the dictionary to df, but immediately summarizes it so that rows in df do not need to
#' be duplicated.
#'
#'
#' @param df          A data.frame (or tibble, data.table, whatever). The column name specified in the text_col argument (default "text") will be matched to the dictionary
#' @param dict        A dictionary data.frame or a character vector. If data.frame, needs to have a column called 'string'. When importing a dictionary (e.g., from quanteda.dictionaries or textdata),
#'                    please check out \code{\link{import_dictionary}}. If a character vector, can be a named character vector, in which case the names will
#'                    be available as label.
#' @param text_col    The column in df with the text to query. Defaults to 'text'.
#' @param context_col Optionally, a column in df with context ids. If used, texts across rows are grouped together so that you can perform Boolean queries across rows.
#'                    The primary use case is if texts are tokens/words, such as produced by tidytext, udpipe or spacyr.
#'
#' @param index_col   Optionally, a column in df with indices for texts within a context. In particular, if texts are tokens, these are the token positions. This is only
#'                    relevant if not all tokens are used, and we therefore don't know these positions. The indices then need to be provided to correctly match multitoken strings and proximity queries.
#' @param mode        There are two modes: "hits" and "terms". The "hits" mode prioritizes finding full and unique matches.
#'                    For example, if we query <climate chang*>~10, then in the text "climate change is changing the world" we'll only find
#'                    one unique hit for "climate change". Alternatively, in "terms" mode we would match "climate", "change" and "changing".
#'                    "Hits" mode is often what you want for counting occurrences. "terms" mode is especially useful if you are matching a dictionary to
#'                    tokens, and want to match every token that satisfies the query. In dict_summarize, the summary is calculated over unique hit_id's in "hits" mode,
#'                    and over all terms in "terms" mode.
#' @param keep_longest If TRUE, then overlapping in case of overlapping queries strings in unique_hits mode, the query with the most separate terms is kept. For example, in the text "mr. Bob Smith", the query [smith OR "bob smith"] would match "Bob" and "Smith". If keep_longest is FALSE, the match that is used is determined by the order in the query itself. The same query would then match only "Smith".
#' @param as_ascii    if TRUE, perform search in ascii. Can be useful if you know text contains things like accents, and these are either used inconsistently or you simply
#'                    can't be bothered to type them in your queries.
#' @param keep_context in dict_filter. If TRUE, then all rows within a context are selected if at least one of the rows matches the dictionary.
#' @param ...         In dict_summarize. name-value pairs where the name becomes a new column, and the value is an expression for summarizing a column in dict. For example,
#'                    if dict has a 'sentiment' column, you can say: sentiment = mean(sentiment). If left empty, then dict_summarize will create a column named "hits" with the
#'                    hit count.
#' @param by_label    In dict_summarize. The name of a column in dict with labels. If given, the summary will be calculated per label, with each summary getting it's own
#'                    column. The name of the column will be [summary_name].[label]
#' @param fill        In dict_summarize. How to fill NA values.
#'
#' @return A data.frame.
#' @examples
#' full_text = data.frame(text = c('This is just a simple example', 'Simple is good'))
#' tokens = data.frame(
#'    text = c('This','is','just','a','simple','example', 'Simple', 'is','good'),
#'    doc_id = c(1,1,1,1,1,1,2,2,2))
#'
#' dict = data.frame(string = c('<this is just>', '<a example>~3'),
#'                   label = c('FIRST QUERY','SECOND QUERY'),
#'                   value =  c(1, 100))
NULL

#' @rdname dictionary_function
#' @return dict_match: A data.table with matches, specifying the index of the df (data_index), the index of the dict (dict_index) and a unique hit_id.
#' @export
#' @examples
#' #' ##### dict_match #####
#'
#' ## full text matches the text twice, once for each query
#' dict_match(full_text, dict)
#'
#' ## tokens in context matches token 1-3 for query 1, and 4&6 for query 2.
#' ## note that the hit_id also shows that 1-3 belong together (hit_id 1 for query 1)
#' dict_match(tokens, dict, context_col='doc_id')
dict_match <- function(df, dict, text_col='text', context_col=NULL, index_col=NULL, mode=c('hits','terms'), keep_longest=TRUE, as_ascii=FALSE) {
  if (!is.data.frame(dict)) dict = data.table::data.table(string=dict, label=names(dict))
  mode = match.arg(mode)

  matches = search_query(df, dict$string, text_col=text_col,
               context_col=context_col, index_col=index_col,
               mode = mode, keep_longest=keep_longest, as_ascii=as_ascii)
  data.table::setnames(matches, 'query_index','dict_index')
  matches
}

#' @rdname dictionary_function
#' @return dict_filter: The input df, filtered on the matched rows
#' @export
#' @examples
#' ##### dict_filter #####
#'
#' ## returns the matched row
#' dict_filter(full_text, dict)
#'
#' ## dict can also be a character vector for a quick lookup
#' dict_filter(full_text, 'simple AND good')
#'
#' ## for rows in a context, by default returns every matched context
#' dict_filter(tokens, dict, context_col='doc_id')
#'
#' ## but can also return just the matched rows
#' dict_filter(tokens, dict, context_col='doc_id', keep_context=FALSE)
dict_filter <- function(df, dict, keep_context=TRUE, text_col='text', context_col=NULL, index_col=NULL, mode=c('hits','terms'), keep_longest=TRUE, as_ascii=FALSE) {
  if (!is.data.frame(dict)) dict = data.table::data.table(string=dict, label=names(dict))
  mode = match.arg(mode)

  matches = search_query(df, dict$string, text_col=text_col,
                         context_col=context_col, index_col=index_col,
                         mode = mode, keep_longest=keep_longest, as_ascii=as_ascii)

  if (keep_context && !is.null(context_col)) {
    selected_context = unique(df[[context_col]][unique(matches$data_index)])
    lookup_table = data.table::data.table(i=1:nrow(df), context=df[[context_col]], key='context')
    df_i = lookup_table[list(selected_context),,on='context']$i
  } else {
    df_i = unique(matches$data_index)
  }

  df[df_i,,drop=F]
}

#' @rdname dictionary_function
#' @return dict_summarize: The input df with columns added for the aggregated dictionary matches
#' @export
#' @examples
#' ##### dict_summarize #####
#'
#' ## by default just counts
#' dict_summarize(full_text, dict, fill=0)
#'
#' ## can count 'by_label', for any categorical column in dict
#' dict_summarize(full_text, dict, by_label='label', fill=0)
#'
#' ## can use custom aggregation function
#' dict_summarize(full_text, dict, value = sum(value), by_label='label', fill=0)
#'
#' ## works the same for tokens. But if the goal is to annotate tokens, you
#' ## want to set mode to 'terms' so that every match is used
#' dict_summarize(tokens, dict, context_col='doc_id', mode='terms')
#'
#' ## you could use by_label to label tokens, but if you just want a unique label
#' ## you could use the custom summarize function to select a label. We'll use a
#' ## different dict to show this.
#'
#' dict = data.frame(string = c('simple OR good', 'just OR simple'),
#'                   label = c('FIRST QUERY', 'SECOND QUERY'))
#' ##One option is to just pick the first. Matches are sorted by dict_index, so this
#' ## way queries higher in the dict df get priority.
#' dict_summarize(tokens, dict, context_col='doc_id', mode='terms',
#'                label = label[1])
dict_summarize <- function(df, dict, ..., by_label=NULL, fill=NULL, text_col='text', context_col=NULL, index_col=NULL, mode=c('hits','terms'), keep_longest=TRUE, as_ascii=FALSE) {
  if (!is.data.frame(dict)) dict = data.table::data.table(string=dict, label=names(dict))
  mode = match.arg(mode)
  agg_list = substitute(list(...))

  ## if length of substituted list is 1, it's empty
  if (length(agg_list) == 1) agg_list = substitute(list(hits = .N))

  matches = search_query(df, dict$string, text_col=text_col,
                      context_col=context_col, index_col=index_col,
                      mode = mode, keep_longest=keep_longest, as_ascii=as_ascii)

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
