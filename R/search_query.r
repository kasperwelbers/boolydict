
#' Match text to queries using a Lucene-like search query
#'
#' @description
#' This function matches a vector of queries to a data.frame with a text column. The output is intentionally very bare bones, so that the function can be used as a
#' basis for more convenient applications, such as query_subset, query_join and query_aggregate.
#'
#'
#' @param df   A data.frame (or tibble, data.table, whatever)
#' @param queries A character string that is a query. See \code{\link{query_syntax}} for available query operators and modifiers. Can be multiple queries (as a vector).
#' @param text The column in df with the text to query. Defaults to 'text'. (see \code{\link{query_syntax}} if you want to use multiple columns in a query)
#' @param context Optionally, a column in df with context ids. If used, texts across rows are grouped together so that you can perform Boolean queries across rows.
#'                The primary use case is if texts are tokens/words, such as produced by tidytext, udpipe or spacyr.
#'
#' @param index Optionally, a column in df with indices for texts within a context. In particular, if texts are tokens, these are the token positions. This is only
#'              relevant if not all tokens are used, and we therefore don't know these positions. The indices then need to be provided to correctly match multitoken strings and proximity queries.
#' @param mode There are two modes: "hits" and "terms". The "hits" mode prioritizes finding full and unique matches.,
#'             which is recommended for counting how often a query occurs. However, this also means that some tokens
#'             for which the query is satisfied might not assigned a hit_id. The "terms" mode, instead, prioritizes
#'             finding all tokens/terms.
#' @param keep_longest If TRUE, then overlapping in case of overlapping queries strings in unique_hits mode, the query with the most separate terms is kept. For example, in the text "mr. Bob Smith", the query [smith OR "bob smith"] would match "Bob" and "Smith". If keep_longest is FALSE, the match that is used is determined by the order in the query itself. The same query would then match only "Smith".
#' @param as_ascii if TRUE, perform search in ascii. Can be useful if you know text contains things like accents, and these are either used inconsistently or you simply
#'                 can't be bothered to type them in your queries.
#'
#'
#'
#' @return A data.table with matches, specifying the index of the data (data_index), the index of the query (query_index) and a unique hit_id.
#' @export
#' @examples
#'
#' # the simplest case is to search in a vector of texts
#' text = c("some example text", "some more text")
#' search_query(text, queries = c("some", "<example text>"))
#'
#' # in the results, the query_index tells which query was matched, data_index tells
#' # the index of the text, and hit_id gives an id for each match (unique within query_index)
#'
#' # we can also specify a context in which text occurs, in which case Boolean operators work across rows. This
#' # for instance means that we can also query specific tokens.
#'
#' d = data.frame(token = c("some","example","text", "some", "more", "text"),
#'                doc_id = c(1,1,1,2,2,2))
#' search_query(d, text='token', context='doc_id', queries="example AND text")
#'
#' # note that the data_index now points to the specific tokens that are matched, and
#' # that they have the same hit_id because it's the same match.
#'
#'
#' \donttest{
#'
#' ## query language examples
#'
#' text = c('A B C', 'D E F. G H I', 'A D', 'GGG')
#'
#' ## single term
#' search_query(text, 'A')
#'
#' search_query(text, 'G*')    ## wildcard *
#' search_query(text, '*G')    ## wildcard *
#' search_query(text, 'G*G')   ## wildcard *
#'
#' search_query(text, 'G?G')   ## wildcard ?
#' search_query(text, 'G?')    ## wildcard ? (no hits)
#'
#' ## boolean
#' search_query(text, 'A AND B')
#' search_query(text, 'A AND D')
#' search_query(text, 'A AND (B OR D)')
#'
#' search_query(text, 'A NOT B')
#' search_query(text, 'A NOT (B OR D)')
#'
#'
#' ## sequence search (adjacent words)
#' search_query(text, '"A B"')
#' search_query(text, '"A C"') ## no hit, because not adjacent
#'
#' search_query(text, '"A (B OR D)"') ## can contain nested OR
#' ## cannot contain nested AND or NOT!!
#'
#' search_query(text, '<A B>') ## can also use <> instead of "".
#'
#' ## proximity search (using ~ flag)
#' search_query(text, '"A C"~5') ## A AND C within a 5 word window
#' search_query(text, '"A C"~1') ## no hit, because A and C more than 1 word apart
#'
#' search_query(text, '"A (B OR D)"~5') ## can contain nested OR
#' search_query(text, '"A <B C>"~5')    ## can contain nested sequence (must use <>)
#' search_query(text, '<A <B C>>~5')    ## <> is always OK, but cannot nest "" in ""
#' ## cannot contain nested AND or NOT!!
#'
#' ## case sensitive search (~s flag)
#' search_query(text, 'g')     ## normally case insensitive
#' search_query(text, 'g~s')   ## use ~s flag to make term case sensitive
#'
#' search_query(text, '(a OR g)~s')   ## use ~s flag on everything between parentheses
#' search_query(text, '(a OR G)~s')
#'
#' search_query(text, '"a b"~s')   ## use ~s flag on everything between quotes
#' search_query(text, '"A B"~s')   ## use ~s flag on everything between quotes
#'
#' ## ghost terms (~g flag)
#' d = data.frame(text = c('A','B'), group=c(1,1))
#' search_query(d, context='group', 'A AND B~g')    ## ghost term (~g) has to occur, but is not returned
#' search_query(text, 'A AND Q~g')    ## no hit
#'
#' # (can also be used on parentheses/quotes/anglebrackets for all nested terms)
#'
#'
#' ## "unique_hits" versus "features" mode
#' text = 'A A B'
#'
#' search_query(text, 'A AND B') ## in "unique_hits" (default), only match full queries
#' # (B is not repeated to find a second match of A AND B)
#'
#' search_query(text, 'A AND B', mode = 'features') ## in "features", match any match
#' # (note that hit_id in features mode is irrelevant)
#'
#' # ghost terms (used for conditions) can be repeated
#' search_query(text, 'A AND B~g')
#'
#' }
search_query <- function(df, queries, text='text', context=NULL, index=NULL, mode = c('hits','terms'), keep_longest=TRUE, as_ascii=F){
  mode = match.arg(mode)
  if (!is.data.frame(df)) df = data.table::data.table(text=df)
  if (!text %in% colnames(df)) stop(sprintf('text (%s) is not available', text))

  hits = perform_query_search(df, queries, mode, text, index, context, as_ascii)
  if (nrow(hits) == 0) return(data.table::data.table(term=character(), query_index=numeric(), hit_id=numeric(), data_index=numeric()))

  ## exact multiword strings are collapsed for efficiency. Here we flatten them again to get all the data_indices
  n_data_indices = sapply(hits$data_indices, length)
  if (any(n_data_indices != 1)) {
    di = unlist(hits$data_indices)
    hits = hits[rep(1:nrow(hits), n_data_indices),]
    hits$data_index = di
  } else {
    hits$data_index = unlist(hits$data_indices)
  }

  hits$data_indices = NULL

  if (mode == 'hits') hits = unique(hits[, c('data_index','query_index','hit_id')])
  if (mode == 'terms') hits = unique(hits[,c('data_index','query_index','term')])
  data.table::setorderv(hits, c('data_index','query_index'))
  hits
}


perform_query_search <- function(df, queries, mode, text, index, context, as_ascii) {
  hit_id = NULL
  ## first parses the queries, which gives the prepared queries split into dictionary_terms and advanced_queries,
  ## and gives the lookup_terms used in these queries.
  queries = parse_queries(queries, feature = text) ## use text as default feature to query on (but queries can also refer to specific other features)

  ## then lookup the terms
  dict_results = get_dict_results(df, queries$lookup_terms, text, index=index, context=context, as_ascii=as_ascii)

  ## process the (less expensive) dictionary_terms
  if (!is.null(queries$dictionary_terms)) {
    text_results = dict_results[["text"]] ## dictionary terms always only use the specified text column (unlike adv, which can have columns specified in the query)
    dictionary_hits = merge(text_results[,c('term','data_indices')], queries$dictionary_terms, by='term')
    dictionary_hits[, hit_id := 1:length(term), by='query_index']
    data.table::setcolorder(dictionary_hits, c('term','query_index','hit_id','data_indices'))
  } else dictionary_hits = NULL

  ## process the advanced query terms
  if (!is.null(queries$advanced_queries)) {
    advanced_hits = lapply(1:nrow(queries$advanced_queries), function(i) {
      h = lucene_like(dict_results, queries$advanced_queries$query[[i]], mode=mode,  keep_longest=keep_longest)
      if (!is.null(h)) h[, query_index := queries$advanced_queries$query_index[i]]
      h[,c('term','query_index','hit_id','data_indices')]
    })
    advanced_hits = data.table::rbindlist(advanced_hits, fill = T)
  } else advanced_hits = NULL

  rbindlist(list(dictionary_hits, advanced_hits))
}







function() {
  df = data.frame(text = c("dit", "is", "een voorbeeld om", "me te", "testen", "and", "this"),
                  group = c(1,1,1,1,1,2,2),
                  token_id = c(1,4,5,6,7,1,4))
  queries = c('een om', '<om me>', 'voorbeeld', '<te testen>')
  search_query(df, queries, context='group', index='token_id')

  parse_queries(queries)

  #options(warn = 2)
  #options(warn = 0)

  parse_queries(c('test', 'dit'))
  #'
  #' hits = search_features(tc, c('query label# A AND B', 'second query# (A AND Q) OR ("D E") OR I'))
  #' hits          ## print shows number of hits
  #' hits$hits     ## hits is a list, with hits$hits being a data.frame with specific features
  #' summary(hits) ## summary gives hits per query
  #'
  #'
  #'
  df = data.table::as.data.table(df)
  test = df[,list(text=list(text)), by='group']
  test$text

  d = corpustools::sotu_texts
  melt_quanteda_dict(quanteda::data_dictionary_LSD2015)
  search_query(d, paste0('"', test$string, '"'))
}
