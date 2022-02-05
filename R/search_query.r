
#' Match text to queries using a Lucene-like search query
#'
#' @description
#' This function is mainly used
#'
#'
#' @param tc a \code{\link{tCorpus}}
#' @param queries A character string that is a query. See details for available query operators and modifiers. Can be multiple queries (as a vector), in which case it is recommended to also specifiy the code argument, to label results.
#' @param code The code given to the tokens that match the query (useful when looking for multiple queries). Can also put code label in query with # (see details)
#' @param feature The name of the feature column within which to search.
#' @param mode There are two modes: "hits" and "terms". The "hits" mode prioritizes finding full and unique matches.,
#'             which is recommended for counting how often a query occurs. However, this also means that some tokens
#'             for which the query is satisfied might not assigned a hit_id. The "terms" mode, instead, prioritizes
#'             finding all tokens/terms.
#' @param context_level Select whether the queries should occur within while "documents" or specific "sentences".
#' @param keep_longest If TRUE, then overlapping in case of overlapping queries strings in unique_hits mode, the query with the most separate terms is kept. For example, in the text "mr. Bob Smith", the query [smith OR "bob smith"] would match "Bob" and "Smith". If keep_longest is FALSE, the match that is used is determined by the order in the query itself. The same query would then match only "Smith".
#' @param as_ascii if TRUE, perform search in ascii.
#'
#' @details
#' Brief summary of the query language
#'
#' The following operators and modifiers are supported:
#' \itemize{
#'    \item{The standaard Boolean operators: AND, OR and NOT. As a shorthand, an empty space can be used as an OR statement, so that "this that those" means "this OR that OR those". NOT statements stricly mean AND NOT, so should only be used between terms. If you want to find \emph{everything except} certain terms, you can use * (wildcard for \emph{anything}) like this: "* NOT (this that those)".}
#'    \item{For complex queries parentheses can (and should) be used. e.g. '(spam AND eggs) NOT (fish and (chips OR albatros))}
#'    \item{Wildcards ? and *. The questionmark can be used to match 1 unknown character or no character at all, e.g. "?at" would find "cat", "hat" and "at". The asterisk can be used to match any number of unknown characters. Both the asterisk and questionmark can be used at the start, end and within a term.}
#'    \item{Multitoken strings, or exact strings, can be specified using quotes. e.g. "united states"}
#'    \item{tokens within a given token distance can be found using quotes plus tilde and a number specifiying the token distance. e.g. "climate chang*"~10}
#'    \item{Alternatively, angle brackets (<>) can be used instead of quotes, which also enables nesting exact strings in proximity/window search}
#'    \item{Queries are not case sensitive, but can be made so by adding the ~s flag. e.g. COP~s only finds "COP" in uppercase. The ~s flag can also be used on parentheses or quotes to make all terms within case sensitive, and this can be combined with the token proximity flag. e.g. "Marco Polo"~s10}
#'    \item{The ~g (ghost) flag can be used to mark a term (or all terms within parentheses/quotes) as a ghost term. This has two effects. Firstly, features that match the query term will not be in the results. This is usefull if a certain term is important for getting reliable search results, but not conceptually relevant. Secondly, ghost terms can be used multiple times, in different query hits (only relevant in unique_hits mode). For example, in the text "A B C", the query 'A~g AND (B C)' will return both B and C as separate hit, whereas 'A AND (B C)' will return A and B as a single hit.}
#'    \item{A code label can be included at the beginning of a query, followed by a # to start the query (label# query). Note that to search for a hashtag symbol, you need to escape it with \ (double \\ in R character vector)}
#'    \item{Aside from the feature column (specified with the feature argument) a query can include any column in the token data. To manually select a column, use 'columnname: ' at the start of a query or nested query (i.e. between parentheses or quotes). See examples for clarification.}
#'    }
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

  search_query(d, paste0('"', test$string, '"'))
}
