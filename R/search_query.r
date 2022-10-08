

search_query <- function(df, queries, text_col='text', context_col=NULL, index_col=NULL, mode = c('hits','terms','contexts'), keep_longest=TRUE, as_ascii=F, use_wildcards=TRUE, cache=NULL){
  mode = match.arg(mode)
  if (!is.data.frame(df)) df = data.table::data.table(text=df)
  if (!text_col %in% colnames(df)) stop(sprintf('text (%s) is not available', text_col))
  if (mode == 'contexts' && is.null(context_col)) stop('Mode cannot be contexts if no context_col is specified')

  hits = perform_query_search(df, queries, mode, text_col, index_col, context_col, as_ascii, use_wildcards, cache)
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

  if (mode == 'hits') {
    hits = unique(hits[, c('data_index','query_index','hit_id')])
    data.table::setorderv(hits, c('data_index','query_index'))
  }
  if (mode == 'terms') {
    hits = unique(hits[,c('data_index','query_index','term')])
    data.table::setorderv(hits, c('data_index','query_index'))
  }
  if (mode == 'contexts') {
    hits$context = df[[context_col]][hits$data_index]
    hits = unique(hits[,c('context','query_index')])
    data.table::setorderv(hits, c('context','query_index'))
  }
  hits
}


perform_query_search <- function(df, queries, mode, text, index, context, as_ascii, use_wildcards, cache) {
  hit_id = NULL
  ## first parses the queries, which gives the prepared queries split into dictionary_terms and advanced_queries,
  ## and gives the lookup_terms used in these queries.
  queries = parse_queries(queries, feature = text) ## use text as default feature to query on (but queries can also refer to specific other features)

  ## then lookup the terms
  dict_results = get_dict_results(df, queries$lookup_terms, text, index=index, context=context, as_ascii=as_ascii, use_wildcards=use_wildcards, cache=cache)

  ## process the (less expensive) dictionary_terms
  if (!is.null(queries$dictionary_terms)) {
    text_results = dict_results[[text]] ## dictionary terms always only use the specified text column (unlike adv, which can have columns specified in the query)
    dictionary_hits = merge(text_results[,c('term','data_indices')], queries$dictionary_terms, by='term')
    if (nrow(dictionary_hits) == 0) {
      dictionary_hits = NULL
    } else {
      dictionary_hits[, hit_id := 1:length(term), by='query_index']
      data.table::setcolorder(dictionary_hits, c('term','query_index','hit_id','data_indices'))
    }
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





