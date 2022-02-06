parse_queries <- function(q, feature='', optimize_OR=T, optimize_seq=T) {
  q = stringi::stri_trim(q)

  ## queries are split into dictionary_terms and advanced_queries, because dictionary_terms
  ## are less expensive. Both do use the same algorithm for looking up the individual terms
  ## so the lookup terms are pooled
  adv = is_advanced(q)

  ## parse advanced queries
  if (any(adv)) {
    queries = lapply(q[adv], parse_advanced_query, feature=feature, optimize_OR=optimize_OR, optimize_seq=optimize_seq)
    advanced_queries = data.table::data.table(query_index = which(adv), query = lapply(queries, function(x) x$query))
    query_lookup_terms = data.table::rbindlist(lapply(queries, function(x) x$query_terms), fill = T)
  } else {
    advanced_queries = NULL
    query_lookup_terms = NULL
  }

  ## parse dictionary queries
  if (any(!adv)) {
    dict_lookup_terms = parse_dictionary_term(q[!adv], feature)
    dictionary_terms = data.table::data.table(query_index = which(!adv), term=dict_lookup_terms$term)
  } else {
    dict_lookup_terms = NULL
    dictionary_terms = NULL
  }
  ## merge the terms that need to be looked for
  lookup_terms = unique(rbind(query_lookup_terms, dict_lookup_terms))
  data.table::setkeyv(lookup_terms, 'feature')

  list(advanced_queries = advanced_queries, dictionary_terms = dictionary_terms, lookup_terms = lookup_terms)
}


#' Check if string is a simple dictionary term or advanced query
#'
#' @param q   a character vector
#'
#' @return A logical vector
#' @export
#'
#' @examples
#'
#' queries = c('this OR that',    ## boolean, so advanced
#'             'this that',       ## space means OR, so advanced
#'             'single',          ## single term, so basic
#'             '<simple multiword str*>')  ## quoted, so basic
#'
#' is_advanced(queries)
is_advanced <- function(q) {
  ## a query can be a simple dictionary query or an advanced boolean query.
  q = gsub('~s$', '', q)   ## only allowed flag

  adv_symbols = '\\b(OR|NOT|AND)\\b|[(){}\\[\\]~]'
  has_adv_symbol = stringi::stri_detect(q, regex=adv_symbols)

  terms = stringi::stri_split_boundaries(gsub('\\?|\\*', '', q), type='word')
  multiterm = sapply(terms, length) > 1
  quoted = stringi::stri_detect(q, regex='^[<"]') & stringi::stri_detect(q, regex='[>"]$')

  has_adv_symbol | (multiterm & !quoted)
}

parse_dictionary_term <- function(q, feature='') {
  case_sensitive = stringi::stri_detect(q, regex='~s$')
  q = gsub('~s$', '', q)
  q = gsub('^[<"]|[>"]$', '', q)
  data.table::data.table(term=q, feature=feature, case_sensitive=case_sensitive, ghost=FALSE)
}


parse_advanced_query <- function(q, feature='', optimize_OR=T, optimize_seq=T) {
  q = parse_query_cpp(q)
  q = simplify_query(q, feature=feature)
  if (optimize_OR) q = optimize_query(q, collapse_or_queries)
  if (optimize_seq) q = optimize_query(q, collapse_sequence_queries)
  list(query = q,
       query_terms = query_terms(q))
}




optimize_query <- function(q, fun) {
  q = fun(q)
  for (i in seq_along(q$terms)) {
    if ('terms' %in% names(q$terms[[i]])) q$terms[[i]] = optimize_query(q$terms[[i]], fun)
  }
  q
}


collapse_or_queries <- function(qlist) {
  if (qlist$relation == 'OR') {
    nested = sapply(qlist$terms, function(x) 'terms' %in% names(x))
    has_flag_query = sapply(qlist$terms, function(x) length(x$flag_query) > 0)
    select = !nested & !has_flag_query # these terms are collapse-able

    if (sum(select) > 1) {
      terms = sapply(qlist$terms[select], function(x) x[c('feature','case_sensitive','ghost','term')], simplify = F)
      terms = data.table::rbindlist(terms)
      col_terms = stats::aggregate(term ~ feature + case_sensitive + ghost, data=terms, FUN = c, simplify=F)
      col_terms = apply(col_terms, 1, as.list)
      col_terms = sapply(col_terms, function(x) c(x, list(flag_query=list())), simplify = F)
      qlist$terms = c(col_terms, qlist$terms[!select])
    }
  }
  qlist
}

collapse_sequence_queries <- function(qlist) {
  if (qlist$relation == 'sequence') {
    has_nested = any(sapply(qlist$terms, function(x) 'terms' %in% names(x)))
    if (has_nested) return(qlist)
    diff_conditions = length(unique(lapply(qlist$terms, function(x) list(x$ghost, x$case_sensitive, x$feature, x$flag_query)))) > 1
    if (diff_conditions) return(qlist)


    terms = sapply(qlist$terms, function(x) x$term)
    qlist$flag_query = qlist$terms[[1]]$flag_query
    qlist$ghost = qlist$terms[[1]]$ghost
    qlist$case_sensitive = qlist$terms[[1]]$case_sensitive
    qlist$feature = qlist$terms[[1]]$feature
    qlist$term = paste(terms, collapse=' ')
    qlist$terms = NULL
    qlist$relation = NULL
    #terms = sapply(qlist$terms[select], function(x) x[c('feature','case_sensitive','ghost','term')], simplify = F)
    #terms = data.table::rbindlist(terms)
    ##col_terms = stats::aggregate(term ~ feature + case_sensitive + ghost, data=terms, FUN = c, simplify=F)
    #col_terms = apply(col_terms, 1, as.list)
    #col_terms = sapply(col_terms, function(x) c(x, list(flag_query=list())), simplify = F)
    #qlist$terms = c(col_terms, qlist$terms[!select])
  }
  qlist
}


simplify_query <- function(q, feature='', all_case_sensitive=F, all_ghost=F, all_flag_query=list()) {
  if (!q$feature == "") feature = q[['feature']]
  if (q$all_case_sensitive) all_case_sensitive = TRUE
  if (q$all_ghost) all_ghost = TRUE
  for (n in names(q$all_flag_query)) all_flag_query[[n]] = unique(c(all_flag_query[[n]], q$all_flag_query[[n]]))
  q$feature = q$all_case_sensitive = q$all_ghost = q$all_flag_query = NULL

  for (i in seq_along(q$terms)) {
    if ('terms' %in% names(q$terms[[i]])) q$terms[[i]] = simplify_query(q$terms[[i]], feature, all_case_sensitive, all_ghost, all_flag_query)

    if ('term' %in% names(q$terms[[i]])) {
      if (all_case_sensitive) q$terms[[i]]$case_sensitive = T
      if (all_ghost) q$terms[[i]]$ghost = T
      q$terms[[i]]$feature = feature
      for (n in names(all_flag_query)) q$terms[[i]]$flag_query[[n]] = unique(c(q$terms[[i]]$flag_query[[n]], all_flag_query[[n]]))

      if (!q$terms[[i]]$case_sensitive) q$terms[[i]]$term = stringi::stri_trans_tolower(q$terms[[i]]$term)
    }
  }
  q
}

query_terms <- function(q) {
  qd = data.table::rbindlist(get_query_terms(q), fill=T)
  unique(qd)
}

get_query_terms <- function(q) {
  terms = list()
  for (i in seq_along(q$terms)) {
    if ('terms' %in% names(q$terms[[i]])) terms = c(terms, get_query_terms(q$terms[[i]]))
    if ('term' %in% names(q$terms[[i]])) {
      term = data.table::data.table(term = q$terms[[i]]$term, feature = q$terms[[i]]$feature,
                                    case_sensitive = q$terms[[i]]$case_sensitive, ghost=q$terms[[i]]$ghost)
      fq = q$terms[[i]]$flag_query
      if ('tokenexpr' %in% names(fq)) term$token_expr = merge_str_expressions(fq[['tokenexpr']])
      if ('metaexpr' %in% names(fq)) term$meta_expr = merge_str_expressions(fq[['metaexpr']])
      terms[['']] = term
    }
  }
  terms
}

merge_str_expressions <- function(x) {
  x = x[!x == '']
  paste(paste0('(',x,')'), collapse=' & ')
}

