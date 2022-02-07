lucene_like <- function(dict_results, qlist, mode = c('hits','terms','contexts'), subcontext=NULL, parent_relation='', keep_longest=TRUE, level=1) {
  i=NULL; hit_id = NULL; .ghost = NULL; .term_i = NULL; .seq_i = NULL; .group_i = NULL ## for solving CMD check notes (data.table syntax causes "no visible binding" message)
  mode = match.arg(mode)
  hit_list = vector('list', length(qlist$terms))

  nterms = length(qlist$terms)
  for (j in 1:nterms) {
    q = qlist$terms[[j]]
    is_nested = 'terms' %in% names(q)
    if (is_nested) {
      jhits = lucene_like(dict_results, q, mode=mode, subcontext=subcontext, parent_relation=qlist$relation, keep_longest=keep_longest, level=level+1)

      if (nterms == 1) {
        if (level == 1 && mode == 'hits' & !is.null(jhits)) jhits = remove_duplicate_hit_id(jhits, keep_longest)
        if (level == 1 && mode == 'contexts' & !is.null(jhits)) jhits = unique(jhits, by=c('context_id',subcontext))
        return(jhits)
      }
      if (qlist$relation == 'proximity' && q$relation %in% c('proximity','AND')) stop("Cannot nest proximity or AND search within a proximity search")
      if (!is.null(jhits)) {
        if (q$relation == 'sequence') jhits[, .seq_i := .term_i]
        if (qlist$relation %in% c('proximity','sequence','AND')) jhits[, .group_i := paste(j, .group_i, sep='_')] ## for keeping track of nested multi word queries
      }
    } else {
      #st = Sys.time()
      only_context = mode == 'contexts' && qlist$relation %in% c('AND','NOT')  ## only_context is more efficient, but can only be used for AND and NOT, because proximity and sequence require feature positions (and OR can be nested in them)

      jhits = lookup(dict_results, q$term, feature=q$feature, case_sensitive=q$case_sensitive, only_context=only_context)

      if (!is.null(jhits)) {
        jhits[, .ghost := q$ghost]
        if (qlist$relation %in% c('proximity','sequence','AND')) jhits[,.group_i := paste0(j, '_')] else jhits[,.group_i := ''] ## for keeping track of nested multi word queries
      }

    }
    if (is.null(jhits)) {
      if (qlist$relation %in% c('AND','proximity','sequence')) return(NULL)
      if (nterms == 1) return(NULL)
      next
    }
    jhits[, .term_i := j]
    if (nrow(jhits) > 0) hit_list[[j]] = jhits
  }

  hits = data.table::rbindlist(hit_list, fill=TRUE)
  if (nrow(hits) == 0) return(NULL)
  feature_mode = mode == 'terms'
  if (parent_relation %in% c('AND','proximity','sequence')) feature_mode = TRUE ## with these parents, hit_id will be recalculated, and all valid terms should be returned
  if (qlist$relation %in% c('AND')) get_AND_hit(hits, n_unique = nterms, subcontext=subcontext, group_i = '.group_i', replace = '.ghost', feature_mode=feature_mode) ## assign hit_ids to groups of tokens within the same context
  if (qlist$relation %in% c('NOT')) get_NOT_hit(hits, n_unique = nterms, subcontext=subcontext, group_i = '.group_i', replace = '.ghost', feature_mode=T)
  if (qlist$relation == 'proximity') get_proximity_hit(hits, n_unique = nterms, window=qlist$window, subcontext=subcontext, seq_i = '.seq_i', replace='.ghost', feature_mode=feature_mode, directed=qlist$directed) ## assign hit_ids to groups of tokens within the given window
  if (qlist$relation %in% c('OR', '')) get_OR_hit(hits)
  if (qlist$relation == 'sequence') get_sequence_hit(hits, seq_length = nterms, subcontext=subcontext) ## assign hit ids to valid sequences


  hits = subset(hits, hit_id > 0)

  if (nrow(hits) == 0) return(NULL)
  if (level == 1 && mode == 'hits') hits = remove_duplicate_hit_id(hits, keep_longest)
  if (level == 1 && mode == 'contexts') hits = unique(hits, by=c('context_id',subcontext))
  return(hits)
}

lookup <- function(dict_results, terms, feature='token', case_sensitive=TRUE, subcontext=NULL, only_context=F){
  if (!feature %in% names(dict_results))
    return(NULL)

  dupl = duplicated(terms)
  if (any(dupl)) {
    terms = terms[!dupl]
    if (length(case_sensitive) > 1) case_sensitive = case_sensitive[!dupl]
  }

  dr = dict_results[[feature]]
  f = data.table::data.table(term=terms, case_sensitive=case_sensitive)
  out = dr[f, allow.cartesian=T, nomatch=0]
  if (nrow(out) == 0) return(NULL)

  out[, case_sensitive := NULL]
  if (only_context) out = unique(out, by=c('context_id',subcontext))
  out
}



get_sequence_hit <- function(d, seq_length, subcontext=NULL){
  hit_id = NULL ## used in data.table syntax, but need to have bindings for R CMD check
  setorderv(d, c('context_id', 'token_id', '.term_i'))
  if (!is.null(subcontext)) subcontext = d[[subcontext]]

  .hit_id = sequence_hit_ids_cpp(as.numeric(d[['context_id']]), as.numeric(subcontext), as.numeric(d[['token_id']]), as.numeric(d[['.term_i']]), seq_length)
  d[,hit_id := .hit_id]
}

get_proximity_hit <- function(d, n_unique, window=NA, subcontext=NULL, seq_i=NULL, replace=NULL, feature_mode=F, directed=F){
  hit_id = NULL ## used in data.table syntax, but need to have bindings for R CMD check
  setorderv(d, c('context_id', 'token_id', '.term_i'))
  if (!is.null(subcontext)) subcontext = d[[subcontext]]
  if (!is.null(seq_i)) seq_i = d[[seq_i]]
  if (!is.null(replace)) replace = d[[replace]]
  .hit_id = proximity_hit_ids_cpp(as.numeric(d[['context_id']]), as.numeric(subcontext), as.numeric(d[['token_id']]), as.numeric(d[['.term_i']]), n_unique, window, as.numeric(seq_i), replace, feature_mode, directed)
  d[,hit_id := .hit_id]
}

get_AND_hit <- function(d, n_unique, subcontext=NULL, group_i=NULL, replace=NULL, feature_mode=F){
  hit_id = NULL ## used in data.table syntax, but need to have bindings for R CMD check
  setorderv(d, c('context_id', 'token_id', '.term_i'))
  if (!is.null(subcontext)) subcontext = d[[subcontext]]
  if (!is.null(group_i)) group_i = d[[group_i]]
  if (!is.null(replace)) replace = d[[replace]]
  .hit_id = AND_hit_ids_cpp(as.numeric(d[['context_id']]), as.numeric(subcontext), as.numeric(d[['.term_i']]), n_unique, as.character(group_i), replace, feature_mode)
  d[,hit_id := .hit_id]
}


get_NOT_hit <- function(d, n_unique, subcontext=NULL, group_i=NULL, replace=NULL, feature_mode=F){
  hit_id = NULL ## used in data.table syntax, but need to have bindings for R CMD check
  setorderv(d, c('context_id', 'token_id', '.term_i'))
  if (!is.null(subcontext)) subcontext = d[[subcontext]]
  if (!is.null(group_i)) group_i = d[[group_i]]
  if (!is.null(replace)) replace = d[[replace]]
  .hit_id = AND_hit_ids_cpp(as.numeric(d[['context_id']]), as.numeric(subcontext), as.numeric(d[['.term_i']]), n_unique, as.character(group_i), replace, feature_mode)
  if (!'hit_id' %in% colnames(d)) d[, hit_id := integer()]
  if (any(is.na(d$hit_id))) d[, hit_id := ifelse(is.na(hit_id), 1:nrow(d), hit_id)]
  d[!(d$.term_i == 1 & .hit_id == 0), hit_id := 0]
}

get_OR_hit <- function(d) {
  hit_id = NULL ## used in data.table syntax, but need to have bindings for R CMD check

  if (!'hit_id' %in% colnames(d)) {
    .hit_id = 1:nrow(d)
  } else .hit_id = d$hit_id

  isna = is.na(.hit_id)
  if (any(isna)) {
    if (all(isna)) na_ids = 1:length(.hit_id) else na_ids = 1:sum(isna) + max(.hit_id, na.rm = T)
    .hit_id[isna] = na_ids
  }
  if ('.term_i' %in% colnames(d)) .hit_id = global_id(d$.term_i, .hit_id)
  d[,hit_id := .hit_id]
}

remove_duplicate_hit_id <- function(d, keep_longest=TRUE) {
  .hit_id_length = .ghost = hit_id = ngram = NULL ## for solving CMD check notes (data.table syntax causes "no visible binding" message)
  if (!'token_id' %in% colnames(d)) return(d)

  ## first ignore duplicates within same hit_id
  ## this can happen if different feature columns are used
  if (!'.ghost' %in% colnames(d)) d$.ghost = F
  d = subset(d, !duplicated(d[,c('context_id','token_id','hit_id','.ghost')]))

  dup = duplicated(d[,c('context_id','token_id')]) & !d$.ghost

  if (any(dup)) {

    if (keep_longest) {
      d[, .hit_id_length := sum(ngram[!.ghost]), by=hit_id]   ## count non ghost terms per hit_id
      pd = data.table::setorderv(d, '.hit_id_length', -1)
      dup_id = pd$hit_id[duplicated(pd[,c('context_id','token_id')]) & !d$.ghost]
      d[, .hit_id_length := NULL]
    } else {
      dup_id = d$hit_id[dup & !d$.ghost]
    }
    d = subset(d, !hit_id %in% unique(dup_id))
  }

  d
}
