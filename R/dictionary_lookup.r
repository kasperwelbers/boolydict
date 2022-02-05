

## returns a data.table with columns hit_id (for multiword matches), dict_i (index of matched dict_string) and feat_i (index of the text/feature)
dictionary_lookup <- function(text, dict_string, index=NULL, context=NULL, regex_sep=' ', mode = c('unique_hits','features'), case_sensitive=F, exact=F, ascii=F, use_wildcards=T){
  ## prepare and validate tokens
  mode = match.arg(mode)

  ## create feature index
  fi = data.table::data.table(
    feature = if (is.factor(text)) text else fast_factor(text),
    token_id = if(is.null(index)) 1:length(text) else index,
    i = 1:length(text),
    context = if (is.null(context)) 1:length(text) else context
  )
  data.table::setkeyv(fi, c('context','token_id'))

  flatten = F
  if (!exact) {
    ## this dude here breaks every text and dict term into tokens. This has two applications.
    ## 1. if text is a vector or tokens, it breaks up any conjunctions
    ## 2. if text is a full text, it breaks it into tokens
    ## The only reason it's optional is that we might want to allow fast but less flexible exact match
    dict_string = standardize_dict_term_spacing(dict_string, use_wildcards)
    is_split = is_splittable(fi$feature)
    if (any(is_split)){
      fi = flatten_terms(fi, 'feature', 'i')
      flatten = T ## remember if splitting was necessary, to 'undo' it later
    }
  }

  ## perform lookup
  if (any(case_sensitive) && !all(case_sensitive)) {
    if (length(case_sensitive) != length(dict_string)) stop('case_sensitive vector needs to be length 1 or length of dictionary')
    out1 = dictionary_lookup_tokens(fi, dict_string[case_sensitive], dict_i_ids = which(case_sensitive), mode=mode, case_sensitive=T, ascii, regex_sep, use_wildcards, flatten, 1)
    out2 = dictionary_lookup_tokens(fi, dict_string[!case_sensitive], dict_i_ids = which(!case_sensitive), mode=mode, case_sensitive=F, ascii, regex_sep, use_wildcards, flatten, max(out1$hit_id)+1)
    out = rbind(out1,out2)
  } else {
    out = dictionary_lookup_tokens(fi, dict_string, dict_i_ids = 1:length(dict_string), mode=mode, unique(case_sensitive), ascii, regex_sep, use_wildcards, flatten, 1)
  }

  ## in case a single asterisk wildcard was used, the lookup was skipped, and we'll just add everything.
  ## this is super expensive, so should think of better solution
  is_ast = which(dict_string == '*')
  if (any(is_ast) && use_wildcards) {
    hit_id_offset = max(out$hit_id)+1
    ast_out = data.table::data.table(hit_id = 1:nrow(fi) + hit_id_offset, dict_i = is_ast, feat_i = fi$i, orig_feat_i = if (flatten) fi$orig_i else fi$i)
    out = rbind(out, ast_out)
  }

  out
}


dictionary_lookup_tokens <- function(fi, dict_string, dict_i_ids, mode, case_sensitive, ascii, regex_sep, use_wildcards, flatten, hit_id_offset=1) {
  levels(fi$feature) = normalize_string(levels(fi$feature), lowercase=!case_sensitive, ascii = ascii)
  #data.table::setkey(fi, 'feature')

  d = collapse_dict(dict_string, regex_sep, use_wildcards, case_sensitive, ascii, levels(fi$feature))
  if (!'terms' %in% names(d)) return(NULL)

  data.table::setindexv(fi, 'feature')
  first_terms = levels(fi$feature)[d$terms_i]
  initial_i = fi[list(feature=first_terms), on='feature', which=T, nomatch=0]
  initial_i = sort(unique(initial_i))


  out = do_code_dictionary(as.numeric(fi$feature), context = fi$context, token_id = fi$token_id, which = initial_i, dict = d, hit_id_offset=hit_id_offset, verbose=F)
  if (is.null(out) || nrow(out) == 0) return(NULL)
  out$dict_i = dict_i_ids[out$dict_i]

  if (flatten) {
    out$orig_i = fi$orig_i[out$feat_i]
  } else {
    out$orig_i = out$feat_i
  }

  if (mode == 'unique_hits') {
    data.table::setorderv(out, 'nterms', -1)
    out = out[!duplicated(out$feat_i),]
  }

  data.table::data.table(hit_id=out$hit_id, dict_i=out$dict_i, feat_i=out$feat_i, orig_feat_i=out$orig_i)
}

normalize_string <- function(x, lowercase=T, ascii=T, trim=T){
  if (lowercase) x = tolower(x)
  if (ascii) x = iconv(x, to='ASCII//TRANSLIT')
  if (trim) x = stringi::stri_trim(stringi::stri_enc_toutf8(x))
  x
}

collapse_dict <- function(string, regex_sep, use_wildcards, case_sensitive, ascii, feature_levels) {
  dict = data.table::data.table(string = normalize_string(string, lowercase=!case_sensitive, ascii=ascii))

  ## remove separator if at start or end of word
  first_or_last = paste0('^',regex_sep, '|', regex_sep, '$')
  dict$string = gsub(first_or_last, '', dict$string)

  sn = stringi::stri_split(dict$string, regex=regex_sep)

  if (use_wildcards && any(grepl('[?*]', dict$string))) {
    sn = expand_wildcards(sn, feature_levels)
    names(sn) = floor(as.numeric(names(sn)))
  } else {
    names(sn) = 1:length(sn)
  }

  if (length(sn) == 0) return(NULL)

  ## for binary search in c++, there are issues with different ordering of terms in R and c++ (and more genrally encoding issues)
  ## here we replace all terms in the dictionary with factor levels
  sn = replace_string_with_factor(sn, feature_levels)

  if (length(sn) == 0) return(NULL)
  rec_collapse_dict(sn, regex_sep=regex_sep)
}

rec_collapse_dict <- function(l, i=1, regex_sep=' ') {
  out = list()

  has_terms = !is.na(sapply(l, '[', j=i))
  if (any(!has_terms)) {
    out$code = as.numeric(names(l)[which(!has_terms)])
    if (all(!has_terms)) return(out)
    l = l[has_terms]
  }

  term = sapply(l, '[', j=i, simplify = T)
  terms = split(l, term)

  out$terms = sapply(terms, rec_collapse_dict, i=i+1, regex_sep=regex_sep, USE.NAMES = F, simplify=F)
  if (length(out$terms) == 0) {
    out$terms = NULL
  } else {
    out$terms_i = as.numeric(names(out$terms))
    names(out$terms) = NULL
  }
  out
}

replace_string_with_factor <- function(query_list, l) {
  ln = names(query_list)
  ## query_list is the list with split dictionary terms
  ## l is the levels of the features
  n = sapply(query_list, length)

  i = rep(1:length(query_list), n)

  ql = data.table::data.table(t = unlist(query_list), i = i)
  ql$t = as.numeric(factor(ql$t, levels=l))
  new = split(ql$t, ql$i)
  names(new) = ln
  new[!sapply(new, anyNA, simplify = T)]
}

expand_wildcards <- function(query_list, voc) {
  ## get a new list where terms with wildcards are repeated for all matches in vocabulary
  ## the names of the list contain ids of which the floor is the index of the dictionary
  n = sapply(query_list, length)
  i = rep(1:length(query_list), n)
  ql = data.table::data.table(t = unlist(query_list), i = i)
  add_n <- function(x) (1:length(x)) + 0  ## (suspected altrep issues)
  ql[, n := add_n(t), by='i']

  ql$is_wc =  grepl('[?*]', ql$t)
  if (!any(ql$is_wc)) {
    names(query_list) = 1:length(query_list)
    return(query_list)
  }
  wct = unique(ql$t[ql$is_wc])
  wctreg = gsub('([^a-zA-Z0-9\\*\\?])', '\\\\\\1', wct)

  ## find more elegant solution for not matching escaped * and ?
  wctreg = gsub('\\\\\\*', '##ASTER##', wctreg)
  wctreg = gsub('\\\\\\?', '##QUEST##', wctreg)

  wctreg = gsub('\\?+', '?', wctreg)
  wctreg = gsub('\\*+', '*', wctreg)
  justast = wctreg == '*'
  if (any(justast)) {
    #warning('Some terms are only an asterisk wildcard, and so could be anything. These are ignored')
    wctreg[justast] = '###IGNORE###'
  }

  wctreg = gsub('\\*', '.*', wctreg)
  wctreg = gsub('\\?', '.{0,1}', wctreg)
  wctreg = gsub('##ASTER##', '\\*', wctreg, fixed=T)
  wctreg = gsub('##QUEST##', '\\?', wctreg, fixed=T)

  ## old approach (just perform regex on all terms)
  #wctreg = paste0('\\b',wctreg,'\\b')
  #full_t = sapply(wctreg, grep, x=voc, value=T, simplify = F)

  ## new (faster) approach (possible due to the standardize step now implemented in dictionary_lookup)
  ## seems to give same results. Only exception is that it really relies on what is split by split_tokens (which might be a good thing)
  ## For instance, "stupid.dot" would before match "dot" because \\b considered the middel dot as a word boundary.
  ## now it doesn't because split_tokens (based on stringi split boundaries) doesn't consider this as two separate tokens
  wctreg = paste0('^',wctreg,'$')
  full_t = fast_wildcard_voc_match(wctreg, voc, n_bin_search = 3)

  nreg = sapply(full_t, length)

  if (sum(nreg) > 0) nr = (1:sum(nreg)) + 0 else nr = numeric()
  full_t = data.table(t = rep(wct, nreg),
                      full_t = unlist(full_t),
                      nr = nr)

  full_t = merge(full_t, ql[,c('i','t')], by='t', allow.cartesian=T)
  out = merge(full_t, ql, by='i', all=T, allow.cartesian = T)

  out$nr[is.na(out$nr)] = 0
  data.table::setorderv(out, 'n', 1)
  out$id = out$i + (out$nr / (max(out$nr)+1))
  out = split(ifelse(out$is_wc, out$full_t, out$t.y), out$id)
  has_na = sapply(out, anyNA)
  out[!has_na]
}

fast_wildcard_voc_match <- function(reg, voc, n_bin_search=3) {
  ## create an index for every term in vocabulary where key is the separate columns for the first n_bin_search characters
  ## these enable binary search on first [n_bin_search] terms of the fixed part of a regex
  voc_index = data.table::data.table(voc=voc, n=nchar(voc))
  for (i in 1:n_bin_search) voc_index[,(paste0('voc',i)) := substr(voc, i,i)]
  data.table::setkeyv(voc_index, paste0('voc', 1:n_bin_search))

  ## get the 'fixed' part of a regex term (only before a wildcard)
  fixedpart = gsub('\\\\b','',reg)
  fixedpart = gsub('^\\^|\\$$', '', fixedpart)
  fixedpart = gsub('\\.[{*].*', '', fixedpart)
  fixedpart = gsub('\\\\','', fixedpart)
  n = nchar(fixedpart)

  ## for every term create a list of the first [n_bin_search] terms from the fixed part
  bin_search_part = substr(fixedpart, 1, n_bin_search)
  qlists = stringi::stri_split_boundaries(bin_search_part, type='character')

  ## use multithreading  (DISABLED. IT WORKS, BUT SOMEHOW BREAKS THE DEBIAN CRAN CHECKS)
  #cl = use_n_cores()
  #if (.Platform$OS.type %in% c("windows")) {
  #  cl = parallel::makeCluster(cl)
  #  on.exit(parallel::stopCluster(cl))
  #}

  #pbapply::pboptions(type='none')
  #full_t = pbapply::pbsapply(1:length(reg), cl=cl, FUN=function(i) {
  #  qlist = as.list(qlists[[i]])             ## get first chars. transform to list for use in data.table search
  #  subvoc = voc_index
  #  if (length(qlist) > 0)
  #    subvoc = subvoc[qlist, nomatch=0]      ## first filter voc with binary search on first part
  #  subvoc = subvoc$voc[subvoc$n >= n[i]]    ## also ignore voc terms that are shorter than fixed part of regex
  #  if (length(subvoc) > 0)
  #    subvoc[stringi::stri_detect(subvoc, regex = reg[i])]
  #  else
  #    character()
  #}, simplify = F)

  full_t = sapply(1:length(reg), FUN=function(i) {
    qlist = as.list(qlists[[i]])             ## get first chars. transform to list for use in data.table search
    subvoc = voc_index
    if (length(qlist) > 0)
      subvoc = subvoc[qlist, nomatch=0]      ## first filter voc with binary search on first part
    subvoc = subvoc$voc[subvoc$n >= n[i]]    ## also ignore voc terms that are shorter than fixed part of regex
    if (length(subvoc) > 0)
      subvoc[stringi::stri_detect(subvoc, regex = reg[i])]
    else
      character()
  }, simplify = F)

  full_t[sapply(full_t, length) > 0]
  names(full_t) = reg
  full_t
}


code_from_features <- function(hits, collapse_sep='_') {
  feature = NULL; hit_id = NULL; group = NULL; code = NULL
  code = hits[, list(.new_code = paste(feature, collapse=collapse_sep)), by=c('hit_id','code')]
  code = code[, list(N = length(hit_id)), by=c('code','.new_code')]
  data.table::setorderv(code, 'N', order = -1)
  code = unique(code, by='code')
  hits = merge(hits, code[,c('code','.new_code')], by='code')
  hits$code = hits$.new_code
  hits
}
