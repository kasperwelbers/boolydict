

split_tokens <- function(x, max_tokens, remember_spaces=F) {
  x = stringi::stri_split_boundaries(x, type='word')

  if (remember_spaces) {
    x = lapply(x, function(x) collapse_terms_cpp(x, collapse=stringi::stri_detect(x, regex='^\\s+$'), sep=" ", sep2=""))
  } else {
    x = lapply(x, function(x) x[!stringi::stri_detect(x, regex='^\\s+$')])
  }
  if (!is.null(max_tokens)) x = sapply(x, utils::head, max_tokens, simplify=F)
  x
}

