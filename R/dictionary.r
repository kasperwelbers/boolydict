


as_dictionary <- function(dict) {
  if (inherits(dict, 'dictionary2')) dict = melt_quanteda_dict(dict)
  dict = data.table::as.data.table(d)
  if ('word' %in% colnames(dict)) data.table::setnames(dict, 'test', 'nee')
}


#' Convert a quanteda dictionary to a long data.table format
#'
#' This is used internally in the query_ functions, but can be used manually
#' for more control. For example, adding numeric scores for sentiment dictionaries, and specifying which label/code to use in search_dictionary().
#'
#' @param dict      The quanteda dictionary
#' @param column    The name of the column with the label/code. If dictionary contains multiple levels,
#'                  additional columns are added with the suffix _l[i], where [i] is the level.
#' @param .index    Do not use (used for recursive melting)
#'
#' @return A data.table
#' @export
#'
#' @examples
#' \donttest{
#' d = quanteda::data_dictionary_LSD2015
#' melt_quanteda_dict(d)
#' }
melt_quanteda_dict <- function(dict, column='code', .index=NULL) {
  if (is.null(.index)) {
    if (!inherits(dict, 'dictionary2')) stop('dict is not a quanteda dictionary2 class')
    .index = data.table::data.table(string = character(length(dict)))
  }
  cname = if (ncol(.index) > 1) paste0(column, '_l', ncol(.index)) else column
  .index[[cname]] = names(dict)

  n = sapply(dict, length)
  .index = .index[rep(1:nrow(.index), n)]
  dict = unlist(dict, recursive = F, use.names = T)
  names(dict) = gsub('.*\\.', '', names(dict))


  if (!any(sapply(dict, class) == 'list')) {
    if (length(unlist(dict)) > nrow(.index)) {
      n = sapply(dict, length)
      .index = .index[rep(1:nrow(.index), n)]
    }
    .index$string = unlist(dict)
    return(.index)
  }

  melt_quanteda_dict(dict, column, .index)
}
