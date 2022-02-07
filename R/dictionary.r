


#' Import dictionary from common formats
#'
#' To use a dictionary in this package, it needs to be a data.frame with a 'string' column.
#' Also, if a string in the dictionary is a multitoken string, it needs to be wrapped in
#' quotes ("like this") or angle brackets (<like this>). Otherwise, spaces will be considered
#' as OR operators (see \code{\link{query_syntax}}). This function helps prepare dictionaries as such.
#' It can take either a quanteda dictionary2 class, or any data.frame type dictionary where each row
#' holds a dictionary term.
#'
#' @param dict            A data.frame where each row is a dictionary term or a quanteda dictionary2 class object.
#' @param auto_quote      If TRUE (default), ensures multitoken terms are quoted
#' @param string_alias    If dict is a data.frame and doesn't have a column called "string", check these aliases
#'                        to rename a column to "string". (If your dict doesn't have a column with any of these column names, just specify the name here)
#'
#' @return A dictionary in data.frame form, ready to be used in the dict_ functions.
#' @export
#'
#' @examples
#' d = data.frame(term = c('good','not good'), label=c('positive','negative'))
#' import_dictionary(d)
import_dictionary <- function(dict, auto_quote=T, string_alias = c('query','word','term','text')) {
  if (inherits(dict, 'dictionary2')) dict = melt_quanteda_dict(dict)
  dict = data.table::as.data.table(dict)

  if (!'string' %in% colnames(dict)) {
    alias = which(string_alias %in% colnames(dict))
    if (length(alias) == 0) stop('dictionary does not have a "string" column. Please specify the name of the column with the dictionary string/query in string_alias')
    data.table::setnames(dict, string_alias[alias[1]], 'string')
  }

  if (auto_quote) dict$string = as_multitoken_string(dict$string)

  dict
}

as_multitoken_string <- function(string) {
  terms = stringi::stri_split_boundaries(gsub('\\?|\\*', '', string), type='word')
  multiterm = sapply(terms, length) > 1
  quoted = stringi::stri_detect(string, regex='^[<"]') & stringi::stri_detect(string, regex='[>"]$')

  ifelse(multiterm & !quoted, paste0('<', string, '>'), string)
}

#' Convert a quanteda dictionary to a long data.table format
#'
#' This is used internally in the query_ functions, but can be used manually
#' for more control. For example, adding numeric scores for sentiment dictionaries, and specifying which label/code to use in search_dictionary().
#'
#' @param dict      The quanteda dictionary
#' @param column    The name of the column with the label/code. If dictionary contains multiple levels,
#'                  additional columns are added with the suffix _l[i], where [i] is the level.
#' @param .index    Do not use (used internally)
#'
#' @return A data.table
#' @export
#'
#' @examples
#' \donttest{
#' d = quanteda::data_dictionary_LSD2015
#' melt_quanteda_dict(d)
#' }
melt_quanteda_dict <- function(dict, column='label', .index=NULL) {
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
