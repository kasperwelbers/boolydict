#' Dictionary lookup
#'
#' Similar to search_features, but for fast matching of large dictionaries.
#'
#' @param tc              A tCorpus
#' @param dict            A dictionary. Can be either a data.frame or a quanteda dictionary. If a data.frame is given, it has to
#'                        have a column named "string"  (or use string_col argument) that contains the dictionary terms, and a column "code" (or use code_col argument) that contains the
#'                        label/code represented by this string. Each row has a single string, that can be
#'                        a single word or a sequence of words seperated by a whitespace (e.g., "not bad"), and can have the common ? and * wildcards.
#'                        If a quanteda dictionary is given, it is automatically converted to this type of data.frame with the
#'                        \code{\link{melt_quanteda_dict}} function. This can be done manually for more control over labels.
#' @param token_col       The feature in tc that contains the token text.
#' @param string_col      If dict is a data.frame, the name of the column in dict with the dictionary lookup string. Default is "string"
#' @param code_col        The name of the column in dict with the dictionary code/label. Default is "code".
#'                        If dict is a quanteda dictionary with multiple levels, "code_l2", "code_l3", etc. can be used to select levels..
#' @param sep             A regular expression for separating multi-word lookup strings (default is " ", which is what quanteda dictionaries use).
#'                        For example, if the dictionary contains "Barack Obama", sep should be " " so that it matches the consequtive tokens "Barack" and "Obama".
#'                        In some dictionaries, however, it might say "Barack+Obama", so in that case sep = '\\+' should be used.
#' @param mode            There are two modes: "unique_hits" and "features". The "unique_hits" mode prioritizes finding unique matches, which is recommended for counting how often a dictionary term occurs.
#'                        If a term matches multiple dictionary terms (which should only happen for nested multi-word terms, such as "bad" and "not bad"), the longest term is always used.
#'                        The features mode does not delete duplicates.
#' @param case_sensitive  logical, should lookup be case sensitive?
#' @param use_wildcards   Use the wildcards * (any number including none of any character) and ? (one or none of any character). If FALSE, exact string matching is used
#' @param ascii           If true, convert text to ascii before matching
#' @param verbose         If true, report progress
#'
#' @return A vector with the id value (taken from dict$id) for each row in tc$tokens
#' @export
#'
#' @examples
#' dict = data.frame(string = c('this is', 'for a', 'not big enough'), code=c('a','c','b'))
#' tc = create_tcorpus(c('this is a test','This town is not big enough for a test'))
#' search_dictionary(tc, dict)$hits
search_dictionary <- function(tc, dict, token_col='token', string_col='string', code_col='code', sep=' ', mode = c('unique_hits','features'), case_sensitive=F, use_wildcards=T, ascii=F, verbose=F){
  hit_id = NULL
  mode = match.arg(mode)

  if (!is_tcorpus(tc)) stop('tc is not a tCorpus')
  if (inherits(dict, 'dictionary2')) dict = melt_quanteda_dict(dict)
  if (!inherits(dict, 'data.frame')) stop('dict has to be a data.frame or a quanteda dictionary2 class')
  if (!string_col %in% colnames(dict)) stop(sprintf('dict does not have a column named "%s"', string_col))
  if (!code_col %in% colnames(dict)) stop(sprintf('dict does not have a column named "%s"', code_col))

  fi = dictionary_lookup(tc, data.table::data.table(string=dict[[string_col]], id = 1:nrow(dict)), regex_sep=sep, mode=mode,
                         token_col=token_col, case_sensitive=case_sensitive, standardize=T, ascii=ascii, use_wildcards=use_wildcards, verbose=verbose)
  if (is.null(fi)) return(featureHits(NULL, data.frame()))

  hits = tc$tokens[fi$feat_i,]
  hits$hit_id = fi$hit_id
  hits$code = dict[[code_col]][as.numeric(fi$dict_i)]
  if (!'sentence' %in% colnames(hits)) hits[, 'sentence' := numeric()]
  hits = subset(hits, select = intersect(c('doc_id','token_id','sentence','code','hit_id',token_col), colnames(hits)))
  data.table::setnames(hits, token_col, 'feature')

  queries = data.frame()
  featureHits(hits, queries)
}
