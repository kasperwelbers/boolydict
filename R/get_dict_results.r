## extends dictionary lookup
## prepares data for lucene_like.

#' @import data.table
get_dict_results <- function(tokens, query_terms, text='text', index=NULL, context=NULL, as_ascii=F) {
  hit_id = feat_i = NULL

  ## index and context are columns in tokens. turn into vectors here
  if (!is.null(index) && !index %in% colnames(tokens)) stop(sprintf('The specified index column (%s) doesnt exists', index))
  if (!is.null(context) && !context %in% colnames(tokens)) stop(sprintf('The specified context column (%s) doesnt exists', context))
  index = if (is.null(index)) 1:nrow(tokens) else tokens[[index]]
  context = if (is.null(context)) 1:nrow(tokens) else tokens[[context]]
  if (is.character(context)) context = fast_factor(context)

  features = unique(query_terms$feature)
  dict_results = vector('list', length(features))
  names(dict_results) = features
  for (i in 1:length(features)) {
    if (!features[i] %in% colnames(tokens)) stop(sprintf('The feature "%s" is not a column in tokens', features[i]))


    dict = query_terms[list(features[i]),,on='feature']

    fi = dictionary_lookup(text = tokens[[features[i]]],  dict_string=dict$term, index = index, context=context,
                           mode='features', case_sensitive=dict$case_sensitive, ascii=as_ascii)

    if (!is.null(fi)) if ('token_expr' %in% colnames(query_terms)) fi = token_expression_filter(tc, fi, query_terms)

    if (!is.null(fi)) {
      fi$ghost = dict$ghost[fi$dict_i]
      fi = fi[,list(data_indices=list(unique(orig_feat_i[!ghost])),
                    orig_feat_i=min(orig_feat_i),
                    token_id=min(feat_i),
                    ngram=(1 + max(feat_i) - min(feat_i))),
              by=c('hit_id','dict_i')]
      fi[, hit_id := NULL]
      fi = cbind(dict[fi$dict_i,c('term','case_sensitive')], fi[,c('data_indices','ngram','token_id')], context=context[fi$orig_feat_i])

      fi$context_id = as.numeric(fi$context)
      data.table::setkeyv(fi, c('term','case_sensitive'))
    }
    dict_results[[i]] = fi
  }
  dict_results
}

token_expression_filter <- function(tc, fi, query_terms) {
  rm_fi = rep(F, nrow(fi))
  for (expr in unique(query_terms$token_expr)) {
    if (is.na(expr)) next
    q_has_expr = !is.na(query_terms$token_expr) & query_terms$token_expr == expr
    fi_has_expr = q_has_expr[fi$dict]                                                                                       ## find which features have the expression as a condition
    which_expr_false = tryCatch(tc$tokens[fi$feat_i[fi_has_expr]][!eval(parse(text=expr)),,which=T], error = function(e) e) ## evaluate expression for these features in tc$tokens

    ## special error handling, because this can be confusing
    if (inherits(which_expr_false, 'simpleError'))
      stop(call. = F, paste('A token subset expression used in a query gave an error:', paste0('The expression\n', expr), paste0('The error\n',which_expr_false$message), sep='\n\n'))

    which_expr_false = tc$tokens[fi$feat_i[fi_has_expr]][!eval(parse(text=expr)),,which=T]                                  ## evaluate expression for these features in tc$tokens


    rm_fi[fi_has_expr][which_expr_false] = T                                                     ## take features with expression, and of those set rm_fi features where expression evaluated to false to T (i.e. should be removed)
  }
  if (all(rm_fi)) return(NULL)
  fi[!rm_fi,]
}


