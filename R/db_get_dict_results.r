function() {
  ## seems the only difference is that db_get_dict_results doesn't return 'context',
  ## which requires loading the full DATA
  ## get_dict_results also still has a part that relies on the tcorpus, which we might have to just take out

  ## db usecase
  ## make it so that it can be used completely standalone. Including the full data

  # test
  d = lapply(1:10, function(x) sotu_texts)
  d = data.table::rbindlist(d)
  d$id = 1:nrow(d)

  queries = parse_queries(c("this OR that",
                            "<united states> OR America~s"), feature = 'text')

  conn = DBI::dbConnect(RSQLite::SQLite(), ':memory:')
  conn@dbname

  x = db_get_dict_results(d, queries$lookup_terms, text='text')

  x = get_dict_results(d, queries$lookup_terms, text='text')
  y = db_get_dict_results(d, queries$lookup_terms, text='text')

  library(microbenchmark)
  microbenchmark(
    get_dict_results(d, queries$lookup_terms, text='text', cache=1000),
    db_get_dict_results(d, queries$lookup_terms, text='text')
  , times=1)

}

db_get_dict_results <- function(df, query_terms, text='text', index=NULL, context=NULL, use_wildcards=TRUE) {
  hit_id = feat_i = NULL

  db = ':memory:'
  features = unique(query_terms$feature)
  conn = db_create_index(d, db, text_fields = features, position = index, context = context, overwrite=T)
  on.exit(DBI::dbDisconnect(conn))

  dict_results = vector('list', length(features))
  names(dict_results) = features
  for (i in 1:length(features)) {
    dict = query_terms[list(features[i]),,on='feature']
    fi = db_dictionary_lookup(conn=conn,
                             feature=features[i],
                             dict_string=dict$term,
                             index = index,
                             context=context,
                             mode='features',
                             case_sensitive=dict$case_sensitive,
                             use_wildcards=use_wildcards)

    if (!is.null(fi)) {
      fi$ghost = dict$ghost[fi$dict_i]
      fi = fi[,list(data_indices=list(unique(orig_feat_i[!ghost])),
                    orig_feat_i=min(orig_feat_i),
                    token_id=min(feat_i),
                    ngram=(1 + max(feat_i) - min(feat_i))),
              by=c('hit_id','dict_i')]
      fi[, hit_id := NULL]

      context =
      fi = cbind(dict[fi$dict_i,c('term','case_sensitive')], fi[,c('data_indices','ngram','token_id')], context=context[fi$orig_feat_i])

      fi$context_id = as.numeric(fi$context)
      data.table::setkeyv(fi, c('term','case_sensitive'))
    }
    dict_results[[i]] = fi
  }

  dict_results
}

db_dictionary_lookup <- function(conn, feature, dict_string, index=NULL, context=NULL, sep=' ', mode = c('unique_hits','features'), case_sensitive=F, use_wildcards=T, cache=NULL){
  ## prepare and validate df
  mode = match.arg(mode)

  ## prepare dict string
  if (sep != ' ') dict_string = gsub(sep, ' ', dict_string)
  dict_string = stringi::stri_trim(dict_string)

  dict_string = standardize_dict_term_spacing(dict_string, use_wildcards)

  ## perform lookup
  if (any(case_sensitive) && !all(case_sensitive)) {
    if (length(case_sensitive) != length(dict_string)) stop('case_sensitive vector needs to be length 1 or length of dictionary')
    out1 = db_search(conn, feature, dict_string[case_sensitive], term_ids=which(case_sensitive),
                     use_wildcards=use_wildcards, case_sensitive=T, unique_hits=mode=='unique')
    out2 = db_search(conn, feature, dict_string[!case_sensitive], term_ids=which(!case_sensitive),
                     use_wildcards=use_wildcards, case_sensitive=F, unique_hits=mode=='unique', hit_id_offset=max(out1$hit_id))
    out = rbind(out1,out2)
  } else {
    out = db_search(conn, feature, dict_string, use_wildcards=use_wildcards, case_sensitive=unique(case_sensitive), unique_hits=mode == 'unique')
  }

  out
}
