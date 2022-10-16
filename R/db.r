db_create_index <- function(d, db, text_fields, position=NULL, context=NULL, overwrite=FALSE) {
  if (file.exists(db)) {
    if (overwrite) file.remove(db) else stop('DB already exists. Use overwrite if you want to overwrite it')
  }

  conn = DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbWriteTable(conn, 'DATA', d, overwrite=T)

  if (!is.null(position)) {
    if (!position %in% colnames(d)) stop(sprintf('There is no column named %s in d', position))
    position = d[[position]]
  }
  if (!is.null(context)) {
    if (!context %in% colnames(d)) stop(sprintf('There is no column named %s in d', context))
    context = d[[context]]
  }
  text_field =
    for (text_field in text_fields) {
      if (!text_field %in% colnames(d)) stop(sprintf('There is no column named %s in d', text_field))
      fi = prepare_db_index(d[[text_field]], position, context)
      db_write_index(conn, paste0('FEATURE__', text_field), fi)
    }
  conn
}


db_search <- function(conn, feature, terms, term_ids=1:length(terms), use_wildcards=TRUE, case_sensitive=FALSE, unique_hits=FALSE, hit_id_offset=0) {

  if (case_sensitive) {
    voc = DBI::dbGetQuery(conn, sprintf("SELECT * FROM FEATURE__%s__VOC", feature))
  } else {
    terms = tolower(terms)
    voc = DBI::dbGetQuery(conn, sprintf("SELECT * FROM FEATURE__%s__CI_VOC", feature))
  }
  queries = prepare_db_queries(voc, terms, use_wildcards)

  hits = db_index_lookup(conn, paste0('FEATURE__', feature), queries, case_sensitive)
  hits = unpack_hits(hits, unique_hits=unique_hits)

  ## retrieve return_i (the original positions before splitting)
  i_query = paste(hits$i, collapse=',')
  return_i_df = DBI::dbGetQuery(conn, sprintf('SELECT i, return_i FROM FEATURE__%s WHERE i in (%s)', feature, i_query))
  hits = data.table::merge.data.table(hits, return_i_df, by='i')

  data.table::data.table(hit_id = hits$hit_id + hit_id_offset,
                         dict_i = term_ids[hits$query],
                         feat_i = hits$i,
                         orig_feat_i=hits$return_i)
}

unpack_hits <- function(hits, unique_hits=FALSE) {
  ## every row is a hit, with a start_i and i.
  ## here we unpack them so for start_i:i
  hits = data.table::as.data.table(hits)
  hits$n_features = 1 + hits$i - hits$start_i
  if (unique_hits) {
    ## if unique hits, take longest hits
    data.table::setorderv(hits, 'n_features', -1)
    hits = hits[!duplicated(hits$i),]
  }
  hits$hit_id = 1:nrow(hits)
  hits = hits[rep(1:nrow(hits), hits$n_features),]
  hits[, i := start_i - 1 + 1:length(i), by='hit_id']
  hits[, start_i := NULL]
  hits
}


db_write_index <- function(conn, name, fi) {
  ## separately store vocabulary (factor levels) and term ids (factor integers)
  ## also separate case sensitive and case insensitive
  levels(fi$feature) = stringi::stri_trim(stringi::stri_enc_toutf8(levels(fi$feature)))
  fi$ci_feature = fi$feature
  levels(fi$ci_feature) = tolower(levels(fi$ci_feature))

  voc = data.table::data.table(feature=levels(fi$feature))
  voc$feature_id = 1:nrow(voc)
  ci_voc = data.table::data.table(feature=levels(fi$ci_feature))
  ci_voc$feature_id = 1:nrow(ci_voc)

  fi$feature = as.numeric(fi$feature)
  fi$ci_feature = as.numeric(fi$ci_feature)

  DBI::dbWriteTable(conn, name, fi, overwrite=T)
  DBI::dbWriteTable(conn, paste0(name, '__VOC'), voc, overwrite=T)
  DBI::dbWriteTable(conn, paste0(name, '__CI_VOC'), ci_voc, overwrite=T)
  DBI::dbExecute(conn, sprintf('CREATE INDEX feature_index ON %s (feature)', name))
  DBI::dbExecute(conn, sprintf('CREATE INDEX ci_feature_index ON %s (ci_feature)', name))
  DBI::dbExecute(conn, sprintf('CREATE INDEX position_index ON %s (i, context)', name))
}

prepare_db_index <- function(text, position=NULL, context=NULL) {
  fi = data.table::data.table(
    feature = if (is.factor(text)) text else fast_factor(text),
    i = if(is.null(position)) 1:length(text) else position,
    context = if (is.null(context)) 1:length(text) else context
  )
  data.table::setkeyv(fi, c('context','i'))


  ## this dude here breaks every text into features. This has two applications.
  ## 1. if text is a vector or tokens, it breaks up any conjunctions
  ## 2. if text is a full text, it breaks it into tokens
  ## It's optional because the (less flexible) exact match is much faster
  is_split = is_splittable(fi$feature)
  if (any(is_split)){
    fi = flatten_terms(fi, 'feature', 'i')
    fi$return_i = fi$orig_i
  } else {
    fi$return_i = fi$i
  }

  data.table::setkeyv(fi, c('feature','context','i'))
  fi
}


db_index_lookup <- function(conn, index, queries, case_sensitive) {
  data.table::setkeyv(queries, 'term_i')
  feature_column = if (case_sensitive) 'feature' else 'ci_feature'

  ## FIRST FIND THE FIRST TERM OF THE QUERY
  DBI::dbWriteTable(conn, 'query', queries[list(1)], overwrite=T, temporary=T)
  #DBI::dbExecute(conn, 'CREATE INDEX query_index ON query (term)')

  hits = DBI::dbGetQuery(conn, sprintf('
    SELECT features.context, features.i,  query.query
    FROM %s features, query
    WHERE features.%s = query.term
  ',  index, feature_column))
  hits$start_i = hits$i

  ## IF THERE ARE NO MULTI-WORD QUERIES, WE'RE DONE
  max_query_length = max(queries$term_i)
  if (max_query_length < 2) return(hits)

  ## IF THERE ARE MULTI-TERM QUERIES,
  ## SEE IF THE NEXT POSITION MATCHES THE NEXT QUERY TERM
  complete_match = list()
  for (i in 2:max_query_length) {
    position_lookup = list(i)  ## don't move this inside q[], because q would use it's own i column
    next_term_query = queries[position_lookup]
    complete_match[['']] = hits[!hits$query %in% unique(next_term_query$query),]
    next_position_query = merge.data.table(hits, next_term_query, by='query')

    DBI::dbWriteTable(conn, 'query', next_position_query, overwrite=T, temporary=T)
    #DBI::dbExecute(conn, 'DROP INDEX query_index')
    #DBI::dbExecute(conn, 'CREATE INDEX query_index ON query (term)')

    hits = DBI::dbGetQuery(conn, sprintf('
    SELECT features.context, features.i,  query.query, query.start_i
    FROM %s features, query
    WHERE features.%s = query.term
      AND features.context = query.context
      AND features.i = query.i + 1
  ',  index, feature_column))

  }
  complete_match[['']] = hits

  rbindlist(complete_match)
}

prepare_db_queries <- function(voc, terms, use_wildcards) {
  ## return a dictionary in which each term has been expanded (in case of wildcards)
  ## and terms are converted to the feature_id
  terms = stringi::stri_trim(stringi::stri_enc_toutf8(terms))
  sn = stringi::stri_split(terms, regex=" ")

  if (use_wildcards && any(grepl('[?*]', terms))) {
    sn = expand_wildcards(sn, unique(voc$feature))
    names(sn) = floor(as.numeric(names(sn)))
  } else {
    names(sn) = 1:length(sn)
  }
  if (length(sn) == 0) return(NULL)

  n = as.numeric(sapply(sn, length))
  queries = data.table::data.table(feature = unlist(sn),
                                   term_i = unlist(lapply(n, function(x) 1:x)),
                                   query = rep(1:length(sn), n))
  queries[voc, term := feature_id, on='feature']
  queries[, feature := NULL]

  nomatch = unique(queries$query[is.na(queries$term)])
  queries[!queries$query %in% nomatch,]
  }
