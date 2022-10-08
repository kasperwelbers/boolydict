library(boolydict)
library(data.table)
d = lapply(1:100, function(x) sotu_texts)
d = data.table::rbindlist(d)
d$id = 1:nrow(d)


microbenchmark(
  dict_filter(d, "war AND iraq", cache=NULL),
  dict_filter(d, "war AND iraq", cache='test.cache'),
  dict_filter(d, "war AND iraq", cache=1024),
  times=1
)

microbenchmark(
  dict_filter(d, "war AND iraq", cache=1024),
  times=1
)

nrow(df$fi)



index = prepare_index(d$text, NULL, NULL, F)
## store 'flatten' in db_table_name

library(DBI)
db <- DBI::dbConnect(RSQLite::SQLite(), "test.sqllite", overwrite=T)
dbWriteTable(db, 'indexed', index$fi, overwrite=T)
dbWriteTable(db, 'raw', index$fi, overwrite=T)

DBI::dbExecute(db, 'CREATE INDEX feature_index ON indexed (feature)')

terms = c('the','president')
termstring = paste(paste0("'",terms,"'"), collapse=',')

f1 = function() {
res = DBI::dbSendQuery(db, sprintf("SELECT * FROM indexed WHERE feature IN (%s)", termstring))
DBI::dbFetch(res)
}
f2 = function() {
res = DBI::dbSendQuery(db, sprintf("SELECT * FROM raw WHERE feature IN (%s)", termstring))
DBI::dbFetch(res)
}

nrow(f1())


getOption('boolydict_cache')

f1()

column = 'text'
create_index('testdb', column, d[[column]])


f <- function() {

}


saveRDS(df$fi, 'test.rds', compress = T)
x = readRDS('test.rds')

microbenchmark(
fwrite(df$fi, 'test.csv'),
saveRDS(df$fi, 'test.rds', compress = F)
, times=1)

?fread()

db_write_index('test.db', 'test', df$fi)

create_index <- function(db, name, text, index=NULL, context=NULL, exact=FALSE) {
  fi = prepare_index(text, index, context, exact)
  db_write_index(db, name, fi)
}

db_write_index <- function(db, name, fi) {
  ## separately store vocabulary (factor levels) and term ids (factor integers)
  voc = data.table::data.table(feature=levels(fi$feature))
  voc$feature_id = 1:nrow(voc)
  voc$term = stringi::stri_trim(stringi::stri_enc_toutf8(voc$term))
  ## note that the voc row id will match the factor indices in fi
  fi$feature = as.numeric(fi$feature)

  conn = DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbWriteTable(conn, name, fi, overwrite=T)
  DBI::dbWriteTable(conn, paste0(name, '__VOC'), voc, overwrite=T)
  DBI::dbExecute(conn, sprintf('CREATE INDEX feature_index ON %s (feature)', name))
  DBI::dbDisconnect(conn)
}

db_search <- function(db, table, terms, case_sensitive) {
  conn = DBI::dbConnect(RSQLite::SQLite(), 'testdb')
  res = DBI::dbSendQuery(conn, sprintf("SELECT * FROM %s__VOC WHERE term IN (%s)", column, termstring))
  terms = DBI::dbFetch(res)
  res = DBI::dbSendQuery(conn, sprintf("SELECT * FROM %s WHERE feature IN (%s)", terms$term))
  terms = DBI::dbFetch(res)

}

prepare_index <- function(text, index=NULL, context=NULL, exact=FALSE) {
  fi = data.table::data.table(
    feature = if (is.factor(text)) text else fast_factor(text),
    token_id = if(is.null(index)) 1:length(text) else index,
    i = 1:length(text),
    context = if (is.null(context)) 1:length(text) else context
  )
  data.table::setkeyv(fi, c('context','token_id'))

  if (!exact) {
    ## this dude here breaks every text into tokens. This has two applications.
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
  }
  fi
}


?fread
data.table::fread()
