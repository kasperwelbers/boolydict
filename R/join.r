#' Join two tables based on a character column of a dictionary or Lucene-like
#' Boolean queries
#'
#' Essentially dplyr's join operations (see \link[dplyr]{join}) but with
#' additional query features.
#'
#' @param x,y A pair of data frames, data frame extensions (e.g. a tibble)
#' @param by A character vector of variables to join by. To join on different
#'   variables between `x` and `y`, use a named vector. For example, `by = c("a"
#'   = "b")` will match `x$a` to `y$b`.
#' @param mode One of "left", "right" or "full".
#' @param ... Handed to \link{search_query}.
#'
#' @return A data.table or tibble
#' @export
#'
#' @examples
#' df = data.frame(
#'   text = c('This is just a simple example', 'Simple is good'),
#'   doc_id = c(1, 2)
#' )
#' dict_join(df, dict, by = c("text" = "string"))
#' dict_left_join(df, dict, by = c("text" = "string"))
#' dict_right_join(df, dict, by = c("text" = "string"))
#' dict_full_join(df, dict, by = c("text" = "string"))
dict_join <- function(x, y, by, mode = "left", ...) {

  # restore to tibble if this is the input
  tbl <- tibble::is_tibble(x)

  if (is.null(names(by))) {
    names(by) <- by
  }

  x <- tibble::add_column(x, .x = seq_len(nrow(x)))
  y <- tibble::add_column(y, .y = seq_len(nrow(y)))

  matches <- search_query(
    df = x,
    queries = y[[by]],
    text_col = names(by)
  )[, !"hit_id"]
  data.table::setnames(matches, c("data_index", "query_index"), c(".x", ".y"))

  if (mode == "left") {

    matches <- merge(x, matches, by = ".x", all.x = TRUE)
    matches <- merge(matches, y, by = ".y", all.x = TRUE)

  } else if (mode == "right") {

    matches <- merge(x, matches, by = ".x", all.y = TRUE)
    matches <- merge(matches, y, by = ".y", all.x = TRUE)

  } else if (mode == "full") {

    matches <- merge(matches, y, by = ".y", all.y = TRUE)
    matches <- merge(x, matches, by = ".x", all = TRUE)

  }

  data.table::setorder(matches, cols = ".x", na.last = TRUE)
  matches[, c(".x", ".y")] <- NULL

  if (tbl) {
    matches <- tibble::as_tibble(matches)
  }
  return(matches)
}

#' @rdname dict_join
#' @export
dict_left_join <- function(x, y, by, ...) {
  dict_join(x, y, by, mode = "left", ...)
}

#' @rdname dict_join
#' @export
dict_right_join <- function(x, y, by, ...) {
  dict_join(x, y, by, mode = "right", ...)
}

#' @rdname dict_join
#' @export
dict_full_join <- function(x, y, by, ...) {
  dict_join(x, y, by, mode = "full", ...)
}
