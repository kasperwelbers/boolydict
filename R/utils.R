fast_dummy_factor <- function(x) {
  x = as.integer(x)
  nlevels = length(stats::na.omit(unique(x)))
  attr(x, 'levels') = if (nlevels > 0) as.character(1:nlevels) else character()
  class(x) <- 'factor'
  x
}

fast_factor <- function(x, levels=NULL) {
  if (!is.factor(x)) {
    if (!all(is.na(x))) {
      if (is.null(levels)) levels = vector('character', 0)
      x = fast_factor_cpp(as.character(x), as.character(levels))
    } else {
      x = fast_dummy_factor(x)
    }
  } else {
    if (length(levels) > 0) levels(x) = levels
  }
  x
}

global_id <- function(group, i, window=NA) {
  ## given local indices per group, make them globally unique
  ## has to be sorted on order(group, i)
  if (!length(unique(group)) == 1) {
    newgroup = which(!duplicated(group)) # where does a new group start

    group.max = i[newgroup-1] # the highest value of each group
    if (!is.na(window)) group.max = group.max + window # increase the highest value of each group with max_window_size to make sure windows of different groups do not overlap.
    add_scores = cumsum(c(0,group.max)) # the amount that should be added to the i at the start of each group

    repeat_add = c(newgroup[-1], length(i)+1) - newgroup # the number of times the add scores need to be repeated to match the i vector
    i + rep(add_scores, repeat_add)
  } else {
    i
  }
}
