% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dictionary.r
\name{melt_quanteda_dict}
\alias{melt_quanteda_dict}
\title{Convert a quanteda dictionary to a long data.table format}
\usage{
melt_quanteda_dict(dict, column = "label", .index = NULL)
}
\arguments{
\item{dict}{The quanteda dictionary}

\item{column}{The name of the column with the label/code. If dictionary contains multiple levels,
additional columns are added with the suffix _l[i], where [i] is the level.}

\item{.index}{Do not use (used internally)}
}
\value{
A data.table
}
\description{
This is used internally in the query_ functions, but can be used manually
for more control. For example, adding numeric scores for sentiment dictionaries, and specifying which label/code to use in search_dictionary().
}
\examples{
\donttest{
d = quanteda::data_dictionary_LSD2015
melt_quanteda_dict(d)
}
}
