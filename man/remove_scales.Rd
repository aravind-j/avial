% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remove_scales.R
\name{remove_scales}
\alias{remove_scales}
\title{Remove scales from \code{ggplot} objects}
\usage{
remove_scales(g, scales)
}
\arguments{
\item{g}{A \code{ggplot} object.}

\item{scales}{The scales to be removed as a character vector.}
}
\value{
A \code{ggplot} object without the scales specified.
}
\description{
Useful for avoiding the warning \code{Scale for * is already present. Adding
another scale for *, which will replace the existing scale.}
}
