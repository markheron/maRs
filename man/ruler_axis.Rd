% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_helpers.R
\name{ruler_axis}
\alias{ruler_axis}
\title{ruler style axis}
\usage{
ruler_axis(side = 1, axis_range_to_zero_one = NULL, data = NULL,
  lim = NULL, ...)
}
\arguments{
\item{side}{to add the axis to}

\item{axis_range_to_zero_one}{a range for which the axis labels should be scaled down to a range of \code{c(0,1)}. Needed for \code{\link{image}}, because it's axis always has a range of \code{c(0,1)}.}

\item{data}{to better estimate the start/end of the axis (if not present it uses the plot dimensions)}

\item{lim}{axis limits usually set to \code{range(date)} or extracted from \code{par("usr")}}

\item{...}{parameters to be passed on to axis}
}
\description{
Adds an axis that looks like a ruler (with minor ticks without labels between the large ticks).
}
\author{
Mark Heron
}
