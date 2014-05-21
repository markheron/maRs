\name{plotHeatmap}
\alias{plotHeatmap}
\title{plotHeatmap}
\usage{
plotHeatmap(z, x = 1:ncol(z), y = 1:nrow(z), colour_range = range(z),
  colour_range_symetric = FALSE, colour_steps = 1000,
  colour_spectrum = c("blue", "white", "red"), label_spaces = c(0.1, 0.1),
  main = "")
}
\arguments{
  \item{z}{matrix of values to be plotted in the heatmap}

  \item{x}{labels for the x axis}

  \item{y}{labels for the y axis}

  \item{colour_range}{numeric range for the colours of the
  heatmap}

  \item{colour_range_symetric}{boolean if the colour range
  should be made symetric around zero}

  \item{colour_steps}{how many different colours should be
  used}

  \item{colour_spectrum}{the major colours that should be
  used for the heatmap}

  \item{label_spaces}{space for the x and y axis labels in
  percent of the complete figure size}

  \item{main}{title of the figure}
}
\description{
Mark's version of a nice heatmap function: - the matrix is
ploted in the same configuration as it is represented in R
- there is a colour scale on the top right - the scales are
not percentages and are made pretty if the provided values
are integers/numeric - the label margins can be set
}

