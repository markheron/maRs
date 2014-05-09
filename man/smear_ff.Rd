\name{smear_ff}
\alias{smear_ff}
\title{smear_ff}
\usage{
smear_ff(x, from = 0, to = 1)
}
\arguments{
  \item{x}{ff vector to smear}

  \item{from}{where to start}

  \item{to}{where to go}
}
\value{
smeared ff vector
}
\description{
Computes a smeared ff vector, basically a running_sum.
}
\details{
Add's the vector on its self several times, so good for
short smears of long vectors.
}
\author{
Mark Heron
}

