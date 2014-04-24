\name{smear}
\alias{smear}
\title{smear}
\usage{
smear(x, from = 0, to = 1)
}
\arguments{
  \item{x}{vector to smear}

  \item{from}{where to start}

  \item{to}{where to go}
}
\value{
smeared ff vector
}
\description{
Computes a smeared vector, basically a running_sum.
}
\details{
add's the vector on its self several times, so good for
short smears of long vectors
}

