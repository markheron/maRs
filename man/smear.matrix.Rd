\name{smear.matrix}
\alias{smear.matrix}
\title{smear.matrix}
\usage{
smear.matrix(x, from = 0, to = 1)
}
\arguments{
  \item{x}{matrix to smear}

  \item{from}{where to start}

  \item{to}{where to go}
}
\value{
smeared ff matrix
}
\description{
Computes a smeared matrix along the first dimension,
basically a running_sum.
}
\details{
Add's the matrix on its self several times, uses a
tree/recursive like approach so the time complexity is only
log(to-from). The start and end will end up with smaller
values on average, since the ends are extended with zeros.
}

