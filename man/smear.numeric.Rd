\name{smear.numeric}
\alias{smear.numeric}
\title{smear.numeric}
\usage{
smear.numeric(x, from = 0, to = 1)
}
\arguments{
  \item{x}{numeric vector to smear}

  \item{from}{where to start}

  \item{to}{where to go}
}
\value{
smeared vector
}
\description{
Computes a smeared vector, basically a running_sum.
}
\details{
Add's the vector on its self several times, uses a
tree/recursive like approach so the time complexity is only
log(to-from). The start and end will end up with smaller
values on average, since the ends are extended with zeros.
}

