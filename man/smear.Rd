\name{smear}
\alias{smear}
\title{smear}
\usage{
smear(x, from = 0, to = 1)
}
\arguments{
  \item{x}{to smear}

  \item{from}{where to start}

  \item{to}{where to go}
}
\value{
smeared x
}
\description{
Computes a smeared version of the data, basically a
running_sum.
}
\details{
Add's the vector on its self several times, uses a
tree/recursive like approach so the time complexity is only
\code{log(to-from)}. The start and end will end up with
smaller values on average, since the ends are extended with
zeros.
}
\seealso{
\code{\link{smear.numeric}}

\code{\link{smear.matrix}}
}

