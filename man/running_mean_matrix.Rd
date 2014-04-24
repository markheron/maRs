\name{running_mean_matrix}
\alias{running_mean_matrix}
\title{running mean on matrix}
\usage{
running_mean_matrix(mat, smooth, over = "rows")
}
\arguments{
  \item{mat}{(numeric) matrix to smooth}

  \item{smooth}{(int) the smothing window size}

  \item{over}{(character) if the matrix should be smoothed
  over (i.e. along) the rows (first dimension) or columns
  (second dimension)}
}
\value{
smoothed matrix
}
\description{
Computes the running mean over one dimension of a matrix.
}
\details{
Should be updated to use speed up also used with smear, or
reimplemented using (still to be implemented) smear_matrix

The matrix \code{mat} is smoothed by computing the mean for
a centered running window of the size \code{smooth} allong
each row or column. The first and last positions are
repeats of the outermost positiones for which the window
still fit. If \code{smooth} is an even number, the window
is extended further to the end.
}
\examples{
running_mean_matrix(matrix(1:16,4,4), 3, over="rows")
}
\seealso{
running_mean
}

