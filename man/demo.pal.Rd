\name{demo.pal}
\alias{demo.pal}
\title{Demo plot for palett}
\usage{
demo.pal(colours, border = (if (n < 32) "light gray" else NA),
  main = paste("color palettes;  n=", n))
}
\arguments{
  \item{colours}{color palett}

  \item{border}{color of the boarder around the rectangles}

  \item{main}{figure title}
}
\description{
Simply plots rectangles with the palett colours
}
\examples{
demo.pal(colours=distinctive_colors(32))
}
\seealso{
distinctive_colors
}

