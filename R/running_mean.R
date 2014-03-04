##' Running mean functions
##' 
##' running mean functions, that also work on a matrix, should probably switch to using the smear functions for speed increase with long vec/mat's.
##' 
##' @name running_mean-file
##' @author Mark Heron
NULL




#' running mean
#'
#' Computes the running mean of a vector.
#' 
#' The vector \code{vec} is smoothed by computing the mean for a centered running window of the size \code{smooth}.
#' The first and last positions are repeats of the outermost positiones for which the window still fit.
#' If \code{smooth} is an even number, the window is extended further to the end.
#'
#' @export
#' @param vec (numeric) vector to smooth
#' @param smooth (int) the smothing window size
#' @param ... parameters to forward to mean
#' @return smoothed vector
#' @seealso running_mean_matrix
#' @examples
#' running_mean(1:10, 3)
#' 
running_mean <- function(vec, smooth, ...) {
  smooth <- smooth-1
  if(smooth > 0) {
    if(smooth < length(vec)) {
      return(c( rep(mean(vec[1:(1+smooth)], ...),floor(smooth/2)) ,sapply(1:(length(vec)-smooth), function (i) {mean(vec[i:(i+smooth)], ...)}), rep(mean(vec[(length(vec)-smooth):(length(vec))], ...)  ,ceiling(smooth/2)) ))
    } else {
      return(rep(mean(vec, ...),length(vec)))
    }
  } else {
    return(vec)
  }
}


#' running mean on matrix
#'
#' Computes the running mean over one dimension of a matrix.
#' 
#' The matrix \code{mat} is smoothed by computing the mean for a centered running window of the size \code{smooth} allong each row or column.
#' The first and last positions are repeats of the outermost positiones for which the window still fit.
#' If \code{smooth} is an even number, the window is extended further to the end.
#'
#' @export
#' @param mat (numeric) matrix to smooth
#' @param smooth (int) the smothing window size
#' @param over (character) if the matrix should be smoothed over (i.e. along) the rows (first dimension) or columns (second dimension) 
#' @return smoothed matrix
#' @seealso running_mean
#' @examples
#' running_mean_matrix(matrix(1:16,4,4), 3, over="rows")
#' 
running_mean_matrix <- function(mat, smooth, over="rows") {
  smooth <- smooth-1
  
  if(over == "columns") {
    mat <- t(mat)
  }
  if(smooth > 0) {
    if(smooth < dim(mat)[1]) {
      smoothed <- cbind( sapply(1:(dim(mat)[1]-smooth), function (i) {colMeans(mat[i:(i+smooth),])})
                         ,matrix(colMeans(mat[(dim(mat)[1]-smooth):(dim(mat)[1]),]), dim(mat)[2] ,ceiling(smooth/2) ))
      if(smooth > 1) {
        smoothed <- cbind( matrix(colMeans(mat[1:(1+smooth),]),dim(mat)[2],floor(smooth/2) ), smoothed)
      }
    } else {
      smoothed <- sapply(1:(dim(mat)[1]), function (i) {colMeans(mat)})
    }
  } else {
    smoothed <- t(mat)
  }
  if(over != "columns") {
    smoothed <- t(smoothed)
  }
  return(smoothed)
}
