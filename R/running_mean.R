##' Running mean functions
##' 
##' running mean functions (also for matrix) internally uses the smear functions for speed/space and then divides by the amount of position summed over.
##' 
##' @name running_mean-file
##' @author Mark Heron
NULL




#' running mean
#'
#' Computes the running mean of a vector.
#' 
#' The vector \code{vec} is smoothed by computing the mean for a centered running window of the size \code{smooth}.
#' The first and last positions are smoothed over smaller running windows (i.e. cut off at the beginnig/end of the \code{vec}).
#' If \code{smooth} is an even number, the window is extended further to the end.
#'
#' [USE TO BE: The first and last positions are repeats of the outermost positiones for which the window still fit.]
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
  smooth_minus_one <- smooth-1
  if(smooth_minus_one > 0) {
    if(smooth_minus_one < length(vec)) {
      return(smear(vec, from=-ceiling(smooth_minus_one/2), to=floor(smooth_minus_one/2)) / smear(rep(1,length(vec)), from=-ceiling(smooth_minus_one/2), to=floor(smooth_minus_one/2)) )
      #return(smear(vec, from=-ceiling(smooth_minus_one/2), to=floor(smooth_minus_one/2)) / (1+c(ceiling(smooth_minus_one/2):(smooth_minus_one), rep((smooth_minus_one), length(vec)-smooth_minus_one-2) , (smooth_minus_one):floor(smooth_minus_one/2))) )
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
#' The matrix \code{mat} is smoothed by computing the mean for a centered running window of the size \code{smooth} along each row or column.
#' The first and last positions are smoothed over smaller running windows (i.e. cut off at the beginnig/end of the \code{mat}).
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
  
  if(over == "columns") {
    return(t(running_mean_matrix(t(mat),smooth, over="rows")))
  } else if (over != "rows") {
    warning("over parameter neither matches columns nor rows!\n using rows for now!")
  }
  
  smooth_minus_one <- smooth-1
  
  if(smooth_minus_one > 0) {
    #if(smooth <= dim(mat)[1]) {
      
      smoothed <- smear(mat, from=-ceiling(smooth_minus_one/2), to=floor(smooth_minus_one/2)) / smear(rep(1, nrow(mat)), from=-ceiling(smooth_minus_one/2), to=floor(smooth_minus_one/2)) 
      #smoothed <- smear(mat, from=-ceiling(smooth/2), to=floor(smooth/2)) / (1+c(ceiling(smooth/2):(smooth), rep((smooth), nrow(mat)-smooth-2) , (smooth):floor(smooth/2)))
      
    #} else {
    #  smoothed <- t( sapply(1:(dim(mat)[1]), function (i) {colMeans(mat)}) )
    #}
  } else {
    smoothed <- mat
  }
  
  return(smoothed)
}



#' running mean on ff_matrix
#' 
#' @import ff
#' @export
#' @seealso running_mean_matirx
#' @examples
#' library(ff)
#' running_mean_ff_matrix(as.ff(matrix(1:16,4,4)), 3, over="rows")
#' 
running_mean_ff_matrix <- function(mat, smooth, over="rows") {
  
  if(over == "columns") {
    return( clone( t(running_mean_ff_matrix(t(mat), smooth, over="rows")), dimorder=(1:length(dim(mat))) ))
    
  } else if (over != "rows") {
    warning("over parameter neither matches columns nor rows!\n using rows for now!")
  }
  smooth_minus_one <- smooth-1
  
  if(smooth_minus_one > 0) {
    #if(smooth <= dim(mat)[1]) {
    
    smoothed <- smear_ff_matrix(mat, from=-ceiling(smooth_minus_one/2), to=floor(smooth_minus_one/2)) / as.ff(matrix(rep(smear(rep(1, nrow(mat)), from=-ceiling(smooth_minus_one/2), to=floor(smooth_minus_one/2)), ncol(mat), ncol=ncol(mat) )))
    
    #smoothed <- as.ff( as.ram(smear_ff_matrix(mat, from=-ceiling(smooth/2), to=floor(smooth/2))) / as.ram(smear(rep(1, nrow(mat)), from=-ceiling(smooth/2), to=floor(smooth/2))))
    
    
#     smoothed <- smear_ff_matrix(mat, from=-ceiling(smooth/2), to=floor(smooth/2))
#     tmp2 <- smear(rep(1, nrow(mat)), from=-ceiling(smooth/2), to=floor(smooth/2))
#     for(i in 1:ncol(smoothed)) {
#       smoothed[,i] <- smoothed[,i]/tmp2[i]
#     }
    #smoothed <- ffcolapply( tmp[,i1:i2,drop=FALSE]/tmp2[i1:i2], X=tmp, RETURN=TRUE, CFUN="cmean")
    
    #} else {
    #  smoothed <- t( sapply(1:(dim(mat)[1]), function (i) {colMeans(mat)}) )
    #}
  } else {
    smoothed <- mat
  }
#   if(over == "columns") {
#     smoothed <- clone(t(smoothed), dimorder=(1:length(dim(smoothed))))
#   }
  return(smoothed)
}
