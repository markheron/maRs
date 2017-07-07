

#' Ops.ff_matrix
#'
#' @import ffbase
#' @param e1,e2 objects
#' @export
#'
Ops.ff_matrix <- function(e1,e2=NULL) {
  
  if( any(ff::dimorder(e1) != ff::dimorder(e2)) ) {
    warning("The method isn't doing what you think it is doing!")
  }
  
  e1_vector <- e1
  if("ff_array" %in% class(e1_vector)) {
    class(e1_vector) <- c("ff_vector", "ff")
  }
  e2_vector <- e2
  if("ff_array" %in% class(e2_vector)) {
    class(e2_vector) <- c("ff_vector", "ff")
  }
  
  tmp <- callGeneric(e1_vector, e2_vector)
  class(tmp) <- c("ff_matrix", "ff_array", "ff")
  dim(tmp) <- dim(e1)
  ff::dimorder(tmp) <- ff::dimorder(e1)
  return(tmp)
}


#' Math.ff_matrix
#'
#' @import ffbase
#' @param x object
#' @param \dots further arguments passed to methods
#' @export
#'
Math.ff_matrix <- function(x, ...) {
  
  x_vector <- x
  if("ff_array" %in% class(x_vector)) {
    class(x_vector) <- c("ff_vector", "ff")
  }
  
  tmp <- callGeneric(x_vector, ...)
  
  class(tmp) <- c("ff_matrix", "ff_array", "ff")
  dim(tmp) <- dim(x)
  ff::dimorder(tmp) <- ff::dimorder(x)
  return(tmp)
}


#' Summary.ff_matrix
#'
#' @import ffbase
#' @param e1 object
#' @param \dots further arguments passed to methods
#' @export
#'
Summary.ff_matrix <- function(e1, ...) {
  
  e1_vector <- e1
  if("ff_array" %in% class(e1_vector)) {
    class(e1_vector) <- c("ff_vector", "ff")
  }
  
  tmp <- callGeneric(e1_vector, ...)
#   class(tmp) <- c("ff_matrix", "ff_array", "ff")
#   dim(tmp) <- dim(e1)
  return(tmp)
}
