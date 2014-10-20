

#' Ops.ff_matrix
#'
#' @import ff
#' @import ffbase
#' @export
#'
Ops.ff_matrix <- function(e1,e2=NULL) {
  
  if( any(dimorder(e1) != dimorder(e2)) ) {
    warning("the method isn't doing what you think it is doing!")
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
  dimorder(tmp) <- dimorder(e1)
  return(tmp)
}


#' Math.ff_matrix
#'
#' @import ff
#' @import ffbase
#' @export
#'
Math.ff_matrix <- function(e1,e2=NULL) {
  
  e1_vector <- e1
  if("ff_array" %in% class(e1_vector)) {
    class(e1_vector) <- c("ff_vector", "ff")
  }
  e2_vector <- e2
  if("ff_array" %in% class(e2_vector)) {
    class(e2_vector) <- c("ff_vector", "ff")
  }
  
  if(length(e2)==0) {
    tmp <- callGeneric(e1_vector)
  } else {
    tmp <- callGeneric(e1_vector, e2_vector)
  }
  class(tmp) <- c("ff_matrix", "ff_array", "ff")
  dim(tmp) <- dim(e1)
  dimorder(tmp) <- dimorder(e1)
  return(tmp)
}


#' Summary.ff_matrix
#'
#' @import ff
#' @import ffbase
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
