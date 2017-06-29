##' smear functions
##' 
##' smear functions that are fast for long vectors
##' 
##' @name smear-file
##' @author Mark Heron
##' 
##' @import methods
NULL


##' smear_simple
##'
##' Computes a smeared vector, basically a running_sum.
##' Simple (and slow) version used to test against.
##' 
##' Adds the vector on its self several times, so good for short smears of long vectors.
##'
##' @param x vector to smear
##' @inheritParams smear
##' @return smeared vector
smear_simple <- function(x, from=0, to=1) {
  
  if(from > to) {
    return(smear_simple(x,to,from))
  }
  
  len <- length(x)
  smeared <- rep(0,len)
  for(i in from:to) {
    smeared <- smeared + c(rep(0,max(i,0)),x[max(1-i,1):min(len-i,len)],rep(0,-min(i,0)))
  }
  return(smeared)
}




##' smear
##' 
##' Computes a smeared version of the data, basically a running sum.
##'
##' Adds the data shifted onto itself several times, using a tree/recursive approach so the time complexity is only \strong{log(to-from)}.
##' The start and end will end up with smaller values on average, since the ends are extended with zeros.
##' 
##' \subsection{if x is numeric vector}{
##' The vector is shifted as expected.
##' }
##'
##' \subsection{if x is numeric matrix}{
##' The matrix is shifted along the first dimension i.e. the smearing happens along the rows.
##' }
##' 
##' @export
##' @param x numeric data to smear
##' @param from where to start
##' @param to where to go
##' @return smeared x
##' 
##' @aliases smear,numeric,matrix-method
##' @aliases smear.numeric,smear.matrix
smear <- function(x, from=0, to=1) {
  warning("smear is not implemented for your x class type: ",class(x),"\n x is returned without change!")
  return(x)
}
setGeneric("smear")



smear.numeric <- function(x, from=0, to=1) {
  
  if(from > to) {
    return(smear(x,to,from))
  }
  
  smooth_len <- to-from+1
  times <- floor(log2(smooth_len))-1
  
  smeared <- c(rep(0,max(0,from)), x, rep(0,max(0,-from)))
  len_smear <- length(smeared)
  if(times >= 0) {
    for(iter in 0:times) {
      i <- 2^iter
      smeared <- smeared + c(rep(0,i),smeared[1:(len_smear-i)])
    }
  }
  smeared <- smeared[(max(1,1-from)):min(len_smear, len_smear-from)]
  
  new_from <- (from+2^floor(log2(smooth_len)))
  if(new_from <= to) {
    
    smeared <- smeared + smear(x, new_from, to)
  }
  return(smeared)
}
##' smear.numeric
##' @aliases smear
##' @rdname smear
setMethod("smear", "numeric", smear.numeric)



smear.matrix <- function(x, from=0, to=1) {
  
  if( !is.numeric(x) ) {
    warning("x is a non numeric matrix!\n x is returned without change!")
    return(x)
  }
  
  if(from > to) {
    return(smear(x,to,from))
  }
  
  smooth_len <- to-from+1
  times <- floor(log2(smooth_len))-1
  
  smeared <- rbind( matrix(0, nrow=max(0,from), ncol=ncol(x)) , x, matrix(0,nrow=max(0,-from), ncol=ncol(x)))
  len_smear <- nrow(smeared)
  if(times >= 0) {
    for(iter in 0:times) {
      i <- 2^iter
      smeared <- smeared + rbind(matrix(0,nrow=i, ncol=ncol(x)),smeared[1:(len_smear-i),])
    }
  }
  smeared <- smeared[(max(1,1-from)):min(len_smear, len_smear-from),]
  
  new_from <- (from+2^floor(log2(smooth_len)))
  if(new_from <= to) {
    
    smeared <- smeared + smear(x, new_from, to)
  }
  return(smeared)
}
##' smear.matrix
##' @aliases smear
##' @rdname smear
setMethod("smear", "matrix", smear.matrix)



##' Computes a smeared vector, basically a running_sum.
##' 
##' Adds the data shifted onto itself several times, implementation of \code{\link{smear_simple}} for \code{ff_vector}.
##' 
##' @title smear_ff_simple
##' @inheritParams smear_ff
##' @return smeared ff vector
##' @importFrom ff as.ff 
##' @author Mark Heron
smear_ff_simple <- function(x, from=0, to=1) {
  
  if(from > to) {
    return(smear_ff_simple(x,to,from))
  }
  
  len <- length(x)
  smeared <- rep(0,len)
  for(i in from:to) {
    smeared <- smeared + c(rep(0,max(i,0)),x[max(1-i,1):min(len-i,len)],rep(0,-min(i,0)))
  }
  return(ff::as.ff(smeared))
}



##' smear_ff
##' 
##' Computes a smeared ff vector, basically a running_sum.
##' 
##' Adds the data shifted onto itself several times, implementation of \code{\link{smear}} for \code{ff_vector}.
##' 
##' @export
##' @param x ff vector to smear
##' @param from where to start
##' @param to where to go
##' @return smeared ff vector
##' @importFrom ff as.ff 
##' @author Mark Heron
smear_ff <- function(x, from=0, to=1) { #_vector
  
  if(from > to) {
    return(smear_ff(x,to,from))
  }
  
  smooth_len <- to-from+1
  times <- floor(log2(smooth_len))-1
  
  smeared <- c(rep(0,max(0,from)), x[], rep(0,max(0,-from)))
  len_smear <- length(smeared)
  if(times >= 0) {
    for(iter in 0:times) {
      i <- 2^iter
      smeared <- smeared + c(rep(0,i),smeared[1:(len_smear-i)])
    }
  }
  smeared <- smeared[(max(1,1-from)):min(len_smear, len_smear-from)]
  
  new_from <- (from+2^floor(log2(smooth_len)))
  if(new_from <= to) {
    
    smeared <- smeared + (smear_ff(x, new_from, to)[])
  }
  return(ff::as.ff(smeared))
}
# setMethod("smear", "ff_vector", smear.ff_vector)






##' smear_ff_matrix
##' 
##' @export
##' @importFrom ff as.ff
##' @importFrom ff add 
##' @author Mark Heron
smear_ff_matrix <- function(x, from=0, to=1) {
  
  if( !("ff_matrix" %in% class(x)) ) {
    warning("x is not a ff_matrix!\n x is returned without change!")
    return(x)
  }
  
  if(from > to) {
    return(smear_ff_matrix(x,to,from))
  }
  
  smooth_len <- to-from+1
  times <- floor(log2(smooth_len))-1
  
  smeared <- ff::as.ff(rbind( matrix(0, nrow=max(0,from), ncol=ncol(x)) , x[], matrix(0,nrow=max(0,-from), ncol=ncol(x))))
  
  len_smear <- nrow(smeared)
  if(times >= 0) {
    for(iter in 0:times) {
      i <- 2^iter
      smeared <- smeared + ff::as.ff(rbind(matrix(0,nrow=i, ncol=ncol(x)),smeared[1:(len_smear-i),]) )
    }
  }
  smeared <- ff::as.ff(smeared[(max(1,1-from)):min(len_smear, len_smear-from),])
  
  new_from <- (from+2^floor(log2(smooth_len)))
  if(new_from <= to) {
    
    smeared <- smeared + smear_ff_matrix(x, new_from, to)
  }
  return(smeared)
}
# setMethod("smear", "matrix", smear.matrix)