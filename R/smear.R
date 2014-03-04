##' smear functions
##' 
##' smear functions that are fast for long vectors
##' 
##' @name smear-file
##' @author Mark Heron
NULL


##' smear_simple
##'
##' Computes a smeared vector, basically a running_sum.
##' 
##' add's the vector on its self several times, so good for short smears of long vectors
##'
##' @param x vector to smear
##' @param from where to start
##' @param to where to go
##' @return smeared ff vector
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
##' Computes a smeared vector, basically a running_sum.
##' 
##' add's the vector on its self several times, so good for short smears of long vectors
##'
##' @export
##' @param x vector to smear
##' @param from where to start
##' @param to where to go
##' @return smeared ff vector
smear <- function(x, from=0, to=1) {
  
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



##' Computes a smeared vector, basically a running_sum.
##' 
##' Add's the vector on its self several times, so good for short smears of long vectors.
##' 
##' @title smear_ff_simple
##' @param x ff vector to smear
##' @param from where to start
##' @param to where to go
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
  return(as.ff(smeared))
}



##' Computes a smeared vector, basically a running_sum.
##' 
##' Add's the vector on its self several times, so good for short smears of long vectors.
##' 
##' @export
##' @title smear_ff
##' @param x ff vector to smear
##' @param from where to start
##' @param to where to go
##' @return smeared ff vector
##' @importFrom ff as.ff 
##' @author Mark Heron
smear_ff <- function(x, from=0, to=1) {
  
  
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
  return(as.ff(smeared))
}
