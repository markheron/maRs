##' smear functions
##' 
##' smear functions that are fast for long vectors
##' 
##' @name smear-file
##' @author Mark Heron
NULL


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
  
  from <- -from
  to <- -to
  
  len <- length(x)
  smeared <- rep(0,len)
  for(i in from:to) {
    smeared <- smeared + c(rep(0,-min(i,0)),x[max(i+1,1):min(len+i,len)],rep(0,max(i,0)))
  }
  return(smeared)
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
  # the from, to parameter meaning seem reversed compared to my intuition (at least at the moment)
  # so maybe I should change it ...
  
  if(from == 0 & to == 0) {
    return(x)
  }
  
  from <- -from
  to <- -to
  
  if(from > to) {
    tmp <- to
    to <- from
    from <- tmp
  }
  
  smooth_len <- to-from+1
  times <- floor(log2(smooth_len))-1
  
  smeared <- c(x[], rep(0,to)) #to
  len <- length(smeared)
  for(iter in 0:times) {
    i <- 2^iter
    smeared <- smeared + c(rep(0,i),smeared[1:(len-i)])
  }
  if((2^(times+1)) < smooth_len) {
    len <- length(x)
    for(i in (-to-1+((2^(times+1)+1):smooth_len)) ) {
      smeared <- smeared + c(rep(0,to-min(-i,0)),x[max(-i+1,1):min(len-i,len)],rep(0,max(-i,0)))
    }
  }
  if(to == 0) {
    return(as.ff(smeared))
  } else {
    return(as.ff(smeared[-(1:to)])) #from
  }
}
