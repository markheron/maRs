


# test smear_ff

smear_ff_1 <- function(x, from=0, to=1) {
  
  len <- length(x)
  smeared <- ff(0,length=len)
  for(i in from:to) {
    smeared[] <- smeared[] + c(rep(0,-min(i,0)),x[max(i+1,1):min(len+i,len)],rep(0,max(i,0)))
  }
  return(smeared)
}


smear_ff_2 <- function(x, from=0, to=1) {
  
  len <- length(x)
  smeared <- ff(0,length=len)
  for(i in from:to) {
    smeared <- smeared[] + c(rep(0,-min(i,0)),x[max(i+1,1):min(len+i,len)],rep(0,max(i,0)))
  }
  return(smeared)
}



smear_ff_3 <- function(x, from=0, to=1) {
  
  len <- length(x)
  smeared <- rep(0,length=len)
  for(i in from:to) {
    smeared <- smeared + c(rep(0,-min(i,0)),x[max(i+1,1):min(len+i,len)],rep(0,max(i,0)))
  }
  return(smeared)
}


smear_ff_4 <- function(x, from=0, to=1) {
  # the from, to parameter meaning seem reversed compared to my intuition (at least at the moment)
  # so maybe I should change it ...
  
  if(from == 0 & to == 0) {
    return(x)
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
    return(smeared)
  } else {
    return(smeared[-(1:to)]) #from
  }
}


vec_length <- 30000000 # longer than the longest chromosome

vec <- ff(0, length=vec_length)
vec[sample(1:vec_length, 1000)] <- sample(1:10, 1000, replace=TRUE)

system.time(tmp1 <- smear_ff_1(vec, -10,10) )
system.time(tmp2 <- smear_ff_2(vec, -10,10) )
system.time(tmp3 <- smear_ff_3(vec, -10,10) )
system.time(tmp4 <- smear_ff_4(vec, -10,10) )


smear_simple <- function(x, from=0, to=1) {
  
  if(from > to) {
    return(smear_simple(x,to,from))
  }
  
  len <- length(x)
  smeared <- rep(0,len)
  for(i in from:to) {
    smeared <- smeared + c(rep(0,-min(i,0)),x[max(i+1,1):min(len+i,len)],rep(0,max(i,0)))
  }
  return(smeared)
}


library(maRs)

vec_length <- 300000 # longer than the longest chromosome

vec <- rep(0, length=vec_length)
vec[sample(1:vec_length, 1000)] <- sample(1:10, 1000, replace=TRUE)

system.time(tmp1 <- smear(vec, -73,73) )
system.time(tmp2 <- smear_simple(vec, -73,73) )

all(tmp1==tmp2)

