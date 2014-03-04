#' helper functions for ploting!
#' 
#' Distincti color selector, ruler axis and a function to display the colors.
#' 
#' @name plot_helpers
#' @author Mark Heron
NULL



#' Distinctive color palett
#' 
#' Produces a selection of distinct colors for plots with many >15 lines
#' 
#' @export
#' @param n (int) number of colors to produce
#' @param method (character) "hsv_split" or "rgb_split";
#' "hsv_split" creats four sets ("normal","grayish","dark","light") of rainbow colours (even split of hue),
#' "rgb_split" picks colors from an evenly spaced grid in the rgb color cube
#' @param four_colours (character) should n=4 be treated specialy as the typical PWM nucleotide colours (\code{"motif"}) or the old colour scheme  (\code{"old"})
#' @return vector of the selected colors
#' @seealso demo.pal
#' @examples
#' col=distinctive_colors(32)
#' demo.pal(col=distinctive_colors(32))
#' 
distinctive_colors <- function(n, method="hsv_split", four_colours="motif") {
  
  if( (n == 4) & (four_colours == "motif") ) {
    return(c("green","blue","goldenrod","red")) # colours matching motif letter colours
  } else if( (n == 4) & (four_colours == "old") ) {
    return(c("green","blue","red","black")) # my old colours
    
  } else if(method == "rgb_split") {
    maxvalue <- 255
    cube_edge <- ceiling( (n+2)^(1/3) ) #n*2
    valseq = seq(0, maxvalue, length.out = cube_edge)
    
    tmp <- NULL
    for(i in 1:length(valseq)) {
      tmp <- rbind(tmp, cbind(valseq[i], valseq))
    }
    spl <- NULL
    for(i in 1:length(valseq)) {
      spl <- rbind(spl, cbind(valseq[i], tmp))
    }
    
    #     tmp_truefalse_mat <- matrix(c(FALSE, TRUE), cube_edge, cube_edge+1, byrow=TRUE)
    #     spl <- spl[ c(as.vector(tmp_truefalse_mat[,-cube_edge+1]),as.vector(tmp_truefalse_mat[,-1])),]
    spl = spl[-c(1, nrow(spl)), ]
    
    spl = spl[floor(seq(from=1,to=dim(spl)[1],length.out=n)),]
    
    pal = apply(spl, 1, function(x) {  rgb(x[1], x[2], x[3], maxColorValue = 255) })
    
    if(n == 16) {
      pal[16] <- "gold"
    }
    return(pal)
    
  } else if(method == "hsv_split") {
    
    part = n%/%4+1
    s = seq(0,1,length.out=part)[-part]
    shift = 1/(part*4)
    pal = c(hsv(s,1,1),
            hsv(s+shift,0.45,0.7),
            hsv(s+2*shift,1,0.5),
            hsv(s+3*shift,0.3,1),
            hsv(0,0,seq(1,0,length.out=n%%4+1))[-1])
    if(n == 16) {
      pal[13] <- "darkorange"
    }
    return(pal[1:n])
  }
}




##' ruler_axis
##'
##' Adds an axis that looks like a ruler (with minor ticks without labels).
##' @export
##' @param side to add the axis to
##' @param data to better estimate the start/end of the axis (if not present it uses the plot dimensions)
##' @author Mark Heron
ruler_axis <- function(side=1, data=NULL) {
  
  axis_p <- c()
  if(length(data) > 0) {
    axis_p <- range(data)
  } else if(side == 1 | side ==3) {
    axis_p <- par("usr")[1:2]
  } else {
    axis_p <- par("usr")[3:4]
  }
  
  p_5 <- pretty(axis_p, 5)
  axis(side, p_5 , lwd=0,  lwd.ticks=1)
  p_10 <- setdiff(pretty(axis_p, 10, u5.bias=10), pretty(axis_p, 5))
  if( length(p_10) == 0) {
    axis(side, setdiff(pretty(axis_p, 25), p_5), labels=FALSE, tcl=-0.2)
  } else {
    axis(side, p_10, lwd=0, lwd.ticks=1, labels=FALSE, tcl=-0.4)
    axis(side, setdiff(pretty(axis_p, 50), union(p_10, p_5)), labels=FALSE, tcl=-0.2)
  }
}




#' Demo plot for palett
#' 
#' Simply plots rectangles with the palett colours
#' 
#' @export
#' @param colours color palett
#' @param border color of the boarder around the rectangles
#' @param main figure title
#' @seealso distinctive_colors
#' @examples
#' demo.pal(colours=distinctive_colors(32))
#' 
demo.pal <-  function(colours, border = (if (n<32) "light gray" else NA), main = paste("color palettes;  n=",n) ) {
  n <- length(colours)
  plot(c(1,1),c(1,n+1), type="n", yaxt="n", ylab="", main=main)
  for (k in 1:n) {
    rect(.6, k, 1.4, k+1, col = colours[k], border = border)
  }
}