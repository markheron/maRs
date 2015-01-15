#' helper functions for ploting!
#' 
#' Distincti color selector, ruler axis and a function to display the colors.
#' 
#' @name plot_helpers
#' @author Mark Heron
NULL



#' Distinctive color palett
#' 
#' Produces a selection of distinct colors for plots with many >15 lines.
#' Inspired by distinctcolors in Bjoern Schwalb's LSD package.
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
  
  if( (n <= 4) & (four_colours == "motif") ) {
    return(c("green","blue","goldenrod","red")[1:n] ) # colours matching motif letter colours
  } else if( (n <= 4) & (four_colours == "old") ) {
    return(c("green","blue","red","black")[1:n]) # my old colours
    
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
  } else {
    warning("Method not known, using a random sampling of colors()")
    return(sample(colors(), n))
  }
}


#' rescale data
#'
#' the values of x are rescaled so that x_range goes from 0 to 1
#' 
#' @param x data to be rescaled
#' @param x_range the original range the data can be in, that is mapped to c(0,1)
#' @author Mark Heron
scale_to_zero_one_range <- function(x, x_range=range(x))  {
  
  if(length(x_range) == 0) {
    return(x)
  } else if(length(x_range) != 2) {
    warning("x_range is neither NULL nor of length 2")
  }
  return( (x-x_range[1])/(x_range[2]-x_range[1]) )
}


##' ruler style axis
##'
##' Adds an axis that looks like a ruler (with minor ticks without labels between the large ticks).
##' @export
##' @param side to add the axis to
##' @param axis_range_to_zero_one a range for which the axis labels should be scaled down to a range of \code{c(0,1)}. Needed for \code{\link{image}}, because it's axis always has a range of \code{c(0,1)}.
##' @param data to better estimate the start/end of the axis (if not present it uses the plot dimensions)
##' @param lim axis limits usually set to \code{range(date)} or extracted from \code{par("usr")}
##' @author Mark Heron
ruler_axis <- function(side=1, axis_range_to_zero_one=NULL, data=NULL, lim=NULL) {
  
  if(length(data) > 0) {
    lim <- range(data)
  }
  
  axis_p <- c()
  if(length(lim) == 2) {
    axis_p <- lim
  } else if(side == 1 | side ==3) {
    axis_p <- par("usr")[1:2]
  } else {
    axis_p <- par("usr")[3:4]
  }
  
  p_5 <- pretty(axis_p, 5)
  p_10 <- c()
  p_25 <- c()
  p_50 <- c()
  axis(side, at=scale_to_zero_one_range(p_5, axis_range_to_zero_one), labels=p_5, lwd=0, lwd.ticks=1)
  if(abs(p_5[2]-p_5[1]) %% 10 == 0 ) {
    p_10 <- setdiff( seq(p_5[1], p_5[length(p_5)], length.out=(length(p_5)-1)*2+1), p_5)
    p_50 <- setdiff( seq(p_5[1], p_5[length(p_5)], length.out=(length(p_5)-1)*10+1), p_10)
  } else if(abs(p_5[2]-p_5[1]) %% 5 == 0 ) {
    p_25 <- setdiff( seq(p_5[1], p_5[length(p_5)], length.out=(length(p_5)-1)*5+1), p_5)
  }
  
  axis(side, at=scale_to_zero_one_range(p_5, axis_range_to_zero_one), labels=p_5, lwd=0, lwd.ticks=1)
  if( length(p_10) == 0) {
    axis(side, at=scale_to_zero_one_range(p_25, axis_range_to_zero_one), lwd=0, lwd.ticks=1, labels=FALSE, tcl=-0.2)
  } else {
    axis(side, at=scale_to_zero_one_range(p_10, axis_range_to_zero_one), lwd=0, lwd.ticks=1, labels=FALSE, tcl=-0.4)
    axis(side, at=scale_to_zero_one_range(p_50, axis_range_to_zero_one), lwd=0, lwd.ticks=1, labels=FALSE, tcl=-0.2)
  }
  axis(side, at=scale_to_zero_one_range(c(p_5,p_10,p_25,p_50), axis_range_to_zero_one) ,labels=FALSE, lwd.ticks=0)
}


#' heatmap_axis
#' 
#' helper function to automatically plot a heatmap axis that makes sense
#' 
#' @param side the axis should be plotted on
#' @param axis_range numeric range of the axis
#' @param labels of each row/column (that is all, not just the ones you want plotted)
#' @param ruler_axis do you want to use the \code{ruler_axis()}?
#' @param ... further parameters for \code{axis(...)}
heatmap_axis <- function(side=1, axis_range=range(labels), labels, ruler_axis=TRUE, ...) {
  if(class(labels) == "numeric" || class(labels) == "integer") {
    labs_pretty <- pretty(labels)
    if(ruler_axis) {
      ruler_axis(side=side, axis_range_to_zero_one=axis_range, data=labels)
    } else {
      axis(side=side, at=scale_to_zero_one_range(labs_pretty, axis_range) , labels=labs_pretty, tick=TRUE, ...)
    }
  } else {
    axis(side=side, at=seq(0,1,length.out=length(labels)), labels=labels, tick=FALSE, ...)
  }
}


#' plotHeatmap
#' 
#' Mark's version of a nice heatmap function:
#'  - the matrix is ploted in the same configuration as it is represented in R
#'  - there is a colour scale on the top right
#'  - the scales are not percentages and are made pretty if the provided values are integers/numeric
#'  - the label margins can be set
#'  
#' @param z matrix of values to be plotted in the heatmap
#' @param x labels for the x axis
#' @param y labels for the y axis
#' @param colour_range numeric range for the colours of the heatmap
#' @param colour_range_symetric boolean if the colour range should be made symetric around zero
#' @param colour_steps how many different colours should be used
#' @param colour_spectrum the major colours that should be used for the heatmap
#' @param label_spaces space for the x and y axis labels in percent of the complete figure size
#' @param main title of the figure
#' 
#' @export
plotHeatmap <- function(z,x=1:ncol(z),y=1:nrow(z), colour_range=range(z), colour_range_symetric=FALSE, colour_steps=1000, colour_spectrum=c("blue", "white", "red"), label_spaces=c(0.1,0.1), main="") {  
  
  if(colour_range_symetric) {
    colour_range <- rep(max(abs(colour_range), na.rm=T),2) * c(-1,1)
  } 
  
  breaks <- c(min(z, colour_range[1], na.rm=T), seq(from=colour_range[1], to=colour_range[2], length=colour_steps), max(z, colour_range[2], na.rm=T))
  colour_scale <- colorRampPalette(colour_spectrum)(colour_steps+1)
  
  
  par(mar=c(0,0,0,0),fig=c(0.87,0.89,0.65,0.85),cex.axis=1.3)
  image(x=1,y=seq(from=colour_range[1], to=colour_range[2], length=colour_steps),z=matrix(1:colour_steps,nrow=1),col=colour_scale,xaxt='n',yaxt='n',ylab="",xlab="")
  axis(side=4,las=1)
  
  par(mar=c(0,0,0,0),fig=c(label_spaces[1],0.85,label_spaces[2],0.90),cex.axis=1.5,new=TRUE)
  image(t(z)[,nrow(z):1], col=colour_scale, breaks=breaks,axes=FALSE)
  title(main=main, outer=TRUE, line=-2)
  heatmap_axis(side=1,labels=x,las=1,ruler_axis=!all(x==1:ncol(z)))
  heatmap_axis(side=2,labels=y,las=1,ruler_axis=!all(y==1:nrow(z)))
}



#' Demo plot for palette
#' 
#' Simply plots rectangles with the palette colours.
#' Method idea and initial code from LSD's package.
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