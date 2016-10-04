## ###########################################################
## Purpose: Put color bar legend on the right side of your plot.
##
## Input:
##   zlim: Gives the range of z values to which you want the colors
##         ("col") assigned.
##   col: Gives the range of colors you want to use
##     To keep multiple plots consistent in terms of the colors
##     assigned to various values, keep zlim and col the same for each
##     of the plots.
##
## Returns: Puts vertical color bar legend to the right of the plot.
##
## Assumes:
##   1. You have already finished making the main portion of the plot;
##      i.e., ADD THE LEGEND LAST.  Do not try to add secondary
##      information onto the plot after you use this legend command,
##      because it won't work.  This is because this funciton alters
##      the par() settings to draw the legend, and trying to reset
##      them sometimes causes errors that are as yet unexplained.
##   2. You are only generating one plot on this device.
##
## 
## Notes:
##    As noted in the help for image.plot() in the "fields" package,
##    putting a legend on a plot is harder than it looks.  You may
##    have to play with this function a bit to get it to work for you.
##    You may also want to try the previously mentioned image.plot()
##    function (to just add the legend, use (legend.only=T).
##
##
## REVISION HISTORY:
##   Prototype: Jenise L. Swall, 09Apr2010
##
##   16Jun (JLS): Altered function so that, when exiting, we do not
##                try to reset the par settings to what they were
##                when we entered the function.  This was causing
##                sporadic errors that are as yet unexplained.
## ###########################################################
vertical.image.legend <- function(zlim, col, cex=0.5, log.scale=FALSE){


  ## Get the current par information.  We restore these settings
  ## before we leave this function.
  ## starting.par.settings <- par(no.readonly=TRUE)


  ## Find information about the overall size of the figure and the
  ## margins included with that figure.
  mai <- par("mai")
  fin <- par("fin")
  

  ## Find total amount of figure space left over after the main plot
  ## is drawn. This really boils down to the area to the right of the
  ## plot, but under the top margin (which may hold a title) and above
  ## the bottom margin (which may hold a label for the x-axis).
  x.legend.fig <- c( 1.0-(mai[4]/fin[1]), 1.0 )
  y.legend.fig <- c( mai[1]/fin[2], 1.0-(mai[3]/fin[2]) )
  
  ## Now, give the portion of this area which can be used for the
  ## actual legend strip.  This means we need to leave a litle space
  ## between the strip and the main plot, and a bigger space for the
  ## labels to the right of the strip.  In the y direction, we can use
  ## all the space available.
  x.legend.plt <- c( x.legend.fig[1]+(0.08*(x.legend.fig[2]-x.legend.fig[1])),
                    x.legend.fig[2]-(0.6*(x.legend.fig[2]-x.legend.fig[1])) )
  y.legend.plt <- y.legend.fig
  

  ## Colors to plot in the color bar.
  z <- seq(zlim[1], zlim[2], length=length(col))
  if (log.scale) z <- 10^seq(log10(zlim[1]),log10(zlim[2]),length=length(col))

  par( new=T, pty="m", plt=c(x.legend.plt, y.legend.plt) )
  ## par( new=T, xpd=T, pty="m", plt=c(x.legend.plt, y.legend.plt) )
  if (log.scale) {
    image(x=1, y=log10(z), z=matrix(log10(z), nrow=1, ncol=length(col)),
          col=col, xlab="", ylab="", xaxt="n", yaxt="n")
    label.z <- sapply(log10(z),function(x) {as.expression(parse(text=paste("10^{",x,"}",sep="")))})
    axis(4, at=log10(z),label=as.expression(label.z), mgp = c(3, 0.2, 0), las = 2, cex.axis=cex, tcl=-0.1, ticks=FALSE)    
  } else {
    image(x=1, y=z, z=matrix(z, nrow=1, ncol=length(col)),
          col=col, xlab="", ylab="", xaxt="n", yaxt="n")
    axis(4, mgp = c(3, 0.2, 0), las = 2, cex.axis=cex, tcl=-0.1)    
  }

  box()


  ## Return par settings to what they were when we entered this function.
  ## par(starting.par.settings)
}
