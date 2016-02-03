## #########################################################
## Purpose: Given a list of points' coordinates and the values
##          observed at those points, return a scatterplot
##          with points located as specified by the coordinates
##          and color-code to represnt the observed value at the
##          location.  This can be used in place of the
##          draw.uneven.image code which I've often used for
##          this purpose in the past.
##
## Inputs:
##   x, y: Coordinates of locations at which observations were made.
##   z: Values observed at the locations whose coordinates are
##        given by (x, y).
##   zlim: Minimum and maximum value of z to which to assign the two
##        most "extreme" colors in the col argument.  Default is to
##        use the range of z.
##   add: If TRUE, adds points to a pre-existing plot.
##        If FALSE, put points onto a new plot.
##   col: Color range to use for the points, with the first color
##        assigned to the zlim[1] and last color assigned to zlim[2].
##   pch: The pointing symbol to use.  Possible values are 21, 22, 23,
##        24, 25.  This is because points.geodata() requires these
##        points, which have outlines around them.  Default is a
##        circle (pch=21).
##   cex.min, cex.max: These control the minimum and maximum amounts to
##        shrink/expland the points, based on the value of z.  By default,
##        these are both set to one, which makes all the points the same
##        size.  For more information, see the help page for
##        points.geodata().
##   symbol.border.col: This controls the color of the border around the
##        plotting symbol.  By default, it is black.  If you don't want
##        to have a border at all, use symbol.border.col="transparent".
##   ...: Any other parameters the user adds will be passed to the
##        plot() function if add=F, and may include options for axis
##        labels, titles, etc.
##
## Returns:
##   A scatterplot with points at (x, y) and colored according to the
##   correspoinding value of z and the colors specified in col.
##
## Assumes:
##   Availability of package geoR for the key function points.geodata().
##
##
## REVISION HISTORY:
##   Prototype: Jenise Swall, 09Apr2010
## #########################################################
draw.color.scatterplot.w.geoR <- function(x, y, z, zlim=range(z, na.rm=T),
                                          add=F, col=heat.colors(12),
                                          pch=21, cex.min=1, cex.max=1,
                                          symbol.border.col="black", ...)
{

  ## Pch values must be 21, 22, 23, 24, or 25, as required by
  ## points.geodata().
  if (!(pch %in% c(21, 22, 23, 24, 25)))
    stop("Value of pch must be 21, 22, 23, 24, or 25")


  ## We need to load geoR package to use the function points.geodata()
  ## below.
  require("geoR")


  ## Divide the total range up into as many bins as you have colors.
  ## The object formed below contains the endpoints for each bin, so
  ## it has to have length of 1 plus the number of bins.
  how.to.divide <- seq(zlim[1], zlim[2], length=1+length(col))


  ## Note that points.geodata() will set the plot axes so that they
  ## have the same length.  While this makes the perspective better,
  ## it also can result in a lot of white space in the plot.  So, we
  ## set up the plotting region first (using the R default), and then
  ## add the points with the points.geodata function.  This has the
  ## added advantage of letting us control the symbol border color,
  ## which due to minor error in points.geodata, can't be done unless
  ## you're adding the points to a plot. This means that if you want
  ## to preserve the perspective, I will use code (like that in
  ## initialize.aspect.plot) to control the actual size of the figure.
  if (!add)
    plot(x, y, type="n", ...)
  ## Make x, y, z into a geoR geodata object.
  points.geodata(coords=cbind(x,y), data=z, col.seq=col,
                 pt.divide=how.to.divide, cex.min=cex.min, cex.max=cex.max,
                 pch.seq=pch, add.to.plot=T, col=symbol.border.col)
}
