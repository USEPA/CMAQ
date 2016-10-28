args <- commandArgs(TRUE)

file.name <- unlist(strsplit(args[1],"="))[2]
file.img <- unlist(strsplit(args[2],"="))[2]
file.Rscript <- unlist(strsplit(args[3],"="))[2]

print(args)
print(c(file.name, file.img, file.Rscript))


library(maps)



source(paste(file.Rscript,"/func_for_CMAQ_files_with_ncdf_rgdal.r", sep=""))
source(paste(file.Rscript,"/vertical_image_legend.R", sep=""))
source(paste(file.Rscript,"/draw_color_scatterplot_w_geoR.r", sep=""))
source(paste(file.Rscript,"/dapply.R", sep=""))


########################################
# open CMAQ LTNG params file
########################################

grid.data <- get.Models3.variable(file = file.name, ldatetime=get.datetime.seq(file.name)[1],
                                  udatetime=get.datetime.seq(file.name)[1],var="OCNMASK")

km.square <- (grid.data$x.cell.ctr[2]-grid.data$x.cell.ctr[1]) *
  (grid.data$y.cell.ctr[2]-grid.data$y.cell.ctr[1]) 

library("fields")





#################################################
# plot maps of lightning NOx parameters
#################################################

# initialize map

world.lines <- get.map.lines.Models3.proj(file=file.name, region="world")

# CMAQ
pdf(file.img)
#tiff("~/tmp/summer_average_deposition_case.tiff", height=1800, width=1800, res=300)


# ocean mask

ocean <- get.Models3.variable(file=file.name, var="OCNMASK")
#                              ldatetime=ISOdatetime(year=2004, month=6, day=1, hour=0, min=0, sec=0, tz="GMT"),
#                              udatetime=ISOdatetime(year=2004, month=8, day=30, hour=23, min=0, sec=0, tz="GMT"))

col.rng <- c("white","grey")
z.rng <- c(0,1) #range(as.vector(season.sum.LNOx))
par(mar=c(3,3,5,4))
image(grid.data$x.cell.ctr, grid.data$y.cell.ctr, ocean$data[,,1],
      col=col.rng, breaks=c(0,0.5,1),zlim=z.rng,
      axes=F, xlab="", ylab="")
#      xlab="Projection x-coord (km)", ylab="Projection y-coord (km)")

title(main="continental mask")

lines(world.lines, col="black")
box()

vertical.image.legend(zlim=z.rng, col=col.rng, cex=1.2)


# LT ratio
LTratio <- get.Models3.variable(file=file.name, var="LTratio")
col.rng <- tim.colors(11)
z.rng <- 10^c(-3,2) # log scale range
par(mar=c(3,3,5,4))
image(grid.data$x.cell.ctr, grid.data$y.cell.ctr, LTratio$data[,,1],
      col=col.rng, breaks=10^c(seq(-3,2,by=0.5),10),zlim=z.rng,
      axes=F, xlab="", ylab="")
#      xlab="Projection x-coord (km)", ylab="Projection y-coord (km)")

title(main="lightning strike correction ratio")

lines(world.lines, col="black")
box()

vertical.image.legend(zlim=z.rng, col=col.rng, cex=0.8, log.scale=TRUE)


# CMAQ strikes
LTstrike <- get.Models3.variable(file=file.name, var="LTstrk")
print(dim(LTstrike$data))
col.rng <- tim.colors(11)
#z.rng <- 10^c(0,5) # log scale range

LTstrike.NA <- LTstrike$data
LTstrike.NA[which(LTstrike$data == 0)] <- NA
log.center <- floor(median(log10(LTstrike.NA), na.rm=T))

z.rng <- 10^c(log.center-2.5,log.center+2.5) # log scale range
par(mar=c(3,3,5,4))
image(grid.data$x.cell.ctr, grid.data$y.cell.ctr, LTstrike$data[,,1],
      col=col.rng,
      breaks=10^c(seq(log.center-2.5,log.center+2.5,by=0.5),10),
      zlim=z.rng,
      axes=F, xlab="", ylab="")
#      xlab="Projection x-coord (km)", ylab="Projection y-coord (km)")

title(main="CMAQ lightning strike density (km^-2 day^-1)")

lines(world.lines, col="black")
box()

vertical.image.legend(zlim=z.rng, col=col.rng, cex=0.8, log.scale=TRUE)


# NLDN strikes
NLDN <- get.Models3.variable(file=file.name, var="NLDNstrk")
print(dim(NLDN$data))

NLDN.NA <- NLDN$data
NLDN.NA[which(NLDN$data == 0)] <- NA
log.center <- floor(median(log10(NLDN.NA), na.rm=T))

col.rng <- tim.colors(11)
z.rng <- 10^c(log.center-2.5,log.center+2.5) # log scale range
par(mar=c(3,3,5,4))
image(grid.data$x.cell.ctr, grid.data$y.cell.ctr, NLDN$data[,,1],
      col=col.rng,
      breaks=10^c(seq(log.center-2.5,log.center+2.5,by=0.5),10),
      zlim=z.rng, axes=F, xlab="", ylab="")
#      xlab="Projection x-coord (km)", ylab="Projection y-coord (km)")

title(main="NLDN lightning strike density (km^-2 day^-1)")

lines(world.lines, col="black")
box()

vertical.image.legend(zlim=z.rng, col=col.rng, cex=0.8, log.scale=TRUE)


# strike difference
strike.diff <- (NLDN$data - (ocean$data * LTratio$data * LTstrike$data))*km.square*30
#strike.diff <- LTstrike$data - (ocean$data * LTratio$data * LTstrike$data)
col.rng <- tim.colors(42)
z.rng <- c(-2000,2000) # log scale range
par(mar=c(3,3,5,4))
image(grid.data$x.cell.ctr, grid.data$y.cell.ctr, strike.diff[,,1],
      col=col.rng, breaks=c(-1e10,seq(-2000,2000,by=100),1e10),zlim=z.rng,
      axes=F, xlab="", ylab="")
#      xlab="Projection x-coord (km)", ylab="Projection y-coord (km)")

title(main="NLDN strikes - CMAQ estimate")

lines(world.lines, col="black")
box()

vertical.image.legend(zlim=z.rng, col=col.rng, cex=0.8, log.scale=FALSE)



# ICCG
ICCG <- get.Models3.variable(file=file.name, var="ICCG")
col.rng <- tim.colors(11)
z.rng <- c(1,6) # log scale range
par(mar=c(3,3,5,4))
image(grid.data$x.cell.ctr, grid.data$y.cell.ctr, ICCG$data[,,1],
      col=col.rng, breaks=c(seq(1,6,by=0.5),10),zlim=z.rng,
      axes=F, xlab="", ylab="")
#      xlab="Projection x-coord (km)", ylab="Projection y-coord (km)")

title(main="inter-cloud to cloud to ground ratio")

lines(world.lines, col="black")
box()

vertical.image.legend(zlim=z.rng, col=col.rng, cex=1.2, log.scale=FALSE)



# moles N per flash
CMAQ <- get.Models3.variable(file=file.name, var="MOLSN")
print(dim(CMAQ$data))
col.rng <- tim.colors(11)
z.rng <- c(0,1000) 
par(mar=c(3,3,5,4))
image(grid.data$x.cell.ctr, grid.data$y.cell.ctr, CMAQ$data[,,1],
      col=col.rng, breaks=c(seq(0,1000,by=100),10e7),zlim=z.rng,
      axes=F, xlab="", ylab="")
#      xlab="Projection x-coord (km)", ylab="Projection y-coord (km)")

title(main="moles NO per flash")

lines(world.lines, col="black")
box()

vertical.image.legend(zlim=z.rng, col=col.rng, cex=1.2, log.scale=FALSE)


# strike count
CMAQ <- get.Models3.variable(file=file.name, var="STRKCNT")
print(dim(CMAQ$data))
col.rng <- tim.colors(11)
z.rng <- c(0,1000) 
par(mar=c(3,3,5,4))
image(grid.data$x.cell.ctr, grid.data$y.cell.ctr, CMAQ$data[,,1],
      col=col.rng, breaks=c(seq(0,1000,by=100),10e7),zlim=z.rng,
      axes=F, xlab="", ylab="")
#      xlab="Projection x-coord (km)", ylab="Projection y-coord (km)")

title(main="strike count parameter")

lines(world.lines, col="black")
box()

vertical.image.legend(zlim=z.rng, col=col.rng, cex=1.2, log.scale=FALSE)



dev.off()



