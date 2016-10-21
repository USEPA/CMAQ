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


library("fields")

grid.data <- get.Models3.variable(file = file.name, ldatetime=get.datetime.seq(file.name)[1],
                                  udatetime=get.datetime.seq(file.name)[1],var="NLDNstrk")




#################################################
# plot maps of lightning NOx parameters
#################################################

# initialize map

world.lines <- get.map.lines.Models3.proj(file=file.name, region="world")

# CMAQ
png(file.img)
#tiff("~/tmp/summer_average_deposition_case.tiff", height=1800, width=1800, res=300)




# NLDN strikes
NLDN <- get.Models3.variable(file=file.name, var="NLDNstrk")
NLDN.NA <- NLDN$data
NLDN.NA[which(NLDN$data == 0)] <- NA
log.center <- floor(median(log10(NLDN.NA), na.rm=T))

print(dim(NLDN$data))
col.rng <- tim.colors(11)
z.rng <- 10^c(log.center-2.5,log.center+2.5) # log scale range
par(mar=c(3,1,5,6))
image(grid.data$x.cell.ctr, grid.data$y.cell.ctr, NLDN$data[,,1],
      col=col.rng,
      breaks=10^c(seq(log.center-2.5,log.center+2.5,by=0.5),10),
      zlim=z.rng,
      axes=F, xlab="", ylab="")
#      xlab="Projection x-coord (km)", ylab="Projection y-coord (km)")

title(main="NLDN lightning strikes (km^-2 day^-1)")

lines(world.lines, col="black")
box()

vertical.image.legend(zlim=z.rng, col=col.rng, cex=1.2, log.scale=TRUE)



dev.off()



