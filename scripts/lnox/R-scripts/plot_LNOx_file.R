library(maps)



file.name <- commandArgs(TRUE)[1]
layer <- as.numeric(commandArgs(TRUE)[2])
hour <- as.numeric(commandArgs(TRUE)[3])
print(file.name)
print(layer)
print(hour)


# make spatial plots of CMAQ and SCIAMACHY results


source("/home/swall/R_examples/func_for_CMAQ_files_with_ncdf_rgdal.r")
#source("/home/swall/R_examples/vertical_image_legend.r")
source("/project/pinder/lightningNOx/process_results/NADP_wetdep/vertical_image_legend.R")
source("/home/swall/R_examples/draw_color_scatterplot_w_geoR.r")
source("/project/pinder/sat_NH3/calcTESprofile/dapply.R")

########################################
# open CMAQ deposition files, for this case
########################################

#file.case <- paste("paired_results/sat_mod.",case,".2004.ncf",sep="")
#CMAQ.case <- get.Models3.variable(file=file.case, var="CMAQ")
#SCIA <- get.Models3.variable(file=file.case, var="SCIAMACHY")
#file.name <- "/project/pinder/lightningNOx/met/layer1/met.2004.07.ioapi"
#file.name <- "project/pinder/lightningNOx/emissions/results/LTNG_RATIO.2004.06.ioapi"
grid.data <- get.Models3.variable(file = file.name,
#                                  ldatetime=get.datetime.seq(file.name)[1],
#                                  udatetime=get.datetime.seq(file.name)[1],
                                  var="NO")

print(dim(grid.data$data))
library("fields")




#################################################
# plot maps of lightning NOx parameters
#################################################

# initialize map

world.lines <- get.map.lines.Models3.proj(file=file.name, region="world")

# CMAQ
#pdf("~/tmp/plot_LNOx_file.pdf")
tiff("~/tmp/plot_LNOx_file.png", height=600, width=600)


# ocean mask


#                              ldatetime=ISOdatetime(year=2004, month=6, day=1, hour=0, min=0, sec=0, tz="GMT"),
#                              udatetime=ISOdatetime(year=2004, month=8, day=30, hour=23, min=0, sec=0, tz="GMT"))

z.rng <- c(0,max(grid.data$data[,,layer,hour]))
print(z.rng)

      
col.rng <- tim.colors(11)

par(mar=c(3,3,5,4))
image(grid.data$x.cell.ctr, grid.data$y.cell.ctr, grid.data$data[,,layer,hour],
      col=col.rng,zlim=z.rng,
      axes=F, xlab="", ylab="")
#      xlab="Projection x-coord (km)", ylab="Projection y-coord (km)")

title(main="LNOx emissions, mol/sec")

lines(world.lines, col="black")
box()

vertical.image.legend(zlim=z.rng, col=col.rng, cex=1.2)


dev.off()
