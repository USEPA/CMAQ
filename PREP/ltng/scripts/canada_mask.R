
#
# nohup R --vanilla < --args file.name="file.1" file.out="file.2" file.img="file.3" ocean_mask.R > & ocean_mask.dump &
#

#library(maps)

#args <- commandArgs(TRUE)

file.name <- "/project/pinder/lightningNOx/emissions/src/base//mcip/met.20060101.ioapi"
file.out <- "/project/pinder/lightningNOx/emissions/src/base//R-out/canada_mask.csv"
file.img <- "/project/pinder/lightningNOx/emissions/src/base//R-out/canada_mask.png"
file.Rscript <- "/project/pinder/lightningNOx/emissions/src/base/R-scripts"

print(args)
print(c(file.name, file.out, file.img))


library(maps)


source(paste(file.Rscript,"/func_for_CMAQ_files_with_ncdf_rgdal.r", sep=""))
source(paste(file.Rscript,"/vertical_image_legend.R", sep=""))
source(paste(file.Rscript,"/draw_color_scatterplot_w_geoR.r", sep=""))
source(paste(file.Rscript,"/dapply.R", sep=""))

########################################
# open CMAQ grid files, for this case
########################################

grid.data <- get.Models3.variable(file = file.name,
                                  ldatetime=get.datetime.seq(file.name)[1],
                                  udatetime=get.datetime.seq(file.name)[1],
                                  var="TEMP2")


grids.from.land <- ceiling(100 / (grid.data$x.cell.ctr[2] - grid.data$x.cell.ctr[1]))
res <- (grid.data$x.cell.ctr[2] - grid.data$x.cell.ctr[1])
n.col <- dim(grid.data$data)[1]
n.row <- dim(grid.data$data)[2]

mask <- matrix(0,nrow=n.row, ncol=n.col)

# figure out a way to do this in batch
for (col in 1:n.col) {
  for(row in 1:n.row) {
    place <- (map.where(database="world",
                        proj.Models3.to.lonlat(grid.data$x.cell.ctr[col],
                                               grid.data$y.cell.ctr[row],file.name)))
    if ((!(is.na(place))) & (length(grep("Canada", place)) > 0))
      mask[row,col] <- 1
    
  }
  print(paste(col,"of",n.col,sep=" "))
}




# get color range
library("fields")
col.rng <- c("white","grey")
detach("package:fields")

z.rng <- c(0,1) 

#################################################
# plot maps of continental ocean mask
#################################################

# initialize map

world.lines <- get.map.lines.Models3.proj(file=file.name, region="world")

# CMAQ
png(file.img, height=600, width=600)

par(mar=c(3,3,5,4))
image(grid.data$x.cell.ctr, grid.data$y.cell.ctr, t(mask),
      col=col.rng, breaks=c(0,0.5,1),zlim=z.rng,
      axes=F, xlab="", ylab="")
#      xlab="Projection x-coord (km)", ylab="Projection y-coord (km)")

title(main="Canada Mask")

lines(world.lines, col="black")
box()

vertical.image.legend(zlim=z.rng, col=col.rng, cex=1.5)

dev.off()

write.table(file=file.out,as.data.frame(mask), sep=",",row.names=F,col.names=F)
