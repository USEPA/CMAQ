
#
# nohup R --vanilla < --args file.name="file.1" file.out="file.2" file.img="file.3" ocean_mask.R > & ocean_mask.dump &
#

#library(maps)
## R Library path
oldLibPath <- .libPaths()
newLibPath <- c("/nas01/depts/ie/cempd/apps/R/64bit",oldLibPath) ## oldPath + additional paths
.libPaths(newLibPath)


args <- commandArgs(TRUE)

file.name <- unlist(strsplit(args[1],"="))[2]
file.out <- unlist(strsplit(args[2],"="))[2]
file.img <- unlist(strsplit(args[3],"="))[2]
file.Rscript <- unlist(strsplit(args[4],"="))[2]

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
require("ncdf")
nc <- open.ncdf(file.name)
var.name <- names(nc$var)[2]
nc <- close.ncdf(nc)



grid.data <- get.Models3.variable(file = file.name,
                                  ldatetime=get.datetime.seq(file.name)[1],
                                  udatetime=get.datetime.seq(file.name)[1],
                                  var=var.name)


grids.from.land <- ceiling(100 / (grid.data$x.cell.ctr[2] - grid.data$x.cell.ctr[1]))
res <- (grid.data$x.cell.ctr[2] - grid.data$x.cell.ctr[1])
n.col <- dim(grid.data$data)[1]
n.row <- dim(grid.data$data)[2]

mask <- matrix(1,nrow=n.row, ncol=n.col)

# figure out a way to do this in batch
for (col in 1:n.col) {
  for(row in 1:n.row) {
    if (is.na(map.where(database="world",
                        proj.Models3.to.lonlat(grid.data$x.cell.ctr[col],
                                               grid.data$y.cell.ctr[row],file.name))))
      mask[row,col] <- 0
    
  }
  print(paste(col,"of",n.col,sep=" "))
}

weight <- matrix(0,nrow=n.row, ncol=n.col)
for (col in 1:n.col) {
  for(row in 1:n.row) {
    col.range <- seq(max(c(1,(col-grids.from.land))), min(c(n.col,col+grids.from.land)))
    row.range <- seq(max(c(1,(row-grids.from.land))), min(c(n.row,row+grids.from.land)))
    contains.land <- sum(mask[row.range,col.range])
    if (contains.land > 0)
      weight[row,col] <- 1
    else
      weight[row,col] <- 0.2
    
    
  }
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
image(grid.data$x.cell.ctr, grid.data$y.cell.ctr, t(weight),
      col=col.rng, breaks=c(0,0.5,1),zlim=z.rng,
      axes=F, xlab="", ylab="")
#      xlab="Projection x-coord (km)", ylab="Projection y-coord (km)")

title(main="continental mask")

lines(world.lines, col="black")
box()

vertical.image.legend(zlim=z.rng, col=col.rng, cex=1.5)

dev.off()

write.table(file=file.out,as.data.frame(weight), sep=",",row.names=F,col.names=F)
