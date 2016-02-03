
#
# nohup R --vanilla < --args file.name="file.1" file.iccg="file.2" file.out="file.3" file.img="file.4" iccg.R > & iccg.dump &
#
oldLibPath <- .libPaths()
newLibPath <- c("/nas01/depts/ie/cempd/apps/R/64bit",oldLibPath) ## oldPath + additional paths
.libPaths(newLibPath)

args <- commandArgs(TRUE)

file.name <- unlist(strsplit(args[1],"="))[2]
file.iccg <- unlist(strsplit(args[2],"="))[2]
file.out <- unlist(strsplit(args[3],"="))[2]
file.img <- unlist(strsplit(args[4],"="))[2]
file.Rscript <- unlist(strsplit(args[5],"="))[2]

print(args)
print(c(file.name, file.iccg, file.out, file.img, file.Rscript))


library(maps)



source(paste(file.Rscript,"/func_for_CMAQ_files_with_ncdf_rgdal.r", sep=""))
source(paste(file.Rscript,"/vertical_image_legend.R", sep=""))
source(paste(file.Rscript,"/draw_color_scatterplot_w_geoR.r", sep=""))
source(paste(file.Rscript,"/dapply.R", sep=""))



########################################
# open CMAQ meteorology files
########################################


grid.data <- get.Models3.variable(file = file.name, ldatetime=get.datetime.seq(file.name)[1],
                                  udatetime=get.datetime.seq(file.name)[1],var="RC")


grids.from.land <- ceiling(100 / (grid.data$x.cell.ctr[2] - grid.data$x.cell.ctr[1]))
res <- (grid.data$x.cell.ctr[2] - grid.data$x.cell.ctr[1])
n.col <- dim(grid.data$data)[1]
n.row <- dim(grid.data$data)[2]


iccg.latlon <- read.csv(file.iccg, header=T)
iccg.latlon$row <- rep(NA,length(iccg.latlon$lon))
iccg.latlon$col <- rep(NA,length(iccg.latlon$lon))


for(lat in seq(min(iccg.latlon$lat),max(iccg.latlon$lat),by=0.5)) {
  for(lon in seq(min(iccg.latlon$lon),max(iccg.latlon$lon),by=0.5)) {
    
    cr <- proj.lonlat.to.Models3(lon,lat,file.name)
    iccg.latlon$col[which((iccg.latlon$lat == lat) & (iccg.latlon$lon == lon))] <- cr[1]
    iccg.latlon$row[which((iccg.latlon$lat == lat) & (iccg.latlon$lon == lon))] <- cr[2]    
  }
}

col.grid <- floor(iccg.latlon$col/res + n.col/2)
row.grid <- floor(iccg.latlon$row/res + n.row/2)
iccg.latlon$col <- col.grid
iccg.latlon$row <- row.grid

iccg.sum <- matrix(0,nrow=n.row, ncol=n.col)
iccg.count <- matrix(0,nrow=n.row, ncol=n.col)
iccg.interpolate <- matrix(0,nrow=n.row, ncol=n.col)


for(lat in seq(min(iccg.latlon$lat)+0.5,max(iccg.latlon$lat)-0.5,by=0.5)) {
  for(lon in seq(min(iccg.latlon$lon)+0.5,max(iccg.latlon$lon)-0.5,by=0.5)) {

    val <- iccg.latlon$iccg[which((iccg.latlon$lat == lat) & (iccg.latlon$lon == lon))]
    row <- iccg.latlon$row[which((iccg.latlon$lat == lat) & (iccg.latlon$lon == lon))]
    col <- iccg.latlon$col[which((iccg.latlon$lat == lat) & (iccg.latlon$lon == lon))]
    if ((row > 0) & (row <= n.row) & (col > 0) & (col <= n.col)) {
    
      upper.left.row <- min(n.row,iccg.latlon$row[which((iccg.latlon$lat == (lat + 0.5)) & (iccg.latlon$lon == (lon - 0.5)))])
      upper.left.col <- max(1,iccg.latlon$col[which((iccg.latlon$lat == (lat + 0.5)) & (iccg.latlon$lon == (lon - 0.5)))])

      lower.right.row <- max(1,iccg.latlon$row[which((iccg.latlon$lat == (lat - 0.5)) & (iccg.latlon$lon == (lon + 0.5)))])
      lower.right.col <- min(n.col,iccg.latlon$col[which((iccg.latlon$lat == (lat - 0.5)) & (iccg.latlon$lon == (lon + 0.5)))])
    
      for (col in seq(upper.left.col, lower.right.col)) {
        for(row in seq(lower.right.row,upper.left.row)) {
        
        
          iccg.sum[row,col] <- iccg.sum[row,col] + val
          iccg.count[row,col] <- iccg.count[row,col] + 1
        }
      }
    }
  }
}
  

# get color range
library("fields")
col.rng <- rainbow(12)
detach("package:fields")


a <- which(iccg.count == 0)
iccg.count[a] <- 1
iccg.sum[a] <- 2.9
iccg.interpolate <- iccg.sum / iccg.count

z.count.rng <- range(iccg.count) #range(as.vector(season.sum.LNOx))
z.interpolate.rng <- range(iccg.interpolate)

#################################################
# plot maps of CMAQ and SCIAMACHY column density
#################################################

# initialize map

world.lines <- get.map.lines.Models3.proj(file=file.name, region="world")

# CMAQ
png("~/tmp/plot_iccg_count.png", height=600, width=600)
#tiff("~/tmp/summer_average_deposition_case.tiff", height=1800, width=1800, res=300)
par(mar=c(3,3,5,4))
image(grid.data$x.cell.ctr, grid.data$y.cell.ctr, t(iccg.count),
      col=col.rng, zlim=z.count.rng,
      axes=F, xlab="", ylab="")
#      xlab="Projection x-coord (km)", ylab="Projection y-coord (km)")

title(main="number of grid cells included in average")

lines(world.lines, col="black")
box()

vertical.image.legend(zlim=z.count.rng, col=col.rng, cex=1.5)

dev.off()



#png(file.img, height=600, width=600)
#tiff("~/tmp/summer_average_deposition_case.tiff", height=1800, width=1800, res=300)
par(mar=c(3,3,5,4))
image(grid.data$x.cell.ctr, grid.data$y.cell.ctr, t(iccg.interpolate),
      col=col.rng, zlim=z.interpolate.rng,
      axes=F, xlab="", ylab="")
#      xlab="Projection x-coord (km)", ylab="Projection y-coord (km)")

title(main="ic:cg interpolate")

lines(world.lines, col="black")
box()

vertical.image.legend(zlim=z.interpolate.rng, col=col.rng, cex=1.5)

dev.off()



write.table(file=file.out,as.data.frame(iccg.interpolate), sep=",",row.names=F,col.names=F)
