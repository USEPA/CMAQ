
library(maps)
png("canada_regions.png")
map.text("world", regions="canada",cex=1)
dev.off()

