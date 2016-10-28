
#
# nohup R --vanilla < --args file.name="file.1" file.out="file.2" file.img="file.3" ocean_mask.R > & ocean_mask.dump &
#

#library(maps)

args <- commandArgs(TRUE)

file.name <- unlist(strsplit(args[1],"="))[2]
file.out <- unlist(strsplit(args[2],"="))[2]
file.img <- unlist(strsplit(args[3],"="))[2]

print(c(file.name, file.out, file.img))


