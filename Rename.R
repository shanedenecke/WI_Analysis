setwd("/home/sdenecke/Dropbox/Results/R_scripts/Renaming")
rnm=as.character(read.csv("23.11.16.Rename.csv",header=FALSE)$V1)
setwd("/run/media/sdenecke/9435-A8B1/PRIVATE/AVCHD/BDMV/STREAM/Shane")
ob=list.files()[1:length(list.files())]




file.rename(ob,rnm)
