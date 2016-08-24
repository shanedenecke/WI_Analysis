
setwd("/home/sdenecke/Dropbox/Results/R_scripts/Renaming")
rnm=as.character(read.csv("23.8.16.Rename.csv",header=FALSE)$V1)
setwd("/home/sdenecke/Documents/Temp_Videos_2")
ob=list.files()[1:length(list.files())]




file.rename(ob,rnm)
