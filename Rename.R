
setwd("C:/Users/Shane/Dropbox/Results/R_Scripts/Renaming")
rnm=as.character(read.csv("22.1.16 Rename.csv",header=FALSE)$V1)
setwd("C:/Users/Shane/Desktop/TV6")
ob=list.files()[1:length(list.files())]




file.rename(ob,rnm)