### Welcome to the Wiggle Script
### Remember to have your wiggle videos named in the following way
### (Genotype).(Dose).(Time).(plate).(Pesticide).(Date).MTS
### Example: (w1118).(48ppm).(15min).(rep1).(Imidacloprid).(23.10.14).MTS
### Any questions (lord knows this isn't explained well) contact the author at shanedenecke@gmail.com. Promise I won't be a jerk


### 1) Import all of your libraries that you will need in this script
{
library(svDialogs)
library(plyr)
library(ggplot2)
require(data.table)
library(colorspace)
library(cowplot)
library(RColorBrewer)
}

### 2) Get some basic info and consolidate the raw output of the imageJ script into one "output" file

{
#Choose file where your data is and set your analysis parameters
dlgMessage(message="Holy Wiggle Batman \n Let's find out what you want to look at today",type="ok")
mainDir=dirname(file.choose())
setwd(mainDir)
Graph_Title=dlgInput(message="Please Enter A Name for Your Experiment")$res
cntrl_Line=dlgInput(message="Please Enter Which Genotype is Your Control")$res
Compare=dlgList(title="What Kind of analysis do you want to do \n (See Denecke et. al 2015)",choices=c("RMR","Raw","GLM"),multiple=T)$res
Length=dlgList(title="What Time Interval Would You Like To Analyze \n Choose 'All' if you don't know what this means",choices=c("Front","Back","All"),multiple=T)$res

# Use parameters to create raw data file and graphing directory
Raw=data.frame()
for (a in list.files()){
  if (grepl(".xls",a)){
  table=read.table(a,sep="\t",header=T) 
  trim=cbind(as.character(table$Image.Name),table$Wiggle.Index) 
  Raw=rbind(Raw,trim)
  } 
}
colnames(Raw)=c("Image.Name","Wiggle.Index")
GraphDir=paste(getwd(),"/",Graph_Title,"_Graph",sep="")
dir.create(GraphDir)
}

###Let's get a workable Data frame from your raw Data File
{          
SPLIT_Frame=matrix(unlist(strsplit(as.character(Raw$Image.Name),"\\).")),ncol=8,byrow=T)[,c(1:6,8)]
SPLIT_Frame=gsub("\\(","",SPLIT_Frame)
SPLIT_Frame=gsub(".tif","",SPLIT_Frame)
SPLIT_Frame=gsub("opRight","TopRight",SPLIT_Frame)
SPLIT_Frame=gsub("opLeft","TopLeft",SPLIT_Frame)
SPLIT_Frame=gsub("owRight","LowRight",SPLIT_Frame)
SPLIT_Frame=gsub("owLeft","LowLeft",SPLIT_Frame)
WI_Frame=cbind(SPLIT_Frame,as.numeric(as.character(Raw$Wiggle.Index)))
colnames(WI_Frame)=c("Genotype","Dose","Time","Plate","Insecticide","Date","Position","Wiggle_Index")
WI_Frame[,'Time']=as.numeric(gsub('min',"",WI_Frame[,'Time'])) 		  
WI_Frame=data.frame(WI_Frame)  
} 
###Prep and Summarize Raw Data For RMR Analysis
{
Corrected_Data=data.frame()
for (a in as.character(unique(WI_Frame$Date))){; for (b in as.character(unique(WI_Frame$Dose))){; for(c in as.character(unique(WI_Frame$Genotype))){; for (d in as.character(unique(WI_Frame$Plate))){; for (e in as.character(unique(WI_Frame$Position))){
	for (f in as.character(unique(WI_Frame$Insecticide))){
		sub=subset(WI_Frame,Date==a & Dose==b & Genotype==c & Plate==d & Position==e & Insecticide==f)
		if(length(sub$Wiggle_Index)>0){
		  EV=c()
		 for (i in 1:length(sub$Time)){
		   x.time=as.numeric(as.character(sub$Time[i]))
		  rat=as.numeric(as.character(sub$Wiggle_Index[which(sub$Time==x.time)]))/as.numeric(as.character(sub$Wiggle_Index[which(sub$Time==0)]))
				EV[i]=rat
			}
			sub$Modified=EV
			Corrected_Data=rbind(Corrected_Data,sub)
		}else{
		print("Such is Life")
		}}
	}}}}}
}
write.csv(Corrected_Data,file="Inidvidual_Well_Data.csv")
### Summarize all of your data for RMR value plots
{
Corrected_Data$Wiggle_Index=as.numeric(as.character(Corrected_Data$Wiggle_Index))
Summary_RMR=ddply(Corrected_Data, c("Dose","Genotype","Insecticide","Time"), summarise,
                  N    = length(Wiggle_Index),
                  Mean_Rel = mean(Modified),
                  Sd_Rel   = sd(Modified),
                  Se_Rel   = Sd_Rel/sqrt(N),
                  CI_Rel   = qnorm(0.975)*Se_Rel,
                  Mean_Raw = mean(Wiggle_Index),
                  Sd_Raw   = sd(Wiggle_Index),
                  Se_Raw   = Sd_Raw/sqrt(N),
                  CI_Raw   = qnorm(0.975)*Se_Raw)
Summary_RMR=data.table(Summary_RMR,key="Dose")
Summary_RMR=data.frame(Summary_RMR)
}
### Prep Raw Data For GLM analysis
{
Sliced_Data=as.matrix(Corrected_Data)
Sliced_Data[,"Time"]=as.numeric(Sliced_Data[,"Time"]) 
rownames(Sliced_Data)=paste(Sliced_Data[,1],Sliced_Data[,2],Sliced_Data[,4],Sliced_Data[,5],Sliced_Data[,6],Sliced_Data[,7],sep="_")
Time_pts=unique(as.numeric(Sliced_Data[,"Time"]))
Time_pts=Time_pts[order(Time_pts)]
Columns=c("Genotype","Dose","Plate","Insecticide","Date","Position",Time_pts)
Frame_By_Time=matrix(ncol=length(Columns))
colnames(Frame_By_Time)=Columns
for (i in unique(rownames(Sliced_Data))) {
	Run=Sliced_Data[which(rownames(Sliced_Data)==i),]
	Run=Run[order(as.numeric(Run[,"Time"])),]
	submat=matrix(ncol=length(Columns),nrow=1)
	rownames(submat)=i
	colnames(submat)=Columns
	submat[1,]=as.vector(c(Run[1,'Genotype'],Run[1,'Dose'],Run[1,'Plate'],Run[1,'Insecticide'],Run[1,'Date'],Run[1,'Position'],Run[,'Wiggle_Index']))
	Frame_By_Time=rbind(Frame_By_Time,submat)
	}
Frame_By_Time=Frame_By_Time[-1,]
}
### Create all plot directories and set plotting variables
{
setwd(GraphDir)
ex=expand.grid(Compare,Length)
	Plot_List=c()
for (i in 1:length(ex$Var1)){
	v=c()
	for (b in ex[i,]){v=paste(v,b,sep="_")}
	Plot_List=c(Plot_List,v)}	
for (p in Plot_List){
	if ((grepl("Relative",p) & grepl("GLM",p))) {
	Plot_List=Plot_List[!Plot_List %in% p]
	}
	}	
sapply(Plot_List,dir.create)
Color=colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan","#7FFF7F", "#FF7F00", "red", "#7F0000"))
#Color=c("purple","orange")
#Color=c(rev(brewer.pal(length(unique(Summary_RMR$Dose))-1,"Spectral")))
	Genotypes=unique(as.character(Summary_RMR$Genotype))
	Plot_Base_Genotype=c(Genotypes[Genotypes %in% cntrl_Line],Genotypes[!Genotypes %in% cntrl_Line])
	Color_Base_Genotype=c("black",Color(length(Plot_Base_Genotype)-1))
	#Color_Base_Genotype=c("black",Color[1:(length(Plot_Base_Genotype))-1])
	Shape_Base_Genotype=c(0,rep(1,length(Plot_Base_Genotype)-1))
	Line_Base_Genotype=c(2,rep(1,length(Plot_Base_Genotype)-1))
	names(Color_Base_Genotype)=Plot_Base_Genotype
	names(Line_Base_Genotype)=Plot_Base_Genotype
	names(Shape_Base_Genotype)=Plot_Base_Genotype
}			  
### Now Run the For Loop That Provides Graphs and Analysis for Each Desired Graph
for (IP in Plot_List){
	setwd(file.path(GraphDir,IP))
	### This series of control flow statements will get variables needed to plot your specific graph
	if (grepl("Front",IP)){
		Plot_Time=Time_pts[which(Time_pts<=30)]
		}else if(grepl("Back",IP)){
			Plot_Time=c(0,Time_pts[which(Time_pts>=30)])
		}else if(grepl("All",IP)){
			Plot_Time=Time_pts}
	if((grepl("RMR",IP) | grepl("Raw",IP))){
		Plot_Data=Summary_RMR[which(Summary_RMR[,"Time"]%in%(Plot_Time)),]
		Color_Genotype=Color_Base_Genotype[as.character(unique(Plot_Data$Genotype))]
		Shape_Genotype=Shape_Base_Genotype[as.character(unique(Plot_Data$Genotype))]
		Line_Genotype=Line_Base_Genotype[as.character(unique(Plot_Data$Genotype))]
	}
	if((grepl("GLM",IP))){
    GLM_Plot_Data=Frame_By_Time[,c("Genotype","Dose","Plate","Insecticide","Insecticide","Date","Position",Plot_Time)]
	}
	### These control flow statements will plot your graphs based on the parameters you chose in the beginning
	if((grepl("RMR",IP))){
		Plot_Data2=subset(Plot_Data, select=c("Dose","Genotype","Time","Insecticide","Mean_Rel","CI_Rel"))
		for (b in unique(Plot_Data$Insecticide)){;for (c in unique(Plot_Data$Dose)){
			Plot_Data3=subset(Plot_Data2,Insecticide==b & Dose==c)
			if (length(rownames(Plot_Data3)>1)){
				Plot_Data3$Time=as.numeric(as.character(Plot_Data3$Time))
				write.csv(Plot_Data3,file=paste("Dose_Genotype_Plot Data",b,c,".csv"))
				x=ggplot(Plot_Data3,aes_string(x="Time",y="Mean_Rel",colour="Genotype")) +
				geom_errorbar(aes(ymin=Mean_Rel-CI_Rel, ymax=Mean_Rel+CI_Rel), width=max(as.numeric(unique(Plot_Data3$Time)))/30,size=.5) +
				geom_line(size=.9,aes_string(linetype="Genotype")) +
				geom_point(size=3,fill=1,aes_string(shape="Genotype")) +
				scale_colour_manual(values=Color_Genotype) +
				scale_shape_manual(values=Shape_Genotype) +
				scale_linetype_manual(values=Line_Genotype) +
				ylim(0,max(Plot_Data3$Mean_Rel+Plot_Data3$CI_Rel)) +
				xlab("Time (min)") +
				ylab(paste("RMR Value")) +
				theme(text=element_text(size=10,face="bold",family="Times"),plot.title=element_text(size=16,face="bold",family="Times"),legend.title=element_text(size=12,face="bold",family="Times"))+
				background_grid(major = "xy", minor = "none",colour.major="grey75")+
				ggtitle(paste(c," ",b,sep=""))
              }
				save_plot(filename=paste(c,"_",b," RMR.pdf",sep=""),plot=x,base_aspect_ratio = 1.3)
	}}}
	if((grepl("Raw",IP))){
		Plot_Data2=subset(Plot_Data, select=c("Dose","Genotype","Time","Insecticide","Mean_Raw","CI_Raw"))
		for (b in unique(Plot_Data$Insecticide)){;for (c in unique(Plot_Data$Dose)){
			Plot_Data3=subset(Plot_Data2,Insecticide==b & Dose==c)
			if (length(rownames(Plot_Data3)>1)){
				Plot_Data3$Time=as.numeric(as.character(Plot_Data3$Time))
				write.csv(Plot_Data3,file=paste("Dose_Genotype_Plot Data",b,c,".csv"))
				x=ggplot(Plot_Data3,aes_string(x="Time",y="Mean_Raw",colour="Genotype")) +
				geom_errorbar(aes(ymin=Mean_Raw-CI_Raw, ymax=Mean_Raw+CI_Raw), width=max(as.numeric(unique(Plot_Data3$Time)))/30,size=.5) +
				geom_line(size=.9,aes_string(linetype="Genotype")) +
				geom_point(size=3,fill=1,aes_string(shape="Genotype")) +
				scale_colour_manual(values=Color_Genotype) +
				scale_shape_manual(values=Shape_Genotype) +
				scale_linetype_manual(values=Line_Genotype) +
				ylim(0,max(Plot_Data3$Mean_Raw+Plot_Data3$CI_Raw)) +
				xlab("Time (min)") +
				ylab(paste("RMR Value")) +
				theme(text=element_text(size=10,face="bold",family="Times"),plot.title=element_text(size=16,face="bold",family="Times"),legend.title=element_text(size=12,face="bold",family="Times"))+
				background_grid(major = "xy", minor = "none",colour.major="grey75")+
				ggtitle(paste(c," ",b,sep=""))
            }
        save_plot(filename=paste(c,"_",b," Raw.pdf",sep=""),plot=x,base_aspect_ratio = 1.3)
    }}}
	if((grepl("GLM",IP))){
		Out=vector(lengt=4)
		for (i in 1:nrow(GLM_Plot_Data)) {
			test.raw=summary(lm(GLM_Plot_Data[i,8:ncol(GLM_Plot_Data)]~Plot_Time))
			test.xlog=summary(lm(GLM_Plot_Data[i,8:ncol(GLM_Plot_Data)]~log(1+Plot_Time)))
			test.xylog=summary(lm(log(as.numeric(GLM_Plot_Data[i,8:ncol(GLM_Plot_Data)]))~log(1+Plot_Time)))
			R.sq=c(raw=test.raw$adj.r.squared,xlog=test.xlog$adj.r.squared,xylog=test.xylog$adj.r.squared)
			#Intercept=c(rawI=test.raw$coefficient[1,1],xlogI=test.xlog$coefficient[1,1],xylogI=test.xylog$coefficient[1,1])
			Betas=c(raw=test.raw$coefficient[2,1],xlog=test.xlog$coefficient[2,1],xylog=test.xylog$coefficient[2,1])
			Out=rbind(Out,c(names(R.sq)[R.sq==max(R.sq)],Betas))
		}
		Out=Out[-1,]
		table(Out[,1])
		Trans=names(table(Out[,1]))[table(Out[,1])==max(table(Out[,1]))]
		wiggle_beta=as.numeric(Out[,Trans])
		GLM_Data=data.frame(cbind(GLM_Plot_Data[,"Genotype"],GLM_Plot_Data[,"Dose"],GLM_Plot_Data[,"Insecticide"],wiggle_beta=(wiggle_beta)))
		GLM_Data=data.frame(GLM_Data)
		colnames(GLM_Data)=c("Genotype","Dose","Insecticide","wiggle_beta")
		for (I in (as.character(unique(GLM_Data$Insecticide)))){; for (D in (as.character(unique(GLM_Data$Dose)))){
			if (length(subset(GLM_Data,Insecticide==I & Dose==D)$Genotype)>1){
		    GLM_Data2=subset(GLM_Data,Insecticide==I & Dose==D)
		  	lim=max(-as.numeric(as.character(GLM_Data2$wiggle_beta)))+(max(as.numeric(as.character(GLM_Data2$wiggle_beta))))/7
		  	GLM_Data2=data.frame(cbind(as.character(GLM_Data2$Genotype),as.numeric(as.character(GLM_Data2$wiggle_beta))))
		  	colnames(GLM_Data2)=c("Genotype","wiggle_beta")
		  	GLM_Data2$Genotype=as.character(GLM_Data2$Genotype)
			  GLM_Data2$wiggle_beta=as.numeric(as.character(GLM_Data2$wiggle_beta))
			  write.csv(GLM_Data2,file=paste("GLM_Data2",D,I,".csv"))
		  	GLM_Summary=ddply(GLM_Data2, "Genotype", summarise,
              N    = length(wiggle_beta),
              Mean = mean(wiggle_beta),
              Sd   = sd(wiggle_beta),
              Se   = Sd/sqrt(N),
              CI   = qnorm(0.975)*Se)
			  GLM_Summary$Mean=-GLM_Summary$Mean
			  write.csv(GLM_Summary,file=paste("GLM_Summary",D,I,".csv"))
		  	x=ggplot(data=GLM_Summary, aes(x=Genotype, y=Mean,fill=Genotype)) +
			  geom_errorbar(aes(ymin=Mean,ymax=Mean+CI), width=.5,size=.5,position=position_dodge()) +
			  geom_bar(stat="identity") +
		  	geom_bar(stat="identity",color="black", show.legend=FALSE) +
			  xlab("Genotypes") +
		  	ylab(expression(bold(paste("-",beta," values",sep="")))) +
			  scale_fill_manual(values=c(Color_Base_Genotype)) +
			  theme(text=element_text(face="bold",family="Times")) +
			  theme(axis.text.x=element_text(size=8,face="bold",family="Times")) +
		  	theme(text=element_text(size=10,face="bold",family="Times"),plot.title=element_text(size=16,face="bold",family="Times"),legend.title=element_text(size=12,face="bold",family="Times"))+
		  	background_grid(major = "xy", minor = "none",colour.major="grey75")+	
		  	ylim(0,lim) +
		  	ggtitle(paste(D," ",I,sep=""))
		  	save_plot(filename=paste(D,I,"GLM.pdf",sep=" "),plot=x)	
			}
		  	}}
	}

}