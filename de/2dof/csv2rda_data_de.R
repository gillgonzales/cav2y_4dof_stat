library(readr)
library(dplyr)
library(reader)
library(tidyverse)
# The folder CSV must be create in the same directory of this file.
# The csv results files in CSV folder come from results repo at https://gitlab.com/g1ll/cav2yresults
# the csv files was not save in this repo to dont increase the repo size.
# Import statistical summary files and save as a compiled  Rda object to R scripts
# Results from: 
# https://gitlab.com/g1ll/cav2yresults/-/raw/master/de/4dof/h0l0_de_np30_ng100/h0l0_de_np30_ng100_d7559_20200312.tar.gz

#Function to get csv file source path
csvfilesource=function(folder,result,algo){
  filesource<-c();
  csvdir<-paste(folder,"csv",result,sep="/")
  algodir<-dir(csvdir);
  algoi<-grep(algo,algodir)
  csvfilepath<-paste(csvdir,algodir[algoi],sep="/")
  filecsv<-grep("csv",dir(csvfilepath),value=TRUE)
  filesource<-paste(csvfilepath,filecsv,sep="/")
  filesource
}

#Extract the values of H0L0
folder<-"np30ng50" #Set the folder name
result<-"2dof_de_d_np30ng50_20201002" #Set the results folder
type<-'d' #The type of CR and F parameters


rdadir<-paste(folder,"rda",sep="/")
if(!dir.exists(rdadir)){
  dir.create(rdadir)
}
#All data for DE1
colnames<-c('a','b','tmin','nfo','time');

algo="de1"
filesource<-csvfilesource(folder,result,algo)
data_de1<-read.csv(filesource,header = F,col.names=colnames)
save(data_de1,file=paste(rdadir,"/data_de1",type,".rda",sep=""))

#All data for DE2
algo="de2"
filesource<-csvfilesource(folder,result,algo)
data_de2<-read.csv(filesource,header = F,col.names=colnames)
save(data_de2,file=paste(rdadir,"/data_de2",type,".rda",sep=""))

#All data for DE3
algo="de3"
filesource<-csvfilesource(folder,result,algo)
data_de3<-read.csv(filesource,header = F,col.names=colnames)
save(data_de3,file=paste(rdadir,"/data_de3",type,".rda",sep=""))

#All data for DE4
algo="de4"
filesource<-csvfilesource(folder,result,algo)
data_de4<-read.csv(filesource,header = F,col.names=colnames)
save(data_de4,file=paste(rdadir,"/data_de4",type,".rda",sep=""))