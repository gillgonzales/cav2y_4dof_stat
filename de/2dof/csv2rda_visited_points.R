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
csvvisitpoint=function(folder,result,algo){
  filesource<-c();
  csvdir<-paste(folder,"csv",result,sep="/")
  algodir<-dir(csvdir);
  algoi<-grep(algo,algodir)
  csvfiledir<-paste(csvdir,algodir[algoi],sep="/")
  csvfilepath<-paste(csvfiledir,grep("opt",dir(csvfiledir),value=TRUE)[1],sep="/")
  filecsv<-grep("csv",dir(csvfilepath),value=TRUE)
  filesource<-paste(csvfilepath,filecsv,sep="/")
  filesource
}

#Extract the values of H0L0
folder<-"np30ng50" #Set the folder name
result<-"labsin5_20200320_np30_ng50" #Set the results folder

rdadir<-paste(folder,"rda",sep="/")
if(!dir.exists(rdadir)){
  dir.create(rdadir)
}
#All data for DE1
colnames<-c('alfa','beta','tmin');

algo="de1"
filesource<-csvvisitpoint(folder,result,algo)
v_points_de1<-read.csv(filesource,header = F,col.names=colnames)
save(v_points_de1,file=paste(rdadir,"v_points_de1.rda",sep="/"))

#All data for DE2
algo="de2"
filesource<-csvvisitpoint(folder,result,algo)
v_points_de2<-read.csv(filesource,header = F,col.names=colnames)
save(v_points_de2,file=paste(rdadir,"v_points_de2.rda",sep="/"))

#All data for DE3
algo="de3"
filesource<-csvvisitpoint(folder,result,algo)
v_points_de3<-read.csv(filesource,header = F,col.names=colnames)
save(v_points_de3,file=paste(rdadir,"v_points_de3.rda",sep="/"))

#All data for DE4
algo="de4"
filesource<-csvvisitpoint(folder,result,algo)
v_points_de4<-read.csv(filesource,header = F,col.names=colnames)
save(v_points_de4,file=paste(rdadir,"v_points_de4.rda",sep="/"))