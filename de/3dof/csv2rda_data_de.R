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

statdata=function(folder,result,algo){

  dircsv=paste(folder,"csv",result,sep="/")
  
  folderslist=dir(dircsv)
  dedir=grep(algo,folderslist)
  filesource <-c();
  for(i in dedir){
    tmppath = grep("csv",dir(paste(dircsv,folderslist[i],sep="/")),fixed=TRUE,value=TRUE)
    tmppath = paste(folderslist[i],tmppath,sep="/")
    filesource=c(filesource,tmppath)
  }
  stat_data_de<-c();
  for(i in 1:length(filesource)){
    tmprda=read_csv(paste(dircsv,filesource[i],sep="/"),col_names =F)
    stat_data_de=bind_rows(stat_data_de,tmprda)
  }
  stat_data_de
}

#Extract the values of H0L0
folder<-"np30ng100" #Set the folder name
result<-"results_labsin3_20200311" #Set the results folder

rdadir<-paste(folder,"rda",sep="/")
if(!dir.exists(rdadir)){
  dir.create(rdadir)
}
#All data for DE1
colnames<-c('alfa','beta','s1h0','tmin','nfo','time');

algo="de1"
data_de1<-statdata(folder,result,algo);
colnames(data_de1)<-colnames
save(data_de1,file=paste(rdadir,"data_de1.rda",sep="/"))

#All data for DE2
algo="de2"
data_de2<-statdata(folder,result,algo);
colnames(data_de2)<-colnames
save(data_de2,file=paste(rdadir,"data_de2.rda",sep="/"))


#All data for DE3
algo="de3"
data_de3<-statdata(folder,result,algo);
colnames(data_de3)<-colnames
save(data_de3,file=paste(rdadir,"data_de3.rda",sep="/"))

#All data for DE4
algo="de4"
data_de4<-statdata(folder,result,algo);
colnames(data_de4)<-colnames
save(data_de4,file=paste(rdadir,"data_de4.rda",sep="/"))
