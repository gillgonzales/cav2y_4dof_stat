library(readr)
library(dplyr)
library(reader)
# The folder data must be create in the same directory of this file.
# The csv results files in data folder come from results repo at https://gitlab.com/g1ll/cav2yresults
# the csv files was not save in this repo to dont increase the repo size.
# Import statistical summary files and save as a compiled  Rda object to R scripts
# Results from: 
# https://gitlab.com/g1ll/cav2yresults/-/raw/master/de/4dof/h0l0_de_np30_ng100/h0l0_de_np30_ng100_d7559_20200312.tar.gz
#Function to find and import CSV files for defined algorithm and folders
statdata=function(folder,result,algo){
  dircsv=paste(folder,"csv",result,sep="/")
  folderslist=dir(dircsv)
  d1i=grep(algo,folderslist)
  filesource <-c();
  for(i in d1i){
    csv_i=grep(algo,dir(paste(dircsv,folderslist[i],sep="/")))
    tmppath =dir(paste(dircsv,folderslist[i],sep="/"))[csv_i]
    tmppath = paste(folderslist[i],tmppath,sep="/")
    filesource=c(filesource,tmppath)
  }
  stat_data_de<-c();
  for(i in 1:length(filesource)){
    tmprda=read_csv(paste(dircsv,filesource[i],sep="/"))
    stat_data_de=bind_rows(stat_data_de,tmprda)
  }
  stat_data_de
}

folder<-"np40ng75" #Set the folder name
result<-"h0l0_de_np40_ng75_labsin3_labsin5_20200401" #Set the results folder

rdadir<-paste(folder,"rda",sep="/")
if(!dir.exists(rdadir)){
  dir.create(rdadir)
}

#Save RDAs files for all DE version
stat_data_de1<-statdata(folder,result,algo="de1");
stat_data_de1<-arrange(stat_data_de1,h0l0)
#Save Rda file with statisctal data from DE1 algorithm for np30ng100
save(stat_data_de1,file=paste(rdadir,"stat_data_de1.rda",sep="/"))

stat_data_de2<-statdata(folder,result,algo="de2");
stat_data_de2<-arrange(stat_data_de2,h0l0)
#Save Rda file with statisctal data from DE2 algorithm for np30ng100
save(stat_data_de2,file=paste(rdadir,"stat_data_de2.rda",sep="/"))

stat_data_de3<-statdata(folder,result,algo="de3");
stat_data_de3<-arrange(stat_data_de3,h0l0)
#Save Rda file with statisctal data from DE3 algorithm for np30ng100
save(stat_data_de3,file=paste(rdadir,"stat_data_de3.rda",sep="/"))

stat_data_de4<-statdata(folder,result,algo="de4");
stat_data_de4<-arrange(stat_data_de4,h0l0)
#Save Rda file with statisctal data from DE4 algorithm for np30ng100
save(stat_data_de4,file=paste(rdadir,"stat_data_de4.rda",sep="/"))

rm(list=c("stat_data_de1","stat_data_de2","stat_data_de3","stat_data_de4"))