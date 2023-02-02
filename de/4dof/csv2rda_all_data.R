library(readr)
library(dplyr)
# The folder CSV must be create in the same directory of this file.
# The csv results files in CSV folder come from results repo at https://gitlab.com/g1ll/cav2yresults
# the csv files was not save in this repo to dont increase the repo size.
# Import statistical summary files and save as a compiled  Rda object to R scripts
# Results from: 
# https://gitlab.com/g1ll/cav2yresults/-/raw/master/de/4dof/h0l0_de_np30_ng100/h0l0_de_np30_ng100_d7559_20200312.tar.gz
#Function to import CSV files and join into a RDA file
alldata=function(filesourcelist,dataeffectarray,cnames){
  all_data<-c()
  j<-1
  for(i in dataeffectarray){
    all_data<-bind_rows(
      all_data,
      bind_cols(
        data.frame(h0l0=rep(i,30)),
        read.csv(filesourcelist[j],header = F,col.names=cnames)
      )
    )
    j=j+1
    if(j>length(filesourcelist))
      break
  }
  all_data
}

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

#Function to get csv file source path
csvfilesource=function(folder,result,algo){
  filesource<-c();
  csvdir<-paste(folder,"csv",result,sep="/")
  algodir<-dir(csvdir);
  algoi<-grep(algo,algodir)
  for(j in algoi){
    tmpdir <- dir(paste(csvdir,algodir[j],sep="/"))
    for(i in h0l0){
      resultdir<-tmpdir[grep(i,tmpdir)]
      if(length(resultdir)!=0){
        resultdirpath<-paste(csvdir,algodir[j],resultdir,sep="/")
        csvfile<-dir(resultdirpath)[grep("csv",dir(resultdirpath))]
        csvfilepath<-paste(resultdirpath,csvfile,sep="/")
        filesource<-c(filesource,csvfilepath)
      }
    }
  }
  filesource
}

#Extract the values of H0L0
folder<-"np40ng75" #Set the folder name
result<-"h0l0_de_np40_ng75_labsin3_labsin5_20200401" #Set the results folder

algo="de1"
datastat<-statdata(folder,result,algo)
h0l0<-datastat$h0l0

rdadir<-paste(folder,"rda",sep="/")
if(!dir.exists(rdadir)){
  dir.create(rdadir)
}

#All data for DE1
colnames<-c('tmin','alfa','beta','s1h0','nfo','time');
algo<-"de1"
filesource<-csvfilesource(folder,result,algo)
all_data_de1<-alldata(filesource,h0l0,colnames)
save(all_data_de1,file=paste(rdadir,"all_data_de1.rda",sep="/"))

#All data for DE2
filesource<-csvfilesource(folder,result,algo="de2")
all_data_de2<-alldata(filesource,h0l0,colnames)
save(all_data_de2,file=paste(rdadir,"all_data_de2.rda",sep="/"))

#All data for DE3
filesource<-csvfilesource(folder,result,algo="de3")
all_data_de3<-alldata(filesource,h0l0,colnames)
save(all_data_de3,file=paste(rdadir,"all_data_de3.rda",sep="/"))

#All data for DE4
filesource<-csvfilesource(folder,result,algo="de4")
all_data_de4<-alldata(filesource,h0l0,colnames)
save(all_data_de4,file=paste(rdadir,"all_data_de4.rda",sep="/"))