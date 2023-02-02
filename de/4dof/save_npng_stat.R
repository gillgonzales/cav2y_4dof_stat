require(car)
library(FSA)
library(rcompanion)
library(ggplot2)
library(tidyverse)

  algo='de3' # trocar o algoritmo
  npdir=grep("np\\d{1,}ng\\d{1,}",dir(),fixed=F,value=T)
  npdir
  i=10 #trocar o índice do valor ao qual se quer caluclar, 10 -> h0l0=24,5
  table=c()
  for (d in npdir){
    stat_data<-local({
      load(file=paste(d,"/rda/stat_data_",algo,".rda",sep=""))
      stopifnot(length(ls())==1)
      environment()[[ls()]]
    })
    std_tmin=stat_data$tmin_std[i]
    mean_tmin=stat_data$tmin_med[i]
    table = rbind(table,c("npng"=d,"mean"=mean_tmin,"std"=std_tmin))
  }
  table
  fname=paste(algo,"_npng_stat_h0l0_",stat_data$h0l0[i],".csv",sep="")
  write.csv(table,file=fname,row.names=F)

  