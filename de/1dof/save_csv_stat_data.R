require(car)
library(FSA)
library(rcompanion)
library(ggplot2)
library(tidyverse)
  
rdafolder = "np5ng9"
alpha=0.05;
vardata = 'tmin'
plottype="ggplot"

if(plottype!="boxplot"){
    plottype="ggplot"
  }
  
  #---Create Dir to save plot image
  
  dirplot<-paste(rdafolder,"plots","kw_boxplot",plottype,'ad',sep="/")
  if(!dir.exists(dirplot)){
    dir.create(dirplot,recursive = T)
  }
  
  # logname=paste(vardata,plottype,"mkw_de_",alpha,rdafolder,".log",sep="_")
  # con <- file(paste(dirplot,logname,sep="/"))
  # sink(con, append=TRUE)
  # sink(con, append=TRUE, type="message")
  

  cat("\nAnalise  de Dados com Kruskal-Wallis para amostras nao parametricas\n")
  
  #CARREGANDO RDAS COM OS DADOS 
  load(file=paste(rdafolder,"rda/data_de1a.rda",sep="/"))
  dtdea1<-bind_cols(algo=rep("ED11",count(data_de1)),data_de1)
  load(file=paste(rdafolder,"rda/data_de2a.rda",sep="/"))
  dtdea2<-bind_cols(algo=rep("ED21",count(data_de2)),data_de2)
  load(file=paste(rdafolder,"rda/data_de3a.rda",sep="/"))
  dtdea3<-bind_cols(algo=rep("ED31",count(data_de3)),data_de3)
  load(file=paste(rdafolder,"rda/data_de4a.rda",sep="/"))
  dtdea4<-bind_cols(algo=rep("ED41",count(data_de4)),data_de4)
  data<-bind_rows(dtdea1,dtdea2,dtdea3,dtdea4)
  
  load(file=paste(rdafolder,"rda/data_de1b.rda",sep="/"))
  dtdeb1<-bind_cols(algo=rep("ED12",count(data_de1)),data_de1)
  load(file=paste(rdafolder,"rda/data_de2b.rda",sep="/"))
  dtdeb2<-bind_cols(algo=rep("ED22",count(data_de2)),data_de2)
  load(file=paste(rdafolder,"rda/data_de3b.rda",sep="/"))
  dtdeb3<-bind_cols(algo=rep("ED32",count(data_de3)),data_de3)
  load(file=paste(rdafolder,"rda/data_de4b.rda",sep="/"))
  dtdeb4<-bind_cols(algo=rep("ED42",count(data_de4)),data_de4)
  data<-bind_rows(data,dtdeb1,dtdeb2,dtdeb3,dtdeb4)
  
  load(file=paste(rdafolder,"rda/data_de1c.rda",sep="/"))
  dtdec1<-bind_cols(algo=rep("ED13",count(data_de1)),data_de1)
  load(file=paste(rdafolder,"rda/data_de2c.rda",sep="/"))
  dtdec2<-bind_cols(algo=rep("ED23",count(data_de2)),data_de2)
  load(file=paste(rdafolder,"rda/data_de3c.rda",sep="/"))
  dtdec3<-bind_cols(algo=rep("ED33",count(data_de3)),data_de3)
  load(file=paste(rdafolder,"rda/data_de4c.rda",sep="/"))
  dtdec4<-bind_cols(algo=rep("ED43",count(data_de4)),data_de4)
  data<-bind_rows(data,dtdec1,dtdec2,dtdec3,dtdec4)
  
  load(file=paste(rdafolder,"rda/data_de1d.rda",sep="/"))
  dtded1<-bind_cols(algo=rep("ED14",count(data_de1)),data_de1)
  load(file=paste(rdafolder,"rda/data_de2d.rda",sep="/"))
  dtded2<-bind_cols(algo=rep("ED24",count(data_de2)),data_de2)
  load(file=paste(rdafolder,"rda/data_de3d.rda",sep="/"))
  dtded3<-bind_cols(algo=rep("ED34",count(data_de3)),data_de3)
  load(file=paste(rdafolder,"rda/data_de4d.rda",sep="/"))
  dtded4<-bind_cols(algo=rep("ED44",count(data_de4)),data_de4)
  data<-bind_rows(data,dtded1,dtded2,dtded3,dtded4)
  
  rm(list=c('data_de1','data_de2','data_de3','data_de4'))
  
  print(data)
  summary(data)
  
  #NUMERO DE OBSERVAcoes
  data.nrep <- with(data, tapply(data[[vardata]],algo,length))
  
  #ANALISE ESTATISTICA DE CADA TRATAMENTO (PARA CADA CONTROLE {BOLTZ, EXP, FAST, BOLTZEXP})
  
  #MEDIA 
  data.mds <-with(data, tapply(data[[vardata]],algo,mean))
  
  data.mda <-with(data, tapply(data[[vardata]],algo,median))
  #VARIANCIAdata,tapply(s1,algo,var))
  data.var <- with(data,tapply(data[[vardata]],algo,var))
  
  #DESVIO PADRO
  data.sd <-with(data,tapply(data[[vardata]],algo,sd))
  # ERRO PADRAO #sqrt(var(x)/length(x)) Standart Error
  data.se <- with(data,tapply(data[[vardata]],algo,se))
  
  #RMSE
  ex<-0.051373548044918 #exhaustive search result
  if(vardata=='ab'){
       ex<-15
  }
  
  data.rmse <-with(data,tapply(data[[vardata]],algo,function(x) sqrt(mean((x-ex)^2))))
  #NAMES
  denames = names(data.var)
  
  # EXIBINDO OS DADOS
  datastat=data.frame(algo = denames, Medias = data.mds, Mediana = data.mda,
                      Variancias = data.var,Desvio=data.sd, Erro = data.se, RMSE = data.rmse)
 
  
  # sink()
  # sink(type="message")
  write.csv(datastat,file=paste(rdafolder,"/csv/datastat.csv",sep=""),row.names=F)

