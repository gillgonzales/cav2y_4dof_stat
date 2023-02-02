require(car)
library(FSA)
library(rcompanion)
library(ggplot2)
library(tidyverse)

rdafolder = "np30ng100"
alpha=0.05;
vardata = 'tmin'
  
  #---Create Dir to save plot image
  
  dirplot<-paste(rdafolder,"plots","kw_boxplot",plottype,sep="/")
  if(!dir.exists(dirplot)){
    dir.create(dirplot,recursive = T)
  }
  
  logname=paste(vardata,plottype,"mkw_de_",alpha,rdafolder,".log",sep="_")
  con <- file(paste(dirplot,logname,sep="/"))
  sink(con, append=TRUE)
  sink(con, append=TRUE, type="message")
  

  cat("\nAnalise  de Dados com Kruskal-Wallis para amostras nao parametricas\n")
  #LENDO TABELA COM OS DADOS 
  #data <- read.table('./dados_s1.txt', head = T)
  load(file=paste(rdafolder,"rda/data_de1.rda",sep="/"))
  dtde1<-bind_cols(algo=rep("DE1",count(data_de1)),data_de1)
  load(file=paste(rdafolder,"rda/data_de2.rda",sep="/"))
  dtde2<-bind_cols(algo=rep("DE2",count(data_de2)),data_de2)
  load(file=paste(rdafolder,"rda/data_de3.rda",sep="/"))
  dtde3<-bind_cols(algo=rep("DE3",count(data_de3)),data_de3)
  data<-bind_rows(dtde1,dtde2,dtde3)

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
  # ERRO PADRAO
  data.se <- with(data,tapply(data[[vardata]],algo,se))
  
  
  #RMSE
  ex<-0.020797505102979 #exhaustive search result
  if(vardata=='a'){
    ex<---53
  }
  if(vardata=='b'){
    ex<-0
  }
  
  
  data.rmse <-with(data,tapply(data[[vardata]],algo,function(x) sqrt(mean((x-ex)^2))))
  #NAMES
  denames = names(data.var)
  
  # EXIBINDO OS DADOS
  datastat=data.frame(Algo = denames,
                      Medias = data.mds,
                      Mediana = data.mda,
                      Variancias = data.var,
                      Desvio=data.sd, 
                      Erro = data.se, 
                      RMSE = data.rmse)
 
  write.csv(datastat,file=paste(rdafolder,"/csv/",vardata,"_",rdafolder,"_datastat.csv",sep=""),row.names=F)