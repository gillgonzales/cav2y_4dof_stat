require(car)
library(FSA)
library(rcompanion)
library(ggplot2)
library(tidyverse)
  
rdafolder = "np30ng50"
alpha=0.05;
vardata = 'tmin'
plottype="ggplot"

if(plottype!="boxplot"){
    plottype="ggplot"
  }

  
  #---Create Dir to save plot image
  
  dirplot<-paste(rdafolder,"plots","kw_boxplot",plottype,'ab',sep="/")
  if(!dir.exists(dirplot)){
    dir.create(dirplot,recursive = T)
  }
  
  logname=paste(vardata,plottype,"mkw_de_",alpha,rdafolder,".log",sep="_")
  con <- file(paste(dirplot,logname,sep="/"))
  sink(con, append=TRUE)
  sink(con, append=TRUE, type="message")
  
  cat("\nAnalise  de Dados com Kruskal-Wallis para amostras nao parametricas\n")
  #CARREGANDO RDAs COM OS DADOS 
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
  
  # ERRO PADRAO
  data.se <- with(data,tapply(data[[vardata]],algo,function(x) sqrt(var(x)/length(x))))
  
  #NAMES
  denames = c("ED11","ED21","ED31","ED41",
              "ED12","ED22","ED32","ED42")
  
  # EXIBINDO OS DADOS
  datastat=data.frame(Repet = data.nrep, Medias = data.mds, Mediana = data.mda,
                      Variancias = data.var, EP = data.se,
                      row.names = denames)
 
  #Test for distribution normality
  sh=shapiro.test(data[[vardata]])
  print(sh)
  #Test for equal variance
  recipe = formula(paste(vardata,'algo',sep="~"));
  lv=leveneTest(recipe,data)
  print(lv)
  #Classification for non-parametric data by Kruskal Wallis test
  kt <- kruskal.test(recipe, data) # Kruskal Wallis test
  print(kt) # show Kruskal Wallis result
  
  PT = dunnTest(recipe,data,method="bh")
  print(PT)
  
  PT = PT$res
  
  letters = rep("a",8)
  try({kt.cldList = cldList(comparison = PT$Comparison,
          p.value    = PT$P.adj,
          threshold  = alpha)
    print(kt.cldList)
  letters = kt.cldList$Letter
  })
  
  sink()
  sink(type="message")
  #############SAVE Plot ##################

  imgfilename=paste(vardata,plottype,"mkw_edab_a",alpha,rdafolder,".png",sep="_")
  
  if(min(data[[vardata]])<0){
    lim_y=c(min(data[[vardata]])*1.2,max(data[[vardata]])*1.2)
  }else{
    lim_y=c(min(data[[vardata]])*0.8,max(data[[vardata]])*1.2)
  }
  
  if(plottype=="boxplot"){
    
    png(
      filename=paste(dirplot,imgfilename,sep="/"),
      width     = 3.25,
      height    = 3.25,
      units     = "in",
      res       = 300,
      pointsize = 5
    )
    #-------------------------------------
  #########BOXPLOT DEFAULT R###############
  par(
    mar      = c(5, 5, 2, 2),
    xaxs     = "i",
    yaxs     = "i",
    cex.axis = 1.5,
    cex.lab  = 1.5
  )
  # this result could be shown on a box plot
  #par(cex.lab=1.3) # is for y-axis
  #par(cex.axis=1.3) # is for x-axis
  boxplot(recipe
  ,data
  ,xlab = "Versoes do Algoritmo ED", ylab = vardata
  , ylim = lim_y
  , notch = F, pch = "O",
  col=)
  mtext(side=3,text=letters,at=1:length(letters),cex=2) # letters at top
  #-----------------------------------
  }else{
  #########GGPLOT BOX PLOT ##############
  kwframe<-data.frame(x=kt.cldList$Group,
                      y=datastat$Mediana,
                      l=toupper(letters));
  cls=c("A"="red","B"="green","AB"="green","C"="pink","BC"="purple","AC"="orange","AB"="black")
  cl1=as.character(cls[as.character(kwframe$l[1])])
  cl2=as.character(cls[as.character(kwframe$l[2])])
  cl3=as.character(cls[as.character(kwframe$l[3])])
  cl4=as.character(cls[as.character(kwframe$l[4])])
  cl5=as.character(cls[as.character(kwframe$l[5])])
  cl6=as.character(cls[as.character(kwframe$l[6])])
  cl7=as.character(cls[as.character(kwframe$l[7])])
  cl8=as.character(cls[as.character(kwframe$l[8])])
  p<-ggplot()+
     labs(x = 'Versoes do ED', y = paste(vardata))+
    #USAR A MESMA ORDEM DO kwframe$X=x
    geom_boxplot(aes(algo,dtdea1[[vardata]]),dtdea1,color=cl1,stat = "boxplot")+
    geom_boxplot(aes(algo,dtdeb1[[vardata]]),dtdeb1,color=cl2,stat = "boxplot")+
    geom_boxplot(aes(algo,dtdea2[[vardata]]),dtdea2,color=cl3,stat = "boxplot")+
    geom_boxplot(aes(algo,dtdeb2[[vardata]]),dtdeb2,color=cl4,stat = "boxplot")+
    geom_boxplot(aes(algo,dtdea3[[vardata]]),dtdea3,color=cl5,stat = "boxplot")+
    geom_boxplot(aes(algo,dtdeb3[[vardata]]),dtdeb3,color=cl6,stat = "boxplot")+
    geom_boxplot(aes(algo,dtdea4[[vardata]]),dtdea4,color=cl7,stat = "boxplot")+
    geom_boxplot(aes(algo,dtdeb4[[vardata]]),dtdeb4,color=cl8,stat = "boxplot")+
     
  geom_point(aes(x,y),kwframe,alpha=0)+
    geom_text(
      aes(x,y,label=l,colour=l),kwframe,
      position = position_dodge(0.9),
      vjust=3,#-3,
      hjust=0.5,#1.5,
      family="Serif",
      size=2)+
      scale_colour_manual(values=c('red','green','blue'))+
      theme_bw()+
      theme(legend.position = "none",
     text = element_text(size=7.5,family="Serif"),
     plot.margin = margin(0.1, 0.1, 0.5, 0.5, "cm"),
     # aspect.ratio=1/4,
     panel.grid= element_line(colour = "white"))+
    
    scale_y_continuous(limits=lim_y)
  
  p
  aspect_ratio <- 2
  height <- 7
  ggsave(paste(dirplot,imgfilename,sep="/"), height = 7 , width = 7 * aspect_ratio,units = "cm")
  }