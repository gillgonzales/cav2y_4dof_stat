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
  
  dirplot<-paste(rdafolder,"plots","kw_boxplot",plottype,'th',sep="/")
  if(!dir.exists(dirplot)){
    dir.create(dirplot,recursive = T)
  }
  
  logname=paste(vardata,plottype,"mkw_de_",alpha,rdafolder,".log",sep="_")
  con <- file(paste(dirplot,logname,sep="/"))
  sink(con, append=TRUE)
  sink(con, append=TRUE, type="message")
  

  cat("\nAnalise  de Dados com Kruskal-Wallis para amostras nao parametricas\n")
  
  #CARREGANDO RDAS COM OS DADOS 
  load(file=paste(rdafolder,"rda/data_de3a.rda",sep="/"))
  dtdea3<-bind_cols(algo=rep("DE31",count(data_de3)),data_de3)
  load(file=paste(rdafolder,"rda/data_de4a.rda",sep="/"))
  dtdea4<-bind_cols(algo=rep("DE41",count(data_de4)),data_de4)
  data<-bind_rows(dtdea3,dtdea4)
  
  load(file=paste(rdafolder,"rda/data_de1b.rda",sep="/"))
  dtdeb1<-bind_cols(algo=rep("DE12",count(data_de1)),data_de1)
  load(file=paste(rdafolder,"rda/data_de2b.rda",sep="/"))
  dtdeb2<-bind_cols(algo=rep("DE22",count(data_de2)),data_de2)
  data<-bind_rows(data,dtdeb1,dtdeb2)
  

  load(file=paste(rdafolder,"rda/data_de3c.rda",sep="/"))
  dtdec3<-bind_cols(algo=rep("DE33",count(data_de3)),data_de3)
  load(file=paste(rdafolder,"rda/data_de4c.rda",sep="/"))
  dtdec4<-bind_cols(algo=rep("DE43",count(data_de4)),data_de4)
  data<-bind_rows(data,dtdec3,dtdec4)
  
  load(file=paste(rdafolder,"rda/data_de1d.rda",sep="/"))
  dtded1<-bind_cols(algo=rep("DE14",count(data_de1)),data_de1)
  load(file=paste(rdafolder,"rda/data_de2d.rda",sep="/"))
  dtded2<-bind_cols(algo=rep("DE24",count(data_de2)),data_de2)
  data<-bind_rows(data,dtded1,dtded2)

  print(data)
  summary(data)
  
  #NUMERO DE OBSERVAcoes
  data.nrep <- with(data, tapply(data[[vardata]],algo,length))
  
  #ANALISE ESTATISTICA DE CADA TRATAMENTO (PARA CADA CONTROLE {BOLTZ, EXP, FAST, BOLTZEXP})
  
  #MDEIA 
  data.mds <-with(data, tapply(data[[vardata]],algo,mean))
  
  data.mda <-with(data, tapply(data[[vardata]],algo,median))
  #VARIANCIAdata,tapply(s1,algo,var))
  data.var <- with(data,tapply(data[[vardata]],algo,var))
  
  # ERRO PADRAO
  data.se <- with(data,tapply(data[[vardata]],algo,function(x) sqrt(var(x)/length(x))))
  
  
  #NAMES
  denames = names(data.var)
  
  # EXIBINDO OS DADOS
  datastat=data.frame(Repet = data.nrep, Medias = data.mds, Mediana = data.mda,
                      Variancias = data.var, EP = data.se,
                      row.names = denames)
 
  
  #Test for distribution normality
  sh=shapiro.test(data[[vardata]])
  print(sh)
  #POR ALGOR
  #DE12
  print(shapiro.test(dtdeb1[[vardata]]))
  #DE14
  print(shapiro.test(dtded1[[vardata]]))
  #DE22
  print(shapiro.test(dtdeb2[[vardata]]))
  #DE24
  print(shapiro.test(dtded2[[vardata]]))
  #DE31
  print(shapiro.test(dtdea3[[vardata]]))
  #DE33
  print(shapiro.test(dtdec3[[vardata]]))
  #DE41
  print(shapiro.test(dtdea4[[vardata]]))
  #DE43
  print(shapiro.test(dtdec4[[vardata]]))
  
  
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
  
  letters = rep("a",16)
  try({kt.cldList = cldList(comparison = PT$Comparison,
          p.value    = PT$P.adj,
          threshold  = alpha)
    print(kt.cldList)
  letters = kt.cldList$Letter
  })
  
  sink()
  sink(type="message")
  #############SAVE Plot ##################

  imgfilename=paste(vardata,plottype,"mkw_edad_a",alpha,rdafolder,".png",sep="_")
  
  if(min(data[[vardata]])<0){
    lim_y=c(min(data[[vardata]])*1.2,max(data[[vardata]])*1.2)
  }else{
    lim_y=c(min(data[[vardata]])*0.8,max(data[[vardata]]))
  }
  
  lim_y
  
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
  ,xlab = "Versoes do Algoritmo DE", ylab = vardata
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
  #0.01
  if(alpha==0.01){
    cls=c("A"="red","B"="blue","ABC"="green","C"="pink","BC"="purple","AC"="orange","AB"="black")
  }else{
  #0.05
    cls=c("A"="red","B"="green","AB"="orange","C"="purple","BC"="blue")
  }
  cl1=as.character(cls[as.character(kwframe$l[1])])
  cl2=as.character(cls[as.character(kwframe$l[2])])
  cl3=as.character(cls[as.character(kwframe$l[3])])
  cl4=as.character(cls[as.character(kwframe$l[4])])
  cl5=as.character(cls[as.character(kwframe$l[5])])
  cl6=as.character(cls[as.character(kwframe$l[6])])
  cl7=as.character(cls[as.character(kwframe$l[7])])
  cl8=as.character(cls[as.character(kwframe$l[8])])
  p<-ggplot()+
     labs(x = 'DE Algorithm Versions', y = paste(vardata))+
    #Use in the same order of levels kt.cldList$Letter
    scale_y_continuous(breaks=seq(0.03,0.124,length.out=6))+
    expand_limits(y=c(0.03,0.124))+
    geom_boxplot(aes(algo,dtdeb1[[vardata]]),dtdeb1,color=cl1,stat = "boxplot")+
    geom_boxplot(aes(algo,dtded1[[vardata]]),dtded1,color=cl2,stat = "boxplot")+
    geom_boxplot(aes(algo,dtdeb2[[vardata]]),dtdeb2,color=cl3,stat = "boxplot")+
    geom_boxplot(aes(algo,dtded2[[vardata]]),dtded2,color=cl4,stat = "boxplot")+
    geom_boxplot(aes(algo,dtdea3[[vardata]]),dtdea3,color=cl5,stat = "boxplot")+
    geom_boxplot(aes(algo,dtdec3[[vardata]]),dtdec3,color=cl6,stat = "boxplot")+
    geom_boxplot(aes(algo,dtdea4[[vardata]]),dtdea4,color=cl7,stat = "boxplot")+
    geom_boxplot(aes(algo,dtdec4[[vardata]]),dtdec4,color=cl8,stat = "boxplot")+
    
  geom_point(aes(x,y),kwframe,alpha=0)+
    geom_text(
      aes(x,y,label=l,colour=l),kwframe,
      position = position_dodge(0.9),
      vjust=3,#-3,
      hjust=0.5,#1.5,
      family="Serif",
      size=3)+
      #scale_colour_manual(values=c('red','orange','green','blue','purple'))+ #for a=0.05
      scale_colour_manual(values=c('red','green','orange','blue','purple'))+
      theme_bw()+
      theme(legend.position = "none",
     text = element_text(size=10,family="Serif"),
     plot.margin = margin(0.1, 0.1, 0.5, 0.5, "cm"),
     # aspect.ratio=1/4,
     panel.grid= element_line(colour = "white"))
  
  p
  aspect_ratio <- 2
  height <- 7
  ggsave(paste(dirplot,imgfilename,sep="/"), height = 7 , width = 7 * aspect_ratio,units = "cm")
  }
  
dev.off()

