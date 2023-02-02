require(car)
library(FSA)
library(rcompanion)
library(ggplot2)
library(tidyverse)

# plot_kw_boxplot = function(rdafolder,vardata,alpha,plottype){
  
  rdafolder = "np30ng100"
  alpha=0.05;
  vardata = 'tmin'
  plottype="ggplot"

  if(plottype!="boxplot"){
    plottype="ggplot"
  }  

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
  # load(file=paste(rdafolder,"rda/data_de4.rda",sep="/"))
  # dtde4<-bind_cols(algo=rep("DE4",count(data_de4)),data_de4)
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
  
  # ERRO PADRAO
  data.se <- with(data,tapply(data[[vardata]],algo,function(x) sqrt(var(x)/length(x))))
  
  # EXIBINDO OS DADOS
  datastat=data.frame(Repet = data.nrep,
                      Medias = data.mds,
                      Mediana = data.mda,
                      Variancias = data.var,
                      EP = data.se, row.names = c("DE1","DE2","DE3"))
 
  
  #Test for distribution normality
  sh=shapiro.test(data[[vardata]])
  print(sh)
  try({
    sh=shapiro.test(data_de1[[vardata]])
    print(sh)
  })
  try({
    sh=shapiro.test(data_de2[[vardata]])
    print(sh)
  })
  try({
    sh=shapiro.test(data_de3[[vardata]])
    print(sh)
  })
  
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
  
  letters = rep("a",3)
  try({kt.cldList = cldList(comparison = PT$Comparison,
          p.value    = PT$P.adj,
          threshold  = alpha)
    print(kt.cldList)
  letters = kt.cldList$Letter
  })
  
  sink()
  sink(type="message")
  #############SAVE Plot ##################

  imgfilename=paste(vardata,plottype,"mkw_ed_a",alpha,rdafolder,".png",sep="_")
  
  png(
    filename=paste(dirplot,imgfilename,sep="/"),
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 300,
    pointsize = 5
  )
  #-------------------------------------
  
  
  if(min(data[[vardata]])<0){
    lim_y=c(min(data[[vardata]])*1.2,max(data[[vardata]])*1.2)
  }else{
    lim_y=c(min(data[[vardata]])*0.8,max(data[[vardata]])*1.2)
  }
  
  if(plottype=="boxplot"){
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
  vlet=c(
      rep(as.character(letters)[1],30),
      rep(as.character(letters)[2],30),
      # rep(as.character(letters)[3],30),
      rep(as.character(letters)[3],30))
  kwframe<-data.frame(x=c("DE1","DE2","DE3"),
                      y=datastat$Mediana,
                      l=toupper(letters));
  cls=c("A"="red","B"="blue","AB"="green")
  cl1=as.character(cls[as.character(kwframe$l[1])])
  cl2=as.character(cls[as.character(kwframe$l[2])])
  cl3=as.character(cls[as.character(kwframe$l[3])])
  # cl4=as.character(cls[as.character(kwframe$l[4])])
  p<-ggplot()+
     labs(x = 'Versoes do DE', y = paste(vardata))+
     geom_boxplot(aes(algo,dtde1[[vardata]]),dtde1,color=cl1,stat = "boxplot")+
    geom_boxplot(aes(algo,dtde2[[vardata]]),dtde2,color=cl2,stat = "boxplot")+
    geom_boxplot(aes(algo,dtde3[[vardata]]),dtde3,color=cl3,stat = "boxplot")+
    # geom_boxplot(aes(algo,dtde4[[vardata]]),dtde4,color=cl4,stat = "boxplot")+
     # ggtitle(expression(atop(bold("Kruskal Wallis"), atop(italic("(Versoes do DE)"), ""))))+
     # geom_text(label=vlet,vjust=0,hjust=0)+
    # geom_jitter(aes(algo,dtde1[[vardata]]),dtde1,color=cl1,size=0.5,alpha=1/2,height = 0)+
    # geom_jitter(aes(algo,dtde2[[vardata]]),dtde2,color=cl2,size=0.5,alpha=1/2,height = 0)+
    # geom_jitter(aes(algo,dtde3[[vardata]]),dtde3,color=cl3,size=0.5,alpha=1/2,height = 0)+
    # geom_jitter(aes(algo,dtde4[[vardata]]),dtde4,color=cl4,size=0.5,alpha=1/2,height = 0)+
     # geom_text(aes(x=algo,y=letters))
     geom_point(aes(x,y),kwframe,alpha=0)+
    geom_text(
      aes(x,y,label=l,colour=l),kwframe,
      position = position_dodge(0.9),
      vjust=-1,
      family="Serif",
      size=4)+
      theme_bw()+
      theme(legend.position = "none",
     text = element_text(size=10,family="Serif"),
     panel.grid= element_line(colour = "white"))+
    scale_y_continuous(limits=lim_y)
  p
  }
  
# }
# 
# rdafolder = "np30ng100"
# alpha=0.05
# plottype="ggplot"

# plot_kw_boxplot(rdafolder,vardata='tmin',alpha,plottype)
# dev.off()
# plot_kw_boxplot(rdafolder,vardata='s1h0',alpha,plottype)
# dev.off()
# plot_kw_boxplot(rdafolder,vardata='beta',alpha,plottype)
# dev.off()
# plot_kw_boxplot(rdafolder,vardata='alfa',alpha,plottype)
# dev.off()

