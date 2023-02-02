require(car)
library(FSA)
library(rcompanion)
library(ggplot2)
library(tidyverse)


# fnc_kruskal_vpoints_boxplot = function(data1,data2,rdafolder,vardata,alpha,plottype){
  
  ###DEBUG######
  deb2bin_b <- read_csv("mutation_test/csv/visited_points_3dof_opt_de_np30_ng_100_cr_0.9_f_1_20200921_200905_log.csv",col_names = FALSE)
  colnames(deb2bin_b)<-c('alfa','beta','s1h0','tmin');
  
  deb2bin_a <- read_csv("mutation_test/csv/visited_points_3dof_opt_de_np30_ng_100_cr_0.9_f_1_20200921_174736_log.csv",col_names = FALSE)
  colnames(deb2bin_a)<-c('alfa','beta','s1h0','tmin');
  
  
  rdafolder = "mutation_test"
  alpha=0.05
  plottype="boxplot"
  data1=deb2bin_a
  data2=deb2bin_b
  vardata="tmin"
  
  if(plottype!="boxplot"){
    plottype="ggplot"
  }
  
  #---Create Dir to save plot image
  
  dirplot<-paste(rdafolder,"plots","kw_boxplot_vpoints",plottype,sep="/")
  if(!dir.exists(dirplot)){
    dir.create(dirplot,recursive = T)
  }
  
  
  logname=paste(vardata,plottype,"mkw_ed_a",alpha,rdafolder,".log",sep="_")
  con <- file(paste(dirplot,logname,sep="/"))
  # sink(con, append=TRUE)
  # sink(con, append=TRUE, type="message")

  cat("\nAnalise  de Dados com Kruskal-Wallis para amostras nao parametricas\n")
  #LENDO TABELA COM OS DADOS
  #load(file=paste(rdafolder,"rda/v_points_de3.rda",sep="/"))
  data1<-filter(
    bind_cols(algo=rep("DT1",count(data1)),data1)
    ,tmin<1)
  
  data2<-filter(
    bind_cols(algo=rep("DT2",count(data2)),data2)
    ,tmin<1)
  
  data<-bind_rows(data1,data2)

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
  datastat=data.frame(Repet = data.nrep, Medias = data.mds, Mediana = data.mda, Variancias = data.var, EP = data.se, row.names = c("DT1","DT2"))
 
  
  #Test for distribution normality
  try({sh=shapiro.test(data[[vardata]])
  print(sh)})
  
  try({
  sh=shapiro.test(data1[[vardata]])
  print(sh)})
  try({
  sh=shapiro.test(data2[[vardata]])
  print(sh)})
  try({
    sh=shapiro.test(data3[[vardata]])
    print(sh)})
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
  
  letters = rep("a",2)
  try({kt.cldList = cldList(comparison = PT$Comparison,
          p.value    = PT$P.adj,
          threshold  = alpha)
    print(kt.cldList)
  letters = kt.cldList$Letter
  })
  # sink()
  # sink(type="message")
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
    lim_y=c(min(data[[vardata]])*-0.1,max(data[[vardata]])*1.2)
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
  ,xlab = "Algoritmos", ylab = vardata
  , ylim = lim_y
  , notch = F, pch = "O",
  col=)
  mtext(side=3,text=letters,at=1:length(letters),cex=2) # letters at top
  #-----------------------------------
  }else{
  #########GGPLOT BOX PLOT ##############
 
  kwframe<-data.frame(x=c("DT1","DT2"),
                      y=datastat$Mediana,
                      l=toupper(letters));
  cls=c("A"="red","B"="green","C"="blue")
  cl1=as.character(cls[as.character(kwframe$l[1])])
  cl2=as.character(cls[as.character(kwframe$l[2])])
  cl3=as.character(cls[as.character(kwframe$l[3])])
  cl4=as.character(cls[as.character(kwframe$l[4])])
  p<-ggplot()+
     labs(x = 'Algoritmos', y = paste(vardata))+
    
    geom_jitter(aes(algo,data1[[vardata]]),data1,color=cl1,size=0.5,alpha=1/20,height = 0.05)+
    geom_jitter(aes(algo,data2[[vardata]]),data2,color=cl2,size=0.5,alpha=1/20,height = 0.05)+
    
    geom_boxplot(aes(algo,data1[[vardata]]),data1,color=cl1,stat = "boxplot",alpha=1/2)+
    geom_boxplot(aes(algo,data2[[vardata]]),data2,color=cl2,stat = "boxplot",alpha=1/2)+
    
    
    
    geom_point(aes(x,y),kwframe,alpha=0)+
    geom_text(
      aes(x,y,label=l,colour=l),kwframe,
      position = position_dodge(),
      hjust=1.5,
      vjust=-2,
      family="Serif",
      fontface="bold",
      size=5)+
      theme_bw()+
      theme(legend.position = "none",
     text = element_text(size=10,family="Serif"),
     panel.grid= element_line(colour = "white"))+
    scale_y_continuous(limits=lim_y)
  p
  }
# }

# rdafolder = "mutation_test"
# alpha=0.01
# plottype="boxplot"

# fnc_kruskal_vpoints_boxplot(deb2bin_a,deb2bin_b,rdafolder,vardata='tmin',alpha,plottype)
# dev.off()
# fnc_kruskal_vpoints_boxplot(deb2bin_a,deb2bin_b,rdafolder,vardata='s1h0',alpha,plottype)
# dev.off()
# fnc_kruskal_vpoints_boxplot(deb2bin_a,deb2bin_b,rdafolder,vardata='beta',alpha,plottype)
# dev.off()
# fnc_kruskal_vpoints_boxplot(deb2bin_a,deb2bin_b,rdafolder,vardata='alfa',alpha,plottype)
dev.off()


