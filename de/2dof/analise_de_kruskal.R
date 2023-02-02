cat("\nAnalise  de Dados com Kruskal-Wallis para amostras nao parametricas\n")

source_data<-'./dados_tmin.txt';
xvar<-'tmin';
alpha<-0.01;

#LENDO TABELA COM OS DADOS 
data <- read.table(source_data, head = T)

print(data)
summary(data)

#NUMERO DE OBSERVAcoes
data.nrep <- with(data, tapply(x,algo,length))

#ANALISE ESTATISTICA DE CADA TRATAMENTO (PARA CADA CONTROLE {BOLTZ, EXP, FAST, BOLTZEXP})

#MEDIA 
data.mds <-with(data, tapply(x,algo,mean))
#VARIANCIAdata,tapply(a,algo,var))

data.var <- with(data,tapply(x,algo,var))

# ERRO PADRAO
data.se <- with(data,tapply(x,algo,function(x) sqrt(var(x)/length(x))))

# EXIBINDO OS DADOS
da.fr<-data.frame(Repet = data.nrep, Medias = data.mds,Variancias = data.var, EP = data.se, row.names = c("DE1","DE2","DE3","DE4"))



require(car)
#-----------SHAPIRO-WILK TEST -------------------
#Test for distribution normality
shp <-shapiro.test(data$x);

if(shp$p.value>alpha){
  print("ANALISE DE DADOS PARAMETRICA")
  #-----------LEVENE TEST -------------------
  #Test for equal variance
  leveneTest(x~algo,data)
  print("ANOVA TEST")

}else{
  print("ANALISE DE DADOS NÃO-PARAMETRICA")
  #-----------KRUSKAL-WALLIS TEST -------------------
  #Classification for non-parametric data by Kruskal Wallis test
  kt <- kruskal.test(x~algo, data) # Kruskal Wallis test
  print(kt) # show Kruskal Wallis result
  
  #-----------FRIEDMAN TEST -------------------
   datam<-cbind(data[data$algo=="DE1",]$x, data[data$algo=="DE2",]$x, data[data$algo=="DE3",]$x, data[data$algo=="DE4",]$x);
   fp<-friedman.test(datam);
  
  print(fp)
  pval = kt$p.value
  print(paste("p-value = ",pval));
  print(paste("alpha = ",alpha));
  pvalues<-c();
  if(pval<alpha){
    print("p.value < alpha");
    de1<-datam[,1];
    de2<-datam[,2];
    de3<-datam[,3];
    de4<-datam[,4];
    
    de1_de2<-wilcox.test(de1,de2,paired=T,exac=T);
    de1_de3<-wilcox.test(de1,de3,paired=T,exac=T);
    de1_de4<-wilcox.test(de1,de4,paired=T,exac=T);
    de2_de3<-wilcox.test(de2,de3,paired=T,exac=T);
    de2_de4<-wilcox.test(de2,de4,paired=T,exac=T);
    de3_de4<-wilcox.test(de3,de4,paired=T,exac=T);
    
   
    print("DE1 x DE2 \n");
    print(de1_de2);
    pv<-FALSE
    if(!is.na(de1_de2$p.value) & de1_de2$p.value<alpha)
      pv<-TRUE
    pvalues<-append(pvalues,pv)
    
    print("DE1 x DE3 \n");
    print(de1_de3);
    pv<-FALSE
    if(!is.na(de1_de3$p.value) & de1_de3$p.value<alpha)
      pv<-TRUE
    pvalues<-append(pvalues,pv)
    
    print("DE1 x DE2 \n");
    print(de1_de4);
    pv<-FALSE
    if(!is.na(de1_de4$p.value) & de1_de4$p.value<alpha)
      pv<-TRUE
    pvalues<-append(pvalues,pv)
    
    print("DE2 x DE3 \n");
    print(de2_de3);
    pv<-FALSE
    if(!is.na(de2_de3$p.value) & de2_de3$p.value<alpha)
      pv<-TRUE
    
    pvalues<-append(pvalues,pv)
    
    print("DE2 x DE4 \n");
    print(de2_de4);
    pv<-FALSE
    if(!is.na(de2_de4$p.value) & de2_de4$p.value < alpha)
      pv<-TRUE
    pvalues<-append(pvalues,pv)
    
    
    print("DE3 x DE4 \n");
    print(de3_de4);
    pv<-FALSE
    if(!is.na(de3_de4$p.value) & de3_de4$p.value < alpha)
      pv<-TRUE
    pvalues<-append(pvalues,pv)
    
    print("p.value < alpha");
    
  }else{
    print("p.value > alpha");
    pvalues<-append(pvalues,rep(FALSE,6));
  }
  
  print(pvalues)
  
  # ------MULTICOMPVIEW PLOT-------
    
    require('multcompView')
    test <- pvalues # select logical vector
    names(test) <- c('de1-de2','de1-de3','de1-de4','de2-de3','de2-de4','de3-de4')# add comparison names
    # create a list with "homogenous groups" coded by letter
    let <- multcompLetters(test,
                           Letters=c(letters, LETTERS, "."),
                           reversed = FALSE)
    png(
      filename=paste("multikruskal_",xvar,"_",alpha,".png"),
      width     = 4,
      height    = 4,
      units="in",
      res=300,
      pointsize = 5
    )
    par(
      mar      = c(5, 5, 2, 2),
      xaxs     = "i",
      yaxs     = "i",
      cex.axis = 2,
      cex.lab  = 2
    )
    # this result could be shown on a box plot
    #par(cex.lab=1.3) # is for y-axis
    #par(cex.axis=1.3) # is for x-axis
    boxplot(x ~algo
            ,data
            ,xlab = "Versoes do Algoritmo ED", ylab = toupper(xvar)
            #, ylim = c(min(data$x,na.rm=T)*1.1,max(data$x,na.rm=T)*1.1)# a b
            , ylim = c(min(data$x,na.rm=T)*0.95,max(data$x,na.rm=T)*1.05) # tmin
            #, ylim = c(-100,-250)
            , notch = F, pch = "O")
    mtext(side=3,text=let$Letters,at=1:length(let$Letters),cex=2) # letters at top
    dev.off()
}