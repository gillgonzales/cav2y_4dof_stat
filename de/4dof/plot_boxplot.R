### Multifactorial boxplot
### Ref.:http://www.r-graph-gallery.com/9-ordered-boxplot/
##SCRIPT DE GERACAO DO GRAFICO
##VARIAVEIS DE CONFIGURACAO - ALTERAR PARA GERAR GRAFICOS PARA DADOS DIFERENTES
folder<-"np40ng75" # mudar valor para alterar a pasta de origem dos dados e salvamento das imagens
legenda<-c("DE1", "DE2","DE3","DE4") #alterar para aplicar legenda diferente
pos_leg<-"bottomleft" #Mudar posicao da legenda
# posx_label<-seq(1,76,4)#all
# posx_label<-seq(1,95,5);#all
posx_label<-seq(1,42,4.2);#003-05
# posx_label<-seq(1,54,6);#3-30
 
load(file=paste(folder,"rda/stat_data_de1.rda",sep="/"))
load(file=paste(folder,"rda/stat_data_de2.rda",sep="/"))
load(file=paste(folder,"rda/stat_data_de3.rda",sep="/"))
load(file=paste(folder,"rda/stat_data_de4.rda",sep="/"))

data<-c()
h0l0<-stat_data_de1$h0l0
for(i in 1:length(h0l0)){
  data<-rbind(data,c(h0l0[i],"de1",stat_data_de1$tmin_min[i],stat_data_de1$tmin_std[i]))
  data<-rbind(data,c(h0l0[i],"de2",stat_data_de2$tmin_min[i],stat_data_de2$tmin_std[i]))
  data<-rbind(data,c(h0l0[i],"de3",stat_data_de3$tmin_min[i],stat_data_de3$tmin_std[i]))
  data<-rbind(data,c(h0l0[i],"de4",stat_data_de4$tmin_min[i],stat_data_de4$tmin_std[i]))
}

data<-data.frame(
  h0l0=as.numeric(data[,1]),
  algo=as.factor(data[,2]),
  tmin=as.numeric(data[,3]),
  tstd=as.numeric(data[,4]))

# data = read.table(paste(algo,'/',dados,sep=""), head = T)

png(filename=paste(folder,"plots",'plot_box_all.png',sep="/"),
	width=3.25,
	height=3.25,
	units="in",
	res=1200,
	pointsize=6)
par(cex.lab=1.3)
par(cex.axis=1)
myplot=boxplot(tmin ~ algo+h0l0 , data=data, 
		ylim=range(c(min(data$tmin)*0.8,max(data$tmin)*1.2)),
		boxwex=1 , xlab='H/L',ylab="Temperatura 4x minimizada",notch = F, pch = "O",
        main="Comparação entre algoritmos..." ,col=c("red" , "green","blue","purple") ,  xaxt="n")
axis(1, at =posx_label, labels = unique(data$h0l0), tick=TRUE , cex=1,out=FALSE)
legend(pos_leg, legend = legenda, col=c("red" , "green","blue","purple"),
       pch = 15, bty = "n", pt.cex = 3, cex = 1.2,  horiz = F, inset = c(0.05, 0.1))

dev.off()