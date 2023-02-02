#---Configurações do Script

np<-10
ng<-50
folder<-paste("np",np,"ng",ng,sep="")#seguir próximos npng
pos_leg<-"toprigh"
legenda<-c(paste("NP =",np),paste("NG =",ng),"DE1", "DE2","DE3")
cores_legenda<-c("white","white","blue","red" , "green")
cores<-c("blue","red" , "green")

load(file=paste(folder,"rda/stat_data_de1.rda",sep="/"))
load(file=paste(folder,"rda/stat_data_de2.rda",sep="/"))
load(file=paste(folder,"rda/stat_data_de3.rda",sep="/"))

data<-c()
h0l0<-stat_data_de1$h0l0
for(i in 1:length(h0l0)){
  data<-rbind(data,c(h0l0[i],"de1",stat_data_de1$tmin_med[i],stat_data_de1$tmin_std[i]))
  data<-rbind(data,c(h0l0[i],"de2",stat_data_de2$tmin_med[i],stat_data_de2$tmin_std[i]))
  data<-rbind(data,c(h0l0[i],"de3",stat_data_de3$tmin_med[i],stat_data_de3$tmin_std[i]))
}

data<-data.frame(
  h0l0=as.numeric(data[,1]),
  algo=as.factor(data[,2]),
  tmed=as.numeric(data[,3]),
  tstd=as.numeric(data[,4])
)

##VARIAVEIS DE CONFIGURACAO - ALTERAR PARA GERAR GRAFICOS PARA DADOS DIFERENTES
#algo<-"deall50" # mudar valor para alterar a pasta de origem dos dados e salvamento das imagens
#dados<-"rdata_std_003-05" # alterar nome do arquivo com os dados fonte
# legenda<-c("DE4(50)", "DE4(100)","DE4(150)","DE4(300)") #alterar para aplicar legenda diferente

# cores<-c("red" , "green","blue","orange","purple") #alterar

# posx_lab<-seq(1,76,4)#all
# posx_lab<-seq(1,95,5)#all
# posx_lab<-seq(1,42,4.2)
# posx_lab<-seq(2.25,24.5,2.47)
# posx_lab<-seq(1,54,6)
# pos_leg<-"topright"

posx_lab<-seq(1.5,31,3.1)

##SCRIPT DE GERACAO DO GRAFICO
#data = read.table(paste(algo,'/',dados,sep=""), head = T)

avg<-data$tmed
sdev<-data$tstd

n=length(data$h0l0)
x <- 1:n

png(filename=paste(folder,"plots_d1d2d3","fixed_scale",paste('plot_std_all_tmin_',folder,'.png',sep=""),sep="/"),
	width=4.5,
	height=3.5,
	units="in",
	res=300,
	pointsize=3)

par(
  cex.lab=5,
  cex.axis=3.5,
  cex.main=5,
  pty='m',
  las=1,
  mai=c(0.35,0.55,0.01,0.01),
  mgp=c(5.5,1.5,0),
  family = "serif"
  )
yscale<-seq(0.015,0.04,0.005);
plot(x, avg,
    # ylim=range(c(avg-sdev, avg+sdev)),
    #ylim=range(c(avg-sdev,  avg+sdev)),
    ylim=range(yscale),
    pch=19, 
    xlab=expression(italic("H")[0]*"/"*italic(L)[0]),
    ylab=expression('('*italic('θ')['max']*')'['3m']*' ×10'^-2),
    xaxt="n",
    yaxt="n",
    col=cores,
    lwd=5
)

axis(1, at =posx_lab, labels = format(unique(data$h0l0), digits=2),tick = TRUE, out=FALSE)
axis(2, at=yscale, labels=sprintf("%1.1f", yscale*1e2))

arrows(x, avg-sdev, x, avg+sdev, length=0.05, angle=90, code=3,col=cores,lwd=2.5)

legend(pos_leg, legend = legenda, col=cores_legenda,pch = 19,
		 bty = "n", pt.cex = 6, cex = 4,  horiz = F, inset = c(0.05, 0.01))

dev.off()
