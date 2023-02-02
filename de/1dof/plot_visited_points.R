
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggformula)
library(ggforce)

plot_visited_points = function(rdafolder,ydata){
  
load(file=paste(rdafolder,"rda/v_points_de1.rda",sep="/"))
datade1 = filter(v_points_de1,tmin<1)
load(file=paste(rdafolder,"rda/v_points_de2.rda",sep="/"))
datade2 = filter(v_points_de2,tmin<1)
load(file=paste(rdafolder,"rda/v_points_de3.rda",sep="/"))
datade3 = filter(v_points_de3,tmin<1)
load(file=paste(rdafolder,"rda/v_points_de4.rda",sep="/"))
datade4= filter(v_points_de4,tmin<1)


############## Legendas e Títulos ##########
titulo<-paste('Visited Points',ydata);
ylab<-ydata;
xlab<-'iters'

############# Plot ##################
#---Create Dir to save plot image
dirplot<-paste(rdafolder,"plots","visited_points",sep="/")
if(!dir.exists(dirplot)){
  dir.create(dirplot,recursive = T)
}
png(paste(dirplot,paste('de_mesh_100_',rdafolder,'_visited_poits','_',ydata,'.png',sep=""),sep="/"),
   width=5,
   height=5,
  units="in",
  res=300,
  pointsize=1)

de1iters=c(1:1:length(datade1[[ydata]]))
de2iters=c(1:1:length(datade2[[ydata]]))
de3iters=c(1:1:length(datade3[[ydata]]))
de4iters=c(1:1:length(datade4[[ydata]]))

gplot <- ggplot()+
  #geom_point(aes(x=data$iter,y=data$alfa),color='blue')+
  #geom_point(aes(x=data$iter,y=data$beta),color='red')+
  
  geom_point(aes(x=de3iters,y=datade3[[ydata]]),color='green',size=3,shape=17,alpha = 1/3)+
  geom_point(aes(x=de1iters,y=datade1[[ydata]]),color='blue',size=3,shape=15,alpha = 1/3)+ 
  geom_point(aes(x=de4iters,y=datade4[[ydata]]),color='red',size=3,alpha = 1/3)+ 
  geom_point(aes(x=de2iters,y=datade2[[ydata]]),color='orange',size=2.5,shape=18,alpha = 1/3)+
  scale_x_continuous(breaks = seq(min(de1iters), max(de2iters), by = 500))+

  theme_bw()+
  theme(legend.position = "none",
        text = element_text(size=10,family="Serif"),
        panel.grid= element_line(colour = "white"))

if(ydata == 'tmin'){
  gplot=gplot+ylim(0, 0.15)
}else{
  if(ydata == 's1h0'){
    gplot=gplot+scale_y_continuous(breaks = seq(0, 1, by = 0.1))
  }
}

gplot=gplot+theme(legend.position="top")+
labs(x=xlab, y = ylab, title = titulo)
gplot
}

rdafolder="np40ng150"

plot_visited_points(rdafolder,ydata='alfa')
dev.off()
plot_visited_points(rdafolder,ydata='beta')
dev.off()
plot_visited_points(rdafolder,ydata='s1h0')
dev.off()
plot_visited_points(rdafolder,ydata='tmin')
dev.off()

