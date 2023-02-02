
library(ggplot2)

datade1 = read.table("de1_data.txt", head = T)
datade2 = read.table("de2_data.txt", head = T)
datade3 = read.table("de3_data.txt", head = T)
datade4 = read.table("de4_data.txt", head = T)

##### Coluna ###
ydata <- 'tmin'

############## Legendas e Títulos ##########
titulo<-paste('H0L0 effect over MSE of ',ydata);
ylab<-ydata;
xlab<-'iters'

############# Plot ##################
#png(paste('de_mesh100_np30_ng100_visited_poits','_',ydata,'.png',sep=""),
#    width=5,
#    height=5,
#   units="in",
#   res=300,
#   pointsize=1)

ggplot()+
  #geom_point(aes(x=data$iter,y=data$alfa),color='blue')+
  #geom_point(aes(x=data$iter,y=data$beta),color='red')+

  geom_point(aes(x=datade3$iter,y=datade3[[ydata]]),color='green',size=3,shape=17)+
  geom_point(aes(x=datade1$iter,y=datade1[[ydata]]),color='blue',size=3,shape=15)+ 
  geom_point(aes(x=datade2$iter,y=datade2[[ydata]]),color='red',size=3,)+ 
  geom_point(aes(x=datade4$iter,y=datade4[[ydata]]),color='orange',size=2.5,shape=18)+
  ylim(0.025, 0.14)+
  scale_x_continuous(breaks = seq(min(datade1$iter), max(datade1$iter), by = 500))+


theme(panel.background = element_blank(),axis.line = element_line(colour = "black"))+
theme(legend.position="top")+
labs(x=xlab, y = ylab, title = titulo)

#dev.off()
