library(tidyverse)

plot_error_all_values = function(rdafolder,ydata){
  
  ############## Legendas e Títulos ##########
  titulo<-paste('H0L0 effect over MSE of ',ydata);
  ylab<-paste('(',ydata,')o');
  xlab<-'H0L0'
  
  ############## CORES  #############
  de1pc<-"green"
  de1sc<-"lightgreen"
  
  de2pc<-"red"
  de2sc<-"pink"
  
  de3pc<-"blue"
  de3sc<-"lightblue"
  
  de4pc<-"orange"
  de4sc<-"sandybrown"
  #----------------------------#
  
  load(file=paste(rdafolder,"rda/all_data_de1.rda",sep="/"))
  load(file=paste(rdafolder,"rda/all_data_de2.rda",sep="/"))
  load(file=paste(rdafolder,"rda/all_data_de3.rda",sep="/"))
  load(file=paste(rdafolder,"rda/all_data_de4.rda",sep="/"))
  data_de1<-all_data_de1
  data_de2<-all_data_de2
  data_de3<-all_data_de3
  data_de4<-all_data_de4
  rm(list=c("all_data_de1","all_data_de2","all_data_de3","all_data_de4"))
  
  load(file=paste(rdafolder,"rda/stat_data_de1.rda",sep="/"))
  load(file=paste(rdafolder,"rda/stat_data_de2.rda",sep="/"))
  load(file=paste(rdafolder,"rda/stat_data_de3.rda",sep="/"))
  load(file=paste(rdafolder,"rda/stat_data_de4.rda",sep="/"))
  
  data_de_min <- stat_data_de1
  for( i in 1:length(data_de_min$h0l0)){
    if(data_de_min$tmin_min[i]>stat_data_de2$tmin_min[i])
      data_de_min[i,] <- as.data.frame(stat_data_de2[i,])
    if(data_de_min$tmin_min[i]>stat_data_de3$tmin_min[i])
      data_de_min[i,] <- as.data.frame(stat_data_de3[i,])
    if(data_de_min$tmin_min[i]>stat_data_de4$tmin_min[i])
      data_de_min[i,] <- as.data.frame(stat_data_de4[i,])
  }
  rm(list=c("stat_data_de1","stat_data_de2","stat_data_de3","stat_data_de4"))
  
  png(paste(rdafolder,"plots",paste('de_mesh100_',rdafolder,'_all_values_smooth','_',ydata,'.png',sep=""),sep="/"),
      width=5,
      height=5,
      units="in",
      res=300,
      pointsize=1)
  
  if(ydata=="tmin"){
    ydataopt<-paste(ydata,"_min",sep="");
  }else{
    ydataopt<-paste(ydata,"_o",sep="");
  }
  #data=data.frame(spline(data_de_min$h0l0,data_de_min[[ydataopt]]))
  data=data.frame(x=data_de_min$h0l0,y=data_de_min[[ydataopt]])
  ggplot(colour=)+
    geom_line(data=data,aes(x=data$x,y=data$y),color='black',size=2)+
    #geom_smooth(aes(x = data_de_min$h0l0+1,y=data_de_min$tmin),color = 'black',
    #se = FALSE, method = "lm", formula = y ~ splines::bs(x,3),
    #size=3,show.legend=TRUE)+
    
    #geom_point(aes(x =data_de1$h0l0,y=data_de1[[ydata]],color=tmin), color=de1pc)+
    #geom_point(aes(x = data_de2$h0l0+0.5,y=data_de2[[ydata]]), color=de2pc)+
    #geom_point(aes(x =data_de3$h0l0+1,y=data_de3[[ydata]]), color=de3pc)+
    #geom_point(aes(x = data_de4$h0l0+1.5,y=data_de4[[ydata]]), color=de4pc)+
    #stat_summary(
    # mapping = aes(x =data_de1$h0l0, y = data_de1[[ydata]]),
    #  fun.ymin = min,
  # fun.ymax = max,
  #  fun.y = median,
  #  color=de1pc
  #)+
  
  #geom_line(aes(x = h0l0,y=min(data_de1[[ydata]])), color=de1sc)+
  #geom_line(aes(x = h0l0,y=de4), color=de4sc)+
  
  #geom_smooth(aes(x = h0l0,y=tmin),color = de1pc, se = FALSE, method = "lm")+
  
  geom_smooth(aes(x = data_de1$h0l0+1,y=data_de1[[ydata]]),color = de1pc, se = TRUE, method = "lm", formula = y ~ splines::bs(x,3))+
    
    ### DE2 ###
    # geom_point(aes(x = data_de2$h0l0+0.5,y=data_de2[[ydata]]), color=de2sc)+
    #stat_summary(
    #mapping = aes(x =data_de2$h0l0+0.5, y = data_de2[[ydata]]),
    #fun.ymin = min,
    #fun.ymax = max,
    #fun.y = median,
    #color=de2pc
    #  )+
    
  geom_smooth(aes(x = data_de2$h0l0+1,y=data_de2[[ydata]]),color = de2pc, se = TRUE, method = "lm", formula = y ~ splines::bs(x,3))+
    
    ### DE3 ###
    # geom_point(aes(x =data_de3$h0l0+1,y=data_de3[[ydata]]), color=de3sc)+
    #stat_summary(
    #  mapping = aes(x =data_de3$h0l0+1, y = data_de3[[ydata]]),
    #  fun.ymin = min,
    #  fun.ymax = max,
    #  fun.y = median,
    # color=de3pc
    #)+
    
  geom_smooth(aes(x = data_de3$h0l0+1,y=data_de3[[ydata]]),color = de3pc, se = TRUE, method = "lm", formula = y ~ splines::bs(x,3),linetype="dashed")+
    
    ### DE4 ###
    #geom_point(aes(x = data_de4$h0l0+1.5,y=data_de4[[ydata]]), color=de4sc)+
    #stat_summary(
    # mapping = aes(x =data_de4$h0l0+1.5, y = data_de4[[ydata]]),
    #  fun.ymin = min,
    #  fun.ymax = max,
    #  fun.y = median,
    # color=de4pc
    #)+
    
  geom_smooth(aes(x = data_de4$h0l0+1,y=data_de4[[ydata]]),color = de4pc, se = TRUE, method = "lm", formula = y ~ splines::bs(x,3),linetype="dashed")+
    
    #--------------
  theme(panel.background = element_blank(),axis.line = element_line(colour = "black"))+
    theme(legend.position="top")+
    labs(x=xlab, y = ylab, title = titulo)
}
rdafolder<-"np40ng75" #Change the folder name for other analysis

plot_error_all_values(rdafolder,ydata = 's1h0')
dev.off()
plot_error_all_values(rdafolder,ydata = 'beta')
dev.off()
plot_error_all_values(rdafolder,ydata = 'alfa')
dev.off()
plot_error_all_values(rdafolder,ydata = 'tmin')
dev.off()