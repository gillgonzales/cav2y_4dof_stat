  library(tidyverse)
  library(scales)
  plot_error_regr=function(folder,var,d_y){
    
    
    filename<-paste('error_',var,'_',folder,'_mesh100',sep='');
    filename
    load(file=paste(folder,"rda/stat_data_de1.rda",sep="/"))
    load(file=paste(folder,"rda/stat_data_de2.rda",sep="/"))
    load(file=paste(folder,"rda/stat_data_de3.rda",sep="/"))
    #load(file=paste(folder,"rda/stat_data_de4.rda",sep="/"))
    
    datareg<-data.frame(h0l0=stat_data_de1$h0l0,
                        de1=stat_data_de1[[paste(var,"_error",sep="")]],
                        de2=stat_data_de2[[paste(var,"_error",sep="")]],
                        de3=stat_data_de3[[paste(var,"_error",sep="")]]
                        #de4=stat_data_de4[[paste(var,"_error",sep="")]]
    )
    
    #datareg = read.csv(paste(filename,'.csv',sep=''), head = T)
    meth<-"loess";
    #ymin<-min(c(datareg$de1,datareg$de2,datareg$de3,datareg$de4))
    #ymax<-max(c(datareg$de1,datareg$de2,datareg$de3,datareg$de4))
    ymin<-min(c(datareg$de1,datareg$de2,datareg$de3))
    ymax<-max(c(datareg$de1,datareg$de2,datareg$de3))
    yaxis<-as.numeric(format(c(seq(ymin,ymax,length.out=10),ymax),digits=d_y))
      
    
    ############## Legendas e Títulos ##########
    titulo<-paste('MSE of H0L0 effect over ',toupper(var))
    ylab<-paste('MSE ',toupper(var))
    xlab<-'H0L0'
    
    ############## CORES  #############
    de1pc<-"blue"
    de1sc<-"lightblue"
    
    de2pc<-"red"
    de2sc<-"pink"
    
    de3pc<-"green"
    de3sc<-"lightgreen"
    
    #de4pc<-"orange"
    #de4sc<-"yellow"
    #----------------------------#
    #---Create Dir to save plot image
    dirplot<-paste(folder,"plots_d1d2d3",sep="/")
    if(!dir.exists(dirplot)){
      dir.create(dirplot)
    }
    png(paste(folder,"plots_d1d2d3",paste(filename,'_',meth,'.png',sep=""),sep="/"),
        width=5,
        height=5,
        units="in",
        res=300,
        pointsize=1)
    
    myplot = ggplot(datareg)
      
      myplot  = myplot+ geom_point(aes(x = h0l0,y=de1), color=de1pc)+
      #geom_line(aes(x = h0l0,y=de1), color=de1sc)+
      
      geom_point(aes(x = h0l0,y=de2), color=de2pc)+
      #geom_line(aes(x = h0l0,y=de2), color=de2sc)+
      
      geom_point(aes(x = h0l0,y=de3), color=de3pc)+
      #geom_line(aes(x = h0l0,y=de3), color=de4sc)+
      
     # geom_point(aes(x = h0l0,y=de4), color=de4pc)+
      #geom_line(aes(x = h0l0,y=de4), color=de4sc)+
      
      
      geom_smooth(aes(x = h0l0,y=de1),color = de1pc, se = FALSE, method = meth)+
      geom_smooth(aes(x = h0l0,y=de2),color = de2pc, se = FALSE, method =  meth)+
      geom_smooth(aes(x = h0l0,y=de3),color = de3pc, se = FALSE, method =  meth,linetype="dashed")+
      #geom_smooth(aes(x = h0l0,y=de4),color = de4pc, se = FALSE, method =  meth,linetype="dashed")+
      
      #geom_smooth(aes(x = h0l0,y=de1),color = de1pc, se = FALSE, method = "lm", formula = y ~ splines::bs(x,3))+
      #geom_smooth(aes(x = h0l0,y=de2),color = de2pc, se = FALSE, method = "lm", formula = y ~ splines::bs(x,3))+
      #geom_smooth(aes(x = h0l0,y=de3),color = de3pc, se = FALSE, method = "lm", formula = y ~ splines::bs(x,3))+
      #eom_smooth(aes(x = h0l0,y=de4),color = de4pc, se = FALSE, method = "lm", formula = y ~ splines::bs(x,3))+
      scale_x_continuous(name=xlab,as.numeric(format(datareg$h0l0,digits=2)))
      
      if(d_y==1){
        myplot = myplot+ scale_y_continuous(name=toupper(var), yaxis)
      }else{
        #myplot  = myplot+ scale_y_continuous(name=toupper(var), yaxis,labels = scales::scientific_format(digits = d_y))
        myplot  = myplot+ scale_y_continuous(name=toupper(var), yaxis,labels = function(x){
            return(parse(text=gsub("e\\+*", "%*% 10^", scientific_format()(x))))
          })
      }
      myplot  = myplot+ theme(panel.background = element_rect(fill = "white", colour = "grey50"),
            panel.border = element_rect(colour = "black",fill = NA),
            axis.line = element_line(colour = "black"))+
      theme(legend.position="top")+
      labs(x=xlab, y = ylab, title = titulo)
      #return(yaxis)
      myplot
      
  }
  
  folder<-"np40ng112"
  plot_error_regr(folder,var='tmim',d_y=3)#usar tmim com m pois esta errada o nome no csv (tmim_error)
  dev.off()
  plot_error_regr(folder,var='beta',d_y=1)
  dev.off()
  plot_error_regr(folder,var='alfa',d_y=1)
  dev.off()
  plot_error_regr(folder,var='s1h0',d_y=3)
  dev.off()  