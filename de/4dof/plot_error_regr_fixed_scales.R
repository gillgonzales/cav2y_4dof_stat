  library(tidyverse)
  library(scales)
  #new version to fix scales
  #yscale = seq(min,max,lenght.out = number_ticks)
  plot_error_regr=function(folder,var,d_y,yscale){
    
    
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
    #Dynamic scale according to min and max y values
    #ymin<-min(c(datareg$de1,datareg$de2,datareg$de3))
    #ymax<-max(c(datareg$de1,datareg$de2,datareg$de3))
    #yaxis<-as.numeric(format(c(seq(ymin,ymax,length.out=10),ymax),digits=d_y))
      
    #Fixed scale for comparison with others results
    yaxis<-as.numeric(format(yscale,digits=d_y))
    
    
    
    ############## Legendas e Títulos ##########
    titulo<-paste('MSE of H0L0 effect over ',toupper(var))
    ylab<-paste('MSE')
    xlab<-expression(italic("H")[0]*"/"*italic(L)[0])
    
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
    dirplot<-paste(folder,"plots_d1d2d3","fixed_scale",sep="/")
    if(!dir.exists(dirplot)){
      dir.create(dirplot)
    }
    png(paste(dirplot,paste('fixed_scale_',filename,'_',meth,'.png',sep=""),sep="/"),
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
        if(var=='alfa'){
          myplot = myplot+ scale_y_continuous(yaxis,name=expression('MSE (α)'['2o']*' ×10'^3), limits = c(min(yaxis),max(yaxis)),
                            labels = label_number(scale = 1e-3, accuracy = 1e-1))
        }else{ 
          myplot = myplot+ scale_y_continuous(yaxis,name=expression("MSE (β)"['3o']), limits = c(min(yaxis),max(yaxis)))}
      }else{
     
        if(var=='tmim'){
          formated_name=expression('MSE ('*italic('θ')['max']*')'['3m']*' ×10'^-5)
          formated_label=label_number(scale = 1e5, accuracy = 1e-1)
        }
        else if(var =='s1h0'){
          formated_label=label_number(scale = 1e1, accuracy = 1e-1)
          formated_name=expression('MSE ('*italic('S')[1]*'/'*italic('H')[0]*')'['o']* ' ×10'^-1)
        }
        
        myplot  = myplot+ scale_y_continuous(yaxis,name=formated_name, limits = c(min(yaxis),max(yaxis)),
                                               labels=formated_label)
      }
      myplot  = myplot+ theme(
            panel.background = element_rect(fill = "white", colour = "black"),
            panel.border = element_rect(colour = "black",fill = NA),
            axis.line = element_line(colour = "black"),
            legend.position="top",
            axis.text = element_text(size = 14, colour = "black"),
            text=element_text(size=18,family="serif"))
          # ggtitle(titulo)
      
      myplot
  }
  
  folder<-"np10ng50"
  # plot_error_regr(folder,var='tmim',d_y=3,seq(0,6.2e-5,length.out = 10))#usar tmim com m pois esta errada o nome no csv (tmim_error)
  # dev.off()
  plot_error_regr(folder,var='beta',d_y=1,seq(0,70,length.out = 10))
  dev.off()
  plot_error_regr(folder,var='alfa',d_y=1,seq(0,3500,length.out = 10))
  dev.off()
  plot_error_regr(folder,var='s1h0',d_y=3,seq(0,4.2e-1,length.out = 10))
  dev.off()  