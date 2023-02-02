library(readr)
library(dplyr)
rdafolder="np30ng100"

load(paste(rdafolder,"/rda/v_points_de1.rda",sep=""))
v_points_de1<-filter(v_points_de1,v_points_de1$tmin<1)
write.csv(v_points_de1,paste(rdafolder,"/csv/de1_vp_",rdafolder,".csv",sep=""),row.names=T)

load(paste(rdafolder,"/rda/v_points_de2.rda",sep=""))
v_points_de2<-filter(v_points_de2,v_points_de2$tmin<1)
write.csv(v_points_de2,paste(rdafolder,"/csv/de2_vp_",rdafolder,".csv",sep=""),row.names=T)

load(paste(rdafolder,"/rda/v_points_de3.rda",sep=""))
v_points_de3<-filter(v_points_de3,v_points_de3$tmin<1)
write.csv(v_points_de3,paste(rdafolder,"/csv/de3_vp_",rdafolder,".csv",sep=""),row.names=T)

load(paste(rdafolder,"/rda/v_points_de4.rda",sep=""))
v_points_de4<-filter(v_points_de4,v_points_de4$tmin<1)
write.csv(v_points_de4,paste(rdafolder,"/csv/de4_vp_",rdafolder,".csv",sep=""),row.names=T)