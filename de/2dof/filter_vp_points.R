library(readr)
library(dplyr)

rdafolder<-"np30ng50" #Set the folder name
algo = 'de4'
load(file=paste(rdafolder,"/rda/v_points_",algo,".rda",sep=""))
vp_all_de = v_points_de4
vp_de=filter(vp_all_de,tmin<1)

alfav = min(vp_de$alfa):1:max(vp_de$alfa);
filtred_effect = c();
for(a in alfav){
  tmpv = filter(vp_de,alfa==a)
  tmn = min(tmpv$tmin)
  filtred_effect=bind_rows(filtred_effect,filter(vp_de,tmin==tmn))
}
colnames(filtred_effect)<-colnames(vp_all_de)
write.csv(vp_de,file=paste(rdafolder,"/csv/vp_",algo,".csv",sep=""),row.names=F)
write.csv(vp_de,file=paste(rdafolder,"/csv/vp_",algo,"_iter.csv",sep=""),row.names=T,)
write.csv(filtred_effect,file=paste(rdafolder,"/csv/filtred_a_effect_vp_",algo,".csv",sep=""),row.names=F)
