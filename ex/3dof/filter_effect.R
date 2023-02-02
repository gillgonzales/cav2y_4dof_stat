library(readr)
library(dplyr)
namecsv="csv/log_dados_ex_3dof__21-Jul-2019_4_30_5_simulated_.csv"
cnames= c("h0l0", "h2l2min", "h1l1min", "s1h0", "alfa", "beta","tmin");
datacsv <- read.csv(namecsv,header = F,col.names=cnames)
datacsv<-filter(datacsv,datacsv$tmin<1)
s1h0 = seq(min(datacsv$s1h0),max(datacsv$s1h0),by=0.1)
filtred_effect = c();
for(s1 in s1h0){
  # print(s1)
  tmpv = filter(datacsv,s1h0==as.numeric(s1))
  # print(tmpv)
  tmn = min(tmpv$tmin)
  # print(tmn) 
  filtred_effect=bind_rows(filtred_effect,filter(datacsv,tmin==tmn))
}
colnames(filtred_effect)<-cnames
write.csv(filtred_effect,"filtred_3dof_s1h0_effect_ex.csv",row.names=F)
