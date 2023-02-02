library(readr)
library(dplyr)
namecsv="csv/log_dados_ex_2dof__04-Jun-2019_23_48_10.csv"
cnames= c("alfa","beta","tmin");
datacsv <- read.csv(namecsv,header = F,col.names=cnames)
datacsv<-filter(datacsv,tmin<1)
alfav = min(datacsv$alfa):1:max(datacsv$alfa);
filtred_effect = c();
for(a in alfav){
  tmpv = filter(datacsv,alfa==a)
  tmn = min(tmpv$tmin)
  filtred_effect=bind_rows(filtred_effect,filter(datacsv,tmin==tmn))
}
colnames(filtred_effect)<-cnames
write.csv(filtred_effect,"filtred_alfa_effect_ex_h1l1h2l2_07.csv",row.names=F)
