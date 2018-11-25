### transmission frequency ## time diff?
# load raw datasets
### Output table
# Create the different combination of gears and years
inputTable <- expand.grid(source=c('AIS','VMS'),Year=c(2016,2017), Gear=c("LL", "PS","SV"))
# define where your data are
pathToData <- "final_data/AUG/"

df_timediff<-NULL
for (i in 1:dim(inputTable)[1]){
data_csv<-read.csv(paste0(pathToData,inputTable$Gear[i],inputTable$source[i],inputTable$Year[i],'_processed.csv'))
# remove values above the 80th percentile for histograms
q99<-quantile(data_csv$timediff,probs=0.9,na.rm=T)
data_csv<-data_csv[-which(data_csv$timediff>as.numeric(q99)),]
# combine timediff columns of different data sources and gears into in df (remove year)
df_timediff<-rbind(df_timediff,data.frame(timediff=data_csv$timediff,source=paste0(inputTable$source[i]),gear=inputTable$Gear[i]))
}

# reorder df so that VMS is plotted first
neworder <- c("VMS","AIS")
library(plyr)  ## or dplyr (transform -> mutate)
df_timediff2 <- arrange(transform(df_timediff,
                           source=factor(source,levels=neworder)),source)
df_timediff2$timediff<-df_timediff2$timediff/60
df_timediff2$plot<-NA
df_timediff2[which(df_timediff2$source=='VMS'),'plot']<-'A'
df_timediff2[which(df_timediff2$source=='AIS'),'plot']<-'B'

# label_panels=c("A","B")
## histogram of transmission frequency (difference in time between transmissions)
yticklabs<-as.character(seq(5,20,5))
ggplot(df_timediff2, aes(timediff,group=gear)) +
  geom_histogram()+xlab("Time between transmissions (min)")+ylab("Frequency x 10,000") + 
  facet_wrap(~plot, scales = "free_x")+xlim(c(0,65))+
  theme_economist_white()+
  scale_y_continuous(breaks=seq(50000,200000,50000),labels = yticklabs)
   # scale_y_continuous(breaks = seq(50000,250000,by=50000),
                       # labels = yticklabs)
# # ggsave(paste0(pathToData, "TransmissionFrequency.png"))
check<-df_timediff2[which(df_timediff2=='AIS'),'timediff']

## medians of transmission frequencies for 
# VMS
median(df_timediff2[df_timediff2$source=='VMS','timediff'],na.rm=T)
# AIS
median(df_timediff2[df_timediff2$source=='AIS','timediff'],na.rm=T)

# hist(data_csv$timediff,breaks=100000,xlim=quantile(data_csv$timediff,probs=c(0,0.8),na.rm=T),main=paste0(inputTable$Gear[i],inputTable$source[i],inputTable$Year[i]))
# hist(data_csv$timediff,xlim=c(0,3600),breaks=100000,main=paste0(inputTable$Gear[i],inputTable$source[i],inputTable$Year[i]))
