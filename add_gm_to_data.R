#This file will grab GM names and attach them to the modern era data
gm_data<-read.csv('full_dataset.csv')
#for each entry in data, look up corresponding entry in gm_data. Grab the GMs
gm_1<-rep(NA,nrow(data))
gm_2<-rep(NA,nrow(data))
for(i in 1:nrow(data)){
  teams<-c(data$Team1[i],data$Team2[i])
  date<-data$Date[i]
  items<-c(data$Team1.Aquisition[i],data$Team2.Aquisition[i])
  temp.frame<-gm_data[which(gm_data$Team1%in%teams & gm_data$Team2%in%teams &
                              gm_data$Date==date &
                              gm_data$Team1.Aquisition%in%items & gm_data$Team2.Aquisition%in%items),]
  if(nrow(temp.frame)==1){
    gm_1[i]<-temp.frame$GM[1]
  }
  if(nrow(temp.frame)==3){
    gm_1[i]<-temp.frame$GM[1]
    gm_2[i]<-paste(temp.frame$GM[2],temp.frame$GM[3])
  }
  if(nrow(temp.frame)==2){
    gm_1[i]<-temp.frame$GM[1]
    gm_2[i]<-temp.frame$GM[2]
  }
}

data$GM_1<-gm_1
data$GM_2<-gm_2
save(data,file='thesis_data.csv')
