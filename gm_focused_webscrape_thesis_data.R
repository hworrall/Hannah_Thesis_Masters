#pull all links
library(XML)
library(stringr)
url<-'http://nhltradetracker.com/user/GM_list'
doc<-htmlParse(url)
links<-xpathSApply(doc,"//a/@href")
free(doc)

#get each url
urls<-rep(NA,length(links))
for(i in 1:length(links)){
  urls[i]<-paste('http://nhltradetracker.com',links[i],'/',sep='')
}

#kick out the ones not related to GMS
urls<-urls[-c(1:9,293:295)]

#get the GM name
gms<-str_sub(urls,50)
gms<-str_sub(gms,1,str_locate(gms,'/')-1)


extract_gm_data<-function(gm_url){
  page.num<-1
  df<-data.frame('Team1','Team2','Team1 Aquisition','Team2 Aquisition','Date')
  names(df)<-c('team1','team2','team1_aq','team2_aq','date')
  url<-paste(gm_url,as.character(page.num),sep='')
  page.has.data<-!page_empty(url)
  while(page.has.data==TRUE){
    page.element.num<-elements_on_page(url)
    for(i in 1:page.element.num){
      xpath.for.el<-paste('//*[@id="container"]/table[',as.character(i),']',sep='')
      dat<-extract_data_element(url,xp=xpath.for.el)
      df<-rbind(df,t(unlist(dat)))
    }
    page.num<-page.num+1
    url<-paste(gm_url,as.character(page.num),sep='')
    page.has.data<-!page_empty(url)
  }
  names(df)<-c('Team1','Team2','Team1 Aquisition','Team2 Aquisition','Date')
  df<-df[-1,]
  GM<-str_sub(gm_url,50)
  df$GM<-rep(str_sub(GM,1,str_locate(GM,'/')-1)[1],nrow(df))
  return(df)
}

# temp<-extract_gm_data(urls[1])

data_for_thesis<-extract_gm_data(urls[1])
for(i in urls[-1]){
  print(i)
  data_for_thesis<-rbind(data_for_thesis,extract_gm_data(i))
}
#breaks on Jim Clark
#moving on
for(i in urls[59:283]){
  print(i)
  data_for_thesis<-rbind(data_for_thesis,extract_gm_data(i))
}
for(i in urls[84:283]){
  print(i)
  data_for_thesis<-rbind(data_for_thesis,extract_gm_data(i))
}

for(i in urls[147:283]){
  print(i)
  data_for_thesis<-rbind(data_for_thesis,extract_gm_data(i))
}

write.csv(data_for_thesis,file='GM_data.csv',row.names = F)


#get season for each trade
#seasons defined as xx-yy

Month_loc<-str_locate(data_for_thesis$Date,' ')[,1]
data_for_thesis$Month<-str_sub(data_for_thesis$Date,1,Month_loc)
data_for_thesis$Year<-str_sub(data_for_thesis$Date,start=str_length(data_for_thesis$Date)-3)
Season<-rep(NA,nrow(data_for_thesis))
for(i in 1:nrow(data_for_thesis)){
  if(data_for_thesis$Month[i]%in%c('January ','February ','March ','April ')){
    Season[i]<-paste(as.character(as.numeric(data_for_thesis$Year[i])-1),'-',as.character(data_for_thesis$Year[i]),sep='')
  }
  else{
    Season[i]<-paste(as.character(data_for_thesis$Year[i]),'-',as.character(as.numeric(data_for_thesis$Year[i])+1),sep='')
  }
}

data_for_thesis$Season<-Season

write.csv(data_for_thesis,file='full_dataset.csv',row.names=F)
