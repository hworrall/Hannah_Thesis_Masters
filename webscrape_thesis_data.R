library(rvest)
library(stringr)
url<-'http://nhltradetracker.com/user/trade_list_by_season/2016-17/5'
xp<-'//*[@id="container"]/table[3]'


extract_data_element<-function(url,xp){
  page<-read_html(url)
  page_element<-html_nodes(page,xpath=xp)
  temp_tab<-html_table(page_element,header=T,trim=T,fill=T)[[1]]
  team1<-names(temp_tab)[1]
  team2<-names(temp_tab)[3]
  date<-temp_tab[1,4]
  team1_aq<-temp_tab[1,1]
  team2_aq<-temp_tab[3,1]
  team1_aq<-gsub('\n','',team1_aq)
  team1_aq<-gsub('\t','',team1_aq)
  team2_aq<-gsub('\n','',team2_aq)
  team2_aq<-gsub('\t','',team2_aq)
  team1_aq<-gsub("\\s\\s+", ",", str_trim(team1_aq))
  team2_aq<-gsub("\\s\\s+", ",", str_trim(team2_aq))
  team1<-gsub(' acquire','',team1)
  team2<-gsub(' acquire','',team2)
  return(list(team1=team1,team2=team2,team1_aq=team1_aq,team2_aq=team2_aq,date=date))
}

page_empty<-function(url){
  xp<-'//*[@id="container"]/table[1]'
  page<-read_html(url)
  page_element<-html_nodes(page,xpath=xp)
  temp_tab<-html_table(page_element,header=T,trim=T,fill=T)
  return(length(temp_tab)==0)
}

#max is 20
elements_on_page<-function(url){
  for(i in 20:1){
    xp<-paste('//*[@id="container"]/table[',as.character(i),']',sep='')
    page<-read_html(url)
    page_element<-html_nodes(page,xpath=xp)
    temp_tab<-html_table(page_element,header=T,trim=T,fill=T)
    if(length(temp_tab)!=0){
      return(i)
    }
  }
  return(0)
}

extract_year_of_data<-function(year){
  page.num<-1
  df<-data.frame('Team1','Team2','Team1 Aquisition','Team2 Aquisition','Date')
  names(df)<-c('team1','team2','team1_aq','team2_aq','date')
  url<-paste('http://nhltradetracker.com/user/trade_list_by_season/',year,'/',as.character(page.num),sep='')
  page.has.data<-TRUE
  while(page.has.data==TRUE){
    page.element.num<-elements_on_page(url)
    for(i in 1:page.element.num){
      xpath.for.el<-paste('//*[@id="container"]/table[',as.character(i),']',sep='')
      dat<-extract_data_element(url,xp=xpath.for.el)
      df<-rbind(df,t(unlist(dat)))
    }
    page.num<-page.num+1
    url<-paste('http://nhltradetracker.com/user/trade_list_by_season/',year,'/',as.character(page.num),sep='')
    page.has.data<-!page_empty(url)
  }
  names(df)<-c('Team1','Team2','Team1 Aquisition','Team2 Aquisition','Date')
  df<-df[-1,]
  return(df)
}

#test
#year<-'1959-60' #works
#year<-'1982-83' #works
#extract_year_of_data(year)
years<-c('2005-06',
         '2006-07',
         '2007-08',
         '2008-09',
         '2009-10',
         '2010-11',
         '2011-12',
         '2012-13',
         '2013-14',
         '2014-15',
         '2015-16',
         '2016-17')

df<-extract_year_of_data(years[1])
for(i in years[-1]){
  print(i)
  df<-rbind(df,extract_year_of_data(i))
}
data<-df
setwd("//enova.com/corp/profiles/hworrall/Desktop/Uchicago/thesis/")

Month_loc<-str_locate(data$Date,' ')[,1]
data$Month<-str_sub(data$Date,1,Month_loc)
data$Year<-str_sub(data$Date,start=str_length(data$Date)-3)
Season<-rep(NA,nrow(data))
for(i in 1:nrow(data)){
  if(data$Month[i]%in%c('January ','February ','March ','April ')){
    Season[i]<-paste(as.character(as.numeric(data$Year[i])-1),'-',as.character(data$Year[i]),sep='')
  }
  else{
    Season[i]<-paste(as.character(data$Year[i]),'-',as.character(as.numeric(data$Year[i])+1),sep='')
  }
}

data$Season<-Season

write.csv(data,file='thesis_data_raw.csv',row.names=F)
