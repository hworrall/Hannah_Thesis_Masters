#need to turn a_mat into another data structure. data table with:
teams<-c(names(table(data$Team1)),'Winnipeg Jets')
seasons<-names(table(data$Season))
#Team 1 Team 2 Season Connection_Strength 
model.frame<-data.frame(team.1=NA,team.2=NA,season=NA,connection_strength=NA)
counter<-1
for(i in 1:31){
  for(j in i:31){
    for(k in 1:12){
      team1<-teams[i]
      team2<-teams[j]
      season<-seasons[k]
      connection<-a.mat.weighted[i,j,k]
      if(team1!=team2){
        model.frame[counter,]<-c(team1,team2,season,connection)
        counter<-counter+1
      }
    }
  }
}
rm(i);rm(j);rm(k);rm(team1);rm(team2);rm(season);rm(connection);rm(counter)
model.frame$connection_strength<-as.numeric(model.frame$connection_strength)
model.frame$trade<-ifelse(model.frame$connection_strength>0,1,0)
model.frame$year_one_lag<-rep(NA,nrow(model.frame))
#need lag variables. Let's start with a one-year lag
for(i in 1:nrow(model.frame)){
  team1.index<-which(teams==model.frame$team.1[i])[1]
  team2.index<-which(teams==model.frame$team.2[i])[1]
  season.index<-which(seasons==model.frame$season[i])[1]
  if(season.index!=1){
    model.frame$year_one_lag[i]<-a.mat.weighted[team1.index,team2.index,season.index-1]
    model.frame$team.1_trade_volume[i]<-apply(a.mat.weighted[,,season.index],1,sum)[team1.index]
    model.frame$team.2_trade_volume[i]<-apply(a.mat.weighted[,,season.index],2,sum)[team2.index]
    model.frame$team.1_trade_volume_last_year[i]<-apply(a.mat.weighted[,,season.index-1],1,sum)[team1.index]
    model.frame$team.2_trade_volume_last_year[i]<-apply(a.mat.weighted[,,season.index-1],2,sum)[team2.index]
    graph.object<-graph_from_adjacency_matrix(a.mat.weighted[,,season.index])
    graph.object_last<-graph_from_adjacency_matrix(a.mat.weighted[,,season.index-1])
    model.frame$distance[i]<-distances(graph.object)[team1.index,team2.index]
    model.frame$distance_last_year[i]<-distances(graph.object_last)[team1.index,team2.index]
    }
}
head(model.frame)
model.frame<-na.omit(model.frame)
write.csv(model.frame,file='modeling.frame.csv')
