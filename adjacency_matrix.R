#make adjacency matrix

#for every season
  #for every team
    #for every other team
      #1 if team i,j traded that year
      #1 if i=j
      #0 elsewise
seasons<-names(table(data_for_thesis$Season))
teams<-union(names(table(data_for_thesis$Team1)),
             names(table(data_for_thesis$Team2)))
#teams<-teams[-c(1,51)]
#a.mat<-array(rep(0,length(seasons)*length(teams)*length(teams)),
#             dim=c(length(teams),length(teams),length(seasons)))
#for(i in 1:length(teams)){
#  for(j in 1:length(teams)){
#    for(t in 1:length(seasons)){
#      if(length(which(data_for_thesis$Team1==teams[i] & data_for_thesis$Team2==teams[j] & data_for_thesis$Season==seasons[t]))+
#         length(which(data_for_thesis$Team1==teams[j] & data_for_thesis$Team2==teams[i] & data_for_thesis$Season==seasons[t]))){
#        a.mat[i,j,t]<-1
#      }
#    }
#  }
#}

a.mat.weighted<-array(rep(0,length(seasons)*length(teams)*length(teams)),
             dim=c(length(teams),length(teams),length(seasons)))
for(i in 1:nrow(data_for_thesis)){
  team1<-which(teams==data_for_thesis$Team1[i])[1]
  team2<-which(teams==data_for_thesis$Team2[i])[1]
  season<-which(seasons==data_for_thesis$Season[i])[1]
  a.mat.weighted[team1,team2,season]<-a.mat.weighted[team1,team2,season]+1
  a.mat.weighted[team2,team1,season]<-a.mat.weighted[team2,team1,season]+1
}

save(a.mat.weighted,file='Weighted Adjacency Matrix, Modern Era.Rdata')
save(a.mat,file='Adjacency_Matrix.Rdata')