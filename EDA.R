### EDA ###
setwd("C:/Users/Tony/Desktop/Hannah_Thessis_Masters")
## trades / year histogram
plot(table(data_for_thesis$Season),ylab='Number of Trades')
## Total trades / team
plot(table(data_for_thesis$Team1) + table(data_for_thesis$Team2),ylab='Number of Trades')
hist(table(data_for_thesis$Team1) + table(data_for_thesis$Team2),xlab='Total Team Trades, Modern Era',main="")
##matrix of trades between two teams over all years in dataset
total.trades<-matrix(apply(a.mat.weighted,c(1,2),sum),nrow=31)
colnames(total.trades)<-names(table(data_for_thesis$Team1))
rownames(total.trades)<-names(table(data_for_thesis$Team1))
library(gplots)
heatmap.2(x = total.trades, Rowv = FALSE, Colv = FALSE, dendrogram = "none",
          cellnote = total.trades, notecol = "black", notecex = .5,
          trace = "none", key = FALSE, margins = c(7, 11))

#is this possibly random?
hist(total.trades,breaks=15)
#poison distribution
#mle: y bar
#2.605
#plot distribution of pois(2.605)
points(seq(1,15),31*31*dpois(seq(1,15),2.605),col='red',type='l')
#evidence of not being poison
#what is the formal test again?
#could do this by team as well. Does every team just have their own lamda?
ducks<-total.trades[-1,1]
#mle: 5.03
hist(ducks,breaks=10)
points(seq(1,15),30*dpois(seq(1,15),5.03),col='red',type='l')

probs=dpois(as.numeric(names(table(ducks))),5.03)
comp=1-sum(probs)
chisq.test(x=c(table(ducks),0),p=c(probs,comp),simulate.p.value = T)

#let's do this for every team
p.vals<-rep(NA,31)
means<-rep(NA,31)
vars<-rep(NA,31)
for(i in 1:31){
  print(i)
  te<-total.trades[-i,i]
  means[i]<-mean(te)
  vars[i]<-var(te)
  probs=dpois(as.numeric(names(table(te))),mean(te))
  comp=1-sum(probs)
  p.vals[i]<-chisq.test(x=c(table(te),0),p=c(probs,comp),simulate.p.value = T)$p.val
}
plot(means,vars)
abline(0,1)
rownames(total.trades)[which(p.vals<.05/30)]
rownames(total.trades)[which(p.vals<.05)]

#is there cor between #trades a team does in year i and #trades a team does in year j?
yearly.trades.by.team<-matrix(apply(a.mat.weighted,c(1,3),sum),nrow=31)
rownames(yearly.trades.by.team)<-names(table(data_for_thesis$Team1))
colnames(yearly.trades.by.team)<-seasons
cor((yearly.trades.by.team))
