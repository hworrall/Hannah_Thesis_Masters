simple.log<-glm(trade~year_one_lag,data=model.frame,family=binomial)
summary(simple.log)
mse<-function(real,predicted){
  return(mean((real-predicted)^2))
}
mse(model.frame$trade,predict(simple.log,type='response'))
mse(model.frame$trade,rep(.1939,nrow(model.frame)))
plot(simple.log$fitted.values,simple.log$residuals)

simple.poisson<-glm(connection_strength~year_one_lag,data=model.frame,family=poisson)
summary(simple.poisson)

#dumb way to deal with team variation
team.dumb.log<-glm(trade~year_one_lag+team.1+team.2,data=model.frame,family=binomial)
summary(team.dumb.log)

team.dumb.poisson<-glm(connection_strength~year_one_lag+team.1+team.2,data=model.frame,family=poisson)
summary(team.dumb.poisson)

#so maybe the significance is due to the team's typical trading volume

vol.log<-glm(trade~year_one_lag+team.1_trade_volume+team.2_trade_volume,data=model.frame,family=binomial)
summary(vol.log)
vol.poi<-glm(connection_strength~year_one_lag+team.1_trade_volume+team.2_trade_volume,data=model.frame,family=poisson)
summary(vol.poi)

#do teams add value to model?
library(lmtest)
vol.team.log<-glm(trade~year_one_lag+team.1_trade_volume+team.2_trade_volume+team.1+team.2,data=model.frame,family=binomial)
lrtest(vol.team.log,vol.log)
#damnit, they do

vol.team.poi<-glm(connection_strength~year_one_lag+team.1_trade_volume+team.2_trade_volume+team.1+team.2,data=model.frame,family=poisson)
lrtest(vol.team.poi,vol.poi)
summary(vol.team.poi)
#marginally significant

#going to have to look into mixed-effects models I think
library(MASS)
model.frame$distance<-ifelse(model.frame$distance==Inf,10,model.frame$distance)
#mixed<-glmmPQL(connection_strength~year_one_lag+team.1_trade_volume+team.2_trade_volume+distance,
#     random=~1|team.1,data=model.frame,family=poisson)
mixed<-lmer(connection_strength~year_one_lag+I(team.1_trade_volume_last_year+team.2_trade_volume_last_year)+distance_last_year+(1|team.1:team.2),data=model.frame) #can get this to run but assumptions are wrong
mixed<-lmer(connection_strength~year_one_lag+I(team.1=='Anaheim Ducks' & team.2=='Toronto Maple Leafs')+I(team.1_trade_volume_last_year+team.2_trade_volume_last_year)+distance_last_year+(1|team.1:team.2),data=model.frame) #can get this to run but assumptions are wrong
summary(mixed)
qqnorm(residuals(mixed));qqline(residuals(mixed))

#remove all trades that are ducks-leafs
mod.frame<-model.frame
mod.frame<-mod.frame[-which(mod.frame$team.1=='Anaheim Ducks' & mod.frame$team.2=='Toronto Maple Leafs'),]
mixed.no.big<-lmer(connection_strength~year_one_lag+I(team.1_trade_volume_last_year+team.2_trade_volume_last_year)+distance_last_year+(1|team.1:team.2),data=mod.frame) #doesn't change much
