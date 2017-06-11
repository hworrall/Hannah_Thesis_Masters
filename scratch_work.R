library(igraph)
library(ndtv)
library(networkDynamic)

list.of.networks<-list()
for(i in 1:length(seasons)){
  list.of.networks[[i]]<-as.network(a.mat.weighted[,,i],directed=FALSE,matrix.type='adjacency')
}

netdyn<-networkDynamic(network.list=list.of.networks)

plot(network.extract(netdyn,at=1))
render.d3movie(netdyn,displaylables=teams)
