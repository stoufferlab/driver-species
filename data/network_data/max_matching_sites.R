complete_network<-read.csv("Bartomeus_Ntw_nceas.txt",sep="\t")
complete_network$Insect_id <- paste("i",complete_network$Insect_id,sep="_")
complete_network$Plant_id <- paste("p",complete_network$Plant_id,sep="_")
nets<-list()
matchings<-list()
for(site in levels(complete_network$Site)){
  subsetname<-subset(complete_network,Site==site)
  ELname<-data.frame(plants=subsetname$Plant_id,insects=subsetname$Insect_id,weight=subsetname$Number_of_visits_per6min)
  attr(nets,site)<-ELname
  graphname<-graph.data.frame(ELname)
  
  plant.count<-get.vertex.attribute(graphname,"name")
  V(graphname)$type<-TRUE
  V(graphname)$type[grep("p",plant.count)]<-FALSE

  name<-paste0(site,"_TF")
  match_results<-maximum.bipartite.matching(graphname)
  attr(matchings,name)<-match_results

  #rm(subsetname,ELname,graphname,plant.count,name)
}
