SEL1OP$Insect_id <- paste("i",SEL1OP$Insect_id,sep="_")
SEL1OP$Plant_id <- paste("p",SEL1OP$Plant_id,sep="_")
head(SEL1OP)
SEL1OP_EL<-data.frame(plants=SEL1OP$Plant_id,insects=SEL1OP$Insect_id,weight=SEL1OP$Number_of_visits_per6min)
head(SEL1OP_EL)
g_SEL1OP<-graph.data.frame(SEL1OP_EL)

V(g_SEL1OP)
V(g_SEL1OP)$type <- c(rep(c(FALSE), length=9),rep(TRUE,length=21))
str(g_SEL1OP, v=TRUE)
maximum.bipartite.matching(g_SEL1OP)
?grep
grep("p",V(g_SEL1OP),fixed=FALSE)
plant.count<-get.vertex.attribute(g_SEL1OP,"name")
V(g_SEL1OP)$type<-FALSE
grep("p",plant.count)
V(g_SEL1OP)$type[grep("p",plant.count)]<-TRUE

sitelist<-c("SEL2OP","SEL1OP","BAT1CA")
for(site in c(SEL2OP,SEL1OP,BAT1CA)){
print(head(site))
}

for(site in)
head(complete_network)
levels(complete_network$Site)
 
}

rm(matchings)
?attr
attr(matchings,"SEL1OP")
site
rm(matchings)

attr(matchings,"SEL1OP_FT")$matching==attr(matchings,"SEL1OP_TF")$matching
is.na(attr(matchings,"SEL2OP_FT")$matching)==is.na(attr(matchings,"SEL2OP_TF")$matching)
nets["SEL2OP"]
sum(attr(nets,"SEL2OP")$weight)

for(i in 1:10000){s <- sample(1:length(attr(nets,site)$weight), 9); if(sum(attr(nets,site)$weight[s]) == attr(matchings,paste0(site,"_FT"))$matching_weight){break}}
length(levels(y$plants))


