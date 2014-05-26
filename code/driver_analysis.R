# read in the network data
complete_network<-read.csv("Bartomeus_Ntw_nceas.txt",sep="\t")

# put a prefix (i for insects, p for plants) in front of the ID columns to distinguish them
complete_network$Insect_id <- paste("i",complete_network$Insect_id,sep="_")
complete_network$Plant_id <- paste("p",complete_network$Plant_id,sep="_")

# make a list to store the info about the interactions (nets) and the results of the analysis (matchings)
nets<-list()
matchings<-list()

# run through each site
for(site in levels(complete_network$Site)){
  
	# separate out the interactions relevant to the current site
	site_subset<-subset(complete_network,Site==site)

	# make an "edge list" with the details of the interactions and their weights
	edge_list<-data.frame(plants=site_subset$Plant_id,insects=site_subset$Insect_id,weight=site_subset$Number_of_visits_per6min)
	
	#save the interactions and weights (edge_list) in nets
	attr(nets,site)<-edge_list

	# make a graph from the edge list
	interaction_graph<-graph.data.frame(edge_list)
	  
	# count the number of plants
	plant.count<-get.vertex.attribute(interaction_graph,"name")

	# set all interactions to type "TRUE", then the plant ones to "FALSE" 
	# (this essentially tells it that they're plants and pollinators, so it's a bipartite network)
	V(interaction_graph)$type<-TRUE
	V(interaction_graph)$type[grep("p",plant.count)]<-FALSE

	# name is the name of the site
	name<-paste0(site)

	# run the analysis
	match_results<-maximum.bipartite.matching(interaction_graph)

	# save the results in the "matchings" list
	attr(matchings,name)<-match_results

	# write the edge list to a file to be used in C++
	write.table(edge_list, file = name, quote = FALSE, sep = " ", row.names = FALSE, col.names = FALSE)
}

main_info_matchings<-data.frame("site"=character(12), "matching_size"=numeric(12), "matching_weight"=numeric(12), stringsAsFactors=FALSE)
line_number<-0
for(site in levels(complete_network$Site)){
	main_info_matchings[line_number,1]<-site
	main_info_matchings[line_number,2]<-attr(matchings,site)$matching_size
	main_info_matchings[line_number,3]<-attr(matchings,site)$matching_weight
	line_number<-line_number+1
}