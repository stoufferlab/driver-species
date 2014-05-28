# load the igraph package
require(igraph)

# read in the network data
complete_network<-read.csv("data/Bartomeus_Ntw_nceas.txt",sep="\t")

# make a new column showing each link so we can determine which are not unique.
complete_network$Links<- paste(complete_network$Insect_id,complete_network$Plant_id,sep="_")

# make a list to store the info about the interactions (nets) and the results of the analysis (matchings)
nets<-list()
matchings<-list()
interactions<-list()

# run through each site
for(site in levels(complete_network$Site)){
  
	# separate out the interactions relevant to the current site
	site_subset<-subset(complete_network,Site==site)

	# make an "edge list" with the details of the interactions and their weights
	edge_list<-data.frame(plants=numeric(0),insects=numeric(0),weight=numeric(0))
	
	# start at row 1
	row_number<-1

	# subset out each interaction
	for(level in levels(as.factor(site_subset$Links)))
	{
		links_subset<-subset(site_subset,Links==level)
		total_weight<-sum(links_subset$Number_of_visits_per6min)
		edge_list[row_number,"plants"]<-links_subset[1,"Plant_id"]
		edge_list[row_number,"insects"]<-links_subset[1,"Insect_id"]
		edge_list[row_number,"weight"]<-total_weight
		row_number<-row_number+1
	}

	#save the interactions and weights (edge_list) in nets
	attr(nets,site)<-edge_list

	# count the number of interactions
	attr(interactions, site)<-nrow(edge_list)

	# print a file of the edge list
	write.table(edge_list, file = site, quote = FALSE, row.names = FALSE, col.names = FALSE)

	# put prefixes in front of the plant and insect IDs to distinguish them
	edge_list$insects <- paste("i",edge_list$insects,sep="_")
	edge_list$plants <- paste("p",edge_list$plants,sep="_")

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
	#write.table(edge_list, file = name, quote = FALSE, sep = " ", row.names = FALSE, col.names = FALSE)
}

main_info_matchings<-data.frame("site"=character(12), "number_links"=numeric(12), "matching_size"=numeric(12), "matching_weight"=numeric(12), stringsAsFactors=FALSE)
line_number<-1
for(site in levels(complete_network$Site)){
	main_info_matchings[line_number,1]<-site
	main_info_matchings[line_number,2]<-attr(interactions, site)
	main_info_matchings[line_number,3]<-attr(matchings,site)$matching_size
	main_info_matchings[line_number,4]<-attr(matchings,site)$matching_weight
	line_number<-line_number+1
}

write.table(main_info_matchings, file = "matchings_info", quote = FALSE, row.names = FALSE)