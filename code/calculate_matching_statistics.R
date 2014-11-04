## Info from Kate:
## The output (in the files <site>_matchings.dat) is a list of numbers like this: { 5 10 11 24 28 } 
## That gives the indices of the matched species. You have to match that up with the network itself
## (in the files <site>) to find out which species are actually involved. But be aware that the output starts at 0!
## So to find the species from the list above, you'd open the corresponding network file, and the matched species 
## would be the ones on the 6th, 11th, 12th, 25th and 29th lines (adding one to account for the zero).
## The file matchings_info tells you, for each site, how many interactions there are, the number of matched
## links and the matched weight.

# specify where the data directory will be
datadir <- "../data/"

# TODO: update this list when the final results are in
networks <- c("BAT1CA","BAT2CA","FRA1OP","FRA2OP","MED1CA","MED3CA","MED4CA","MIQ1OP","MIQ2OP","SEL1OP","SEL2OP")
#networks <- c("BAT1CA","BAT2CA","FRA1OP","FRA2OP","MED1CA","MED2CA","MED3CA","MED4CA","MIQ1OP","MIQ2OP","SEL1OP","SEL2OP")

# look at all of the data files
for(network in networks){
	for(binary in c(FALSE,TRUE)){
		if(binary) network <- paste0(network,"_binary")

		# read in the network info
		net_data <- read.csv(paste0(datadir,"network_data/",network),header=F,sep=" ",colClasses="character")

		# check for the matching results before proceeding
		if(!file.exists(paste0(datadir,"matching_results/",network,"_matchings.dat"))){
			warning(paste0("Did you know that there is no matching file for ",network,"?"))
			break
		}

		# read in the different matchings
		matching_data <- read.csv(paste0(datadir,"matching_results/",network,"_matchings.dat"),sep=" ",header=F,na.strings=c("{","}"))
		matching_data <- matching_data[,which(!is.na(colSums(matching_data)))]
		matching_data <- matching_data + 1

		# build some dataframes to hold the row/col matching results
		row_species <- data.frame(row.names=unique(net_data[,1]))
		row_species$matched <- 0
		row_species$unmatched <- nrow(matching_data)

		col_species <- data.frame(row.names=unique(net_data[,2]))
		col_species$matched <- 0
		col_species$unmatched <- nrow(matching_data)
		
		# figure out which row/col species are connected by the matched interaction
		for(i in 1:nrow(matching_data)){
			for(interaction in matching_data[i,]){
				row_species[net_data[interaction,1],"matched"] <- row_species[net_data[interaction,1],"matched"] + 1
				row_species[net_data[interaction,1],"unmatched"] <- row_species[net_data[interaction,1],"unmatched"] - 1

				col_species[net_data[interaction,2],"matched"] <- col_species[net_data[interaction,2],"matched"] + 1
				col_species[net_data[interaction,2],"unmatched"] <- col_species[net_data[interaction,2],"unmatched"] - 1
			}
		}

		# write out the results
		write.csv(row_species,file=paste0(datadir,"matching_statistics/",network,"_row_matchings.dat"),quote=F)
		write.csv(col_species,file=paste0(datadir,"matching_statistics/",network,"_col_matchings.dat"),quote=F)

		# check that everything is as it should be
		if(min(row_species)<0)
			warning(paste0("Did you know that there is something odd with the matching data for ",network,"?"))
		else
			if(min(col_species)<0)
				warning(paste0("Did you know that there is something odd with the matching data for ",network,"?"))
	}
}
