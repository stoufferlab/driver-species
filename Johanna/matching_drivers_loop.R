## Info from Kate:
## The output (in the files <site>_matchings.dat) is a list of numbers like this: { 5 10 11 24 28 } 
## That gives the indices of the matched species. You have to match that up with the network itself
## (in the files <site>) to find out which species are actually involved. But be aware that the output starts at 0!
## So to find the species from the list above, you'd open the corresponding network file, and the matched species 
## would be the ones on the 6th, 11th, 12th, 25th and 29th lines (adding one to account for the zero).
## The file matchings_info tells you, for each site, how many interactions there are, the number of matched
## links and the matched weight.


# not sure whether we need quotations
for(site in c("BAT1CA",""....))
{


# read in the network data

site_data <- read.delim(paste0("D:/R Control networks - driver before and after invasions/",site),header = FALSE,sep="")
View(site_data)

site_matchings <- read.delim(paste0("D:/R Control networks - driver before and after invasions/",site,"_matchings.dat"), header = FALSE, sep="")
View(site_matchings)
site_matchings

# {} This brackets must be deleted, otherwise the following steps won`t work.
# <site>_matchings[,<last column <site>_matchings>]<-NULL
# <site>_matchings[,<first column <site>_matchings>]<-NULL
number_columns<-ncol(site_matchings)
site_matchings[,number_columns]<-NULL
site_matchings[,1]<-NULL

# create new column where the numbers fit to the row number of the <site>_matchings.dat files (starting with 0)
# <site>$int<-(as.numeric(row.names(<site>)))-1
site_data$int<-(as.numeric(row.names(site_data)))-1
site_data


# code to find matchings (1=yes, 0=no) between each species interaction in <site> and matchings of each raw of <site>_matchings
# has to be run for each row of <site>_matchings
# <site>$r<row number><-ifelse(<site>$int %in% <site>_matchings[<row number>,],1,0)
number_rows<-nrow(site_matchings)
site_data$total<-0

for(i in 1:number_rows)
{
	site_data$i<-ifelse(site_data$int %in% site_data_matchings[i,],1,0)	
	site_data$total<-site_data$total+site_data$i
}


# control capacity (percentage of matching)
# <site>$control_capacity<-(<site>$total/<number of rows in <site>_matching>)
site_data$control_capacity<-(site_data$total/number_rows)

write.csv(site_data,paste0("D:/R Control networks - driver before and after invasions/",site,"_control_capacity.csv"))
}