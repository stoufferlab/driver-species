## Info from Kate:
## The output (in the files <site>_matchings.dat) is a list of numbers like this: { 5 10 11 24 28 } 
## That gives the indices of the matched species. You have to match that up with the network itself
## (in the files <site>) to find out which species are actually involved. But be aware that the output starts at 0!
## So to find the species from the list above, you'd open the corresponding network file, and the matched species 
## would be the ones on the 6th, 11th, 12th, 25th and 29th lines (adding one to account for the zero).
## The file matchings_info tells you, for each site, how many interactions there are, the number of matched
## links and the matched weight.



# read in the network data

BAT1CA <- read.delim("D:/R Control networks - driver before and after invasions/BAT1CA",header = FALSE,sep="")
View(BAT1CA)

BAT1CA_matchings <- read.delim("D:/R Control networks - driver before and after invasions/BAT1CA_matchings.dat", header = FALSE, sep="")
View(BAT1CA_matchings)
BAT1CA_matchings

#{} This brackets must be deleted, otherwise the following steps won`t work.
BAT1CA_matchings[,9]<-NULL
BAT1CA_matchings[,1]<-NULL

#create new column where the numbers fit to the row number of the <site>_matchings.dat files (starting with 0)
BAT1CA$int<-(as.numeric(row.names(BAT1CA)))-1
BAT1CA


# code to find matchings (1=yes, 0=no) between each species interaction in <site>
# and matchings of each raw of <site>_matchings.dat
BAT1CA$r1<-ifelse(BAT1CA$int %in% BAT1CA_matchings[1,],1,0)
BAT1CA$r2<-ifelse(BAT1CA$int %in% BAT1CA_matchings[2,],1,0)
BAT1CA$r3<-ifelse(BAT1CA$int %in% BAT1CA_matchings[3,],1,0)
BAT1CA$r4<-ifelse(BAT1CA$int %in% BAT1CA_matchings[4,],1,0)


# sum of the total matches
BAT1CA$total<-(BAT1CA$r1+BAT1CA$r2+BAT1CA$r3+BAT1CA$r4)

# percentage of matching
BAT1CA$percentage<-(BAT1CA$total/4)*100

write.csv(BAT1CA,("D:/R Control networks - driver before and after invasions/BAT1CAfull2.csv"))