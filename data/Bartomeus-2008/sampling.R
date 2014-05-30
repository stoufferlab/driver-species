#run max_matching_sites.R first

length(attr(nets,site)$weight)
choose(52,7)

?combn
sample_list<-list()
for(site in levels(complete_network$Site))
{
  success<-1
  samples<-list()
  no_links<-attr(matchings,paste0(site,"_TF"))$matching_size
  count<-0
  for(i in 1:100000)
  { count<-count+1
    s<-sample(1:length(attr(nets,site)$weight),no_links)
    weight_sum<-sum(attr(nets,site)$weight[s])
    if(weight_sum>=attr(matchings,site)$matching_weight)
    {
      sample_subset<-attr(nets,site)[s,]
      sample_levels<-droplevels(sample_subset)
      if(length(levels(sample_levels$plants))==attr(matchings,site)$matching_size)
      {
        if(length(levels(sample_levels$insects))==attr(matchings,site)$matching_size)
        {
          samples[[success]]<-sample_subset
          success<-success+1
          break
        }
      }
    }
  }
  attr(sample_list,site)<-samples
  break
}

attr(sample_list,site)[1]
