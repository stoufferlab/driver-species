---
title: "Generate Networks"
author: "Fernando Cagua"
date: "17 August 2015"
output: html_document
---



First I read the data. Here I'm assuming that the different sites are independent, that the `Round` column corresponds to different samplings days and that it's OK to estimate the total weight by adding `Number_of_visits_per6min` across `Round`


```r
data <- read.csv("./data/network_data/Bartomeus_Ntw_nceas.txt",sep="\t") %>%
	tbl_df() %T>% print(width = Inf)
```

```
## Source: local data frame [785 x 10]
## 
##      Site Round Plant_id Plant_Family          Plant_Gen_Sp Insect_id
## 1  BAT1CA     3       31    Asteracea Urospermum_picrioides       125
## 2  BAT1CA     4       32    Asteracea    Sonchus_tenerrinus        11
## 3  BAT1CA     4       32    Asteracea    Sonchus_tenerrinus       105
## 4  BAT1CA     1       24      Labiada    Lavandula_stoechas        11
## 5  BAT1CA     4       32    Asteracea    Sonchus_tenerrinus       114
## 6  BAT1CA     1        8    Asteracea   Aetheorrina_bulbosa       114
## 7  BAT1CA     1        8    Asteracea   Aetheorrina_bulbosa       115
## 8  BAT1CA     1       24      Labiada    Lavandula_stoechas       181
## 9  BAT1CA     1       24      Labiada    Lavandula_stoechas       182
## 10 BAT1CA     1       24      Labiada    Lavandula_stoechas       184
## ..    ...   ...      ...          ...                   ...       ...
##    Insect_Order Insect_Family       Insect_Gen_Sp Number_of_visits_per6min
## 1    Coleoptera   Oedemeridae   Oedemera_flavipes                        1
## 2   Hymenoptera  Megachilidae Anthidium_sticticum                        1
## 3    Coleoptera   Oedemeridae     Oedemera_lurida                        1
## 4   Hymenoptera  Megachilidae Anthidium_sticticum                        2
## 5    Coleoptera     Dasytidae          Psilothrix                        2
## 6    Coleoptera     Dasytidae          Psilothrix                        1
## 7    Coleoptera Chrysomelidae      Criptocephalus                        1
## 8   Hymenoptera  Megachilidae           Megaquile                        3
## 9   Hymenoptera  Megachilidae           Megaquile                        2
## 10  Hymenoptera Anthophoridae  Amegilla_femorata?                        3
## ..          ...           ...                 ...                      ...
```

```r
ntw <- data %>%
	dplyr::mutate(pla = paste0("p_", Plant_id),
								pol = paste0("i_", Insect_id)) %>%
	dplyr::group_by(Site, pla, pol) %>%
	dplyr::summarise(weight = sum(Number_of_visits_per6min)) %>%
	dplyr::group_by() %T>%
	print()
```

```
## Source: local data frame [577 x 4]
## 
##      Site  pla   pol weight
## 1  BAT1CA  p_1  i_11      1
## 2  BAT1CA  p_1 i_125      1
## 3  BAT1CA p_13 i_105      1
## 4  BAT1CA p_13 i_112      1
## 5  BAT1CA p_21  i_45      1
## 6  BAT1CA p_24  i_11     12
## 7  BAT1CA p_24  i_13      1
## 8  BAT1CA p_24 i_175      1
## 9  BAT1CA p_24 i_181      3
## 10 BAT1CA p_24 i_182      4
## ..    ...  ...   ...    ...
```

Now let's convert it to an `igraph` network and calculate a bipartite maximal matching.


```r
m_match <- plyr::dlply(ntw, "Site", function(x){
	edg <- x %>% 
		dplyr::select(-Site)
	ver <- edg %>%
		dplyr::select(-weight) %>%
		tidyr::gather(type, name, pla, pol) %>%
		dplyr::distinct() %>%
		dplyr::select(name, type)
	
	net <- igraph::graph.data.frame(edg, directed = F, vertices = ver)
	list(net = net,
			 match = igraph::maximum.bipartite.matching(net))
})
```

Now let's have a look at the matching results


```r
plyr::ldply(m_match, function(x){
	data.frame(matching_size = x$match$matching_size,
						 n_pla = table(igraph::V(x$net)$type)["pla"],
						 n_pol = table(igraph::V(x$net)$type)["pol"],
						 n_int = length(igraph::E(x$net)$weight),
						 matching_weight = x$match$matching_weight,
						 sum_weight = sum(igraph::E(x$net)$weight))
}) %>% 
	dplyr::mutate(n_comb = choose(n_int, matching_size)) %>%
	kable
```



|Site   | matching_size| n_pla| n_pol| n_int| matching_weight| sum_weight|       n_comb|
|:------|-------------:|-----:|-----:|-----:|---------------:|----------:|------------:|
|BAT1CA |             8|     9|    26|    40|               9|         70| 7.690468e+07|
|BAT2CA |             9|    10|    47|    82|              32|        185| 2.930521e+11|
|FRA1OP |             6|     7|    24|    32|              15|         63| 9.061920e+05|
|FRA2OP |             8|     8|    25|    35|              21|         97| 2.353582e+07|
|MED1CA |            10|    10|    25|    42|              20|         88| 1.471443e+09|
|MED2CA |            13|    14|    43|    85|              25|        177| 7.387997e+14|
|MED3CA |            10|    10|    30|    48|              21|         93| 6.540716e+09|
|MED4CA |            10|    11|    27|    61|              16|        114| 9.017717e+10|
|MIQ1OP |             8|     8|    27|    42|              16|         77| 1.180302e+08|
|MIQ2OP |             8|     8|    24|    36|              20|        112| 3.026034e+07|
|SEL1OP |             8|     9|    21|    32|              24|         74| 1.051830e+07|
|SEL2OP |             8|     9|    28|    42|              23|         77| 1.180302e+08|

The matching size and number of interactions doesn't coincide with the ones previously used for the bipartite matching.
