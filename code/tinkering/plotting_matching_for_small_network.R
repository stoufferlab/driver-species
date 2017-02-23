library(igraph)

j <- 2

igraph::E(dir)$matched <- 2
igraph::E(dir)$matched[mat2[j, ]] <- 4

igraph::V(dir)$matched <- 2
igraph::V(dir)$matched[igraph::V(dir)$name %in%
														igraph::ends(dir, mat2[j, ])[, 2]] <- 4

igraph::V(dir)$superior <- 1
igraph::V(dir)$superior[igraph::V(dir)$name %in%
													 	igraph::ends(dir, mat2[j, ])[, 1]] <- 2

names_fancy <- sp_names_fancy
names_fancy[igraph::V(dir)$name %in%
							ends(dir, mat2[j, ])[, 1]] <-
	sp_names_fancy_b[igraph::V(dir)$name %in%
									 	igraph::ends(dir, mat2[j, ])[, 1]]

matching_weight <- sum(igraph::E(dir)$weight[mat2[j, ]])

dir %>% 
	plot(
			 vertex.size = 5,
			 vertex.label.family = fam,
			 vertex.label.color = l_c,
			 vertex.label.cex = igraph::V(.)$superior * 0.5 + 0.5 ,
			 # vertex.shape = igraph::V(.)$vertex.shape,
			 vertex.color = pal[igraph::V(.)$matched],
			 edge.color = pal[igraph::E(.)$matched],
			 # edge.color = c("#B2ABD2", "#FDB863"),
			 edge.label = round(igraph::E(.)$weight,2), 
			 edge.width = igraph::E(.)$matched/2,
			 edge.label.family = fam,
			 edge.label.color = igraph::E(.)$matched,
			 edge.label.cex = l_s,
			 edge.curved = F,
			 edge.arrow.size = 0.5,
			 margin = ma, 
			 palette = pal)
