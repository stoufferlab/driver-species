library(ggplot2)
library(ggtern)
library(magrittr)

points <- 
	rbind(c(1, 0, 0, 100), 
				c(2, 100, 0, 0),
				c(3, 0, 100, 0),
				c(4, 100/3, 100/3, 100/3),
				c(5, 50, 0, 50), 
				c(6, 0, 50, 50),
				c(7, 50, 50, 0)) %>%
	data.frame() %>%
	`colnames<-`(c("id", "o", "c", "r")) %T>% {
		ggtern(data = ., aes(o, c, r)) +
			geom_text(aes(label = id))
	}

polygons <- 
	rbind(c(1, 1),
				c(1, 5),
				c(1, 4),
				c(1, 6),
				c(2, 2),
				c(2, 7),
				c(2, 4),
				c(2, 5),
				c(3, 3),
				c(3, 6),
				c(3, 4),
				c(3, 7)) %>%
	data.frame() %>%
	`colnames<-`(c("lab", "id")) %>%
	inner_join(points)

ggtern(polygons,aes(o,r,c)) +
	geom_polygon(aes(fill = as.factor(lab)), colour = "black", size = 2) +
	scale_fill_manual(values = c("#4daf4a", "#377eb8", "#e41a1c")) +
	theme(text = element_blank(), 
				line = element_blank(), 
				legend.position = "top")

