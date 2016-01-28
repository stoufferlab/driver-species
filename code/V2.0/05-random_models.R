library(magrittr)
library(vegan)

# base matrix
size <- 81
m <- actuar::rpareto(size, 2.3, size) %>% round() %>% abs() 
m[sample(length(m), round(size/10 * 6))] <- 0
m %<>% matrix(ncol = sqrt(size))

# random models
simul <- vegan::nullmodel(m, "r00_ind") %>%
	simulate(3)

# marginal sums
all_same <- . %>% apply(1, function(x) max(x) - min(x) == 0)

margin <- list(colSums, rowSums) %>% 
	lapply(function(x){
		apply(simul, 3, x) %>%
			cbind(x(m)) %>%
			all_same() %>%
			all()
	})

# connectance
conn <- simul %>%
	apply(3, function(x) sum(as.vector(x) == 0)) %>%
	c(sum(as.vector(m) == 0)) %>%
	matrix(nrow = 1) %>%
	all_same()

# marginal frequencies
fre <- function(x, margin){
	apply(x, margin, function(x){
	sum(x == 0)
	})}

frequen <- 1:2 %>%
	lapply(function(x){
		apply(simul, 3, fre, x) %>% 
			cbind(fre(m, x)) %>%
			all_same() %>%
			all()
	})

# distribution
par(mfrow = c(2,2))
hist(as.vector(m))
plyr::a_ply(simul, 3, function(x){
	hist(as.vector(x))
})

# total sum
tmargin <- simul %>%
	apply(3, function(x) sum(as.vector(x))) %>%
	c(sum(as.vector(m))) %>%
	matrix(nrow = 1) %>%
	all_same()

message("marginal rows: ", margin[[2]])
message("marginal cols: ", margin[[1]])
message("connectance maintained: ", conn)
message("frequen- rows: ", frequen[[1]])
message("frequen- cols: ", frequen[[2]])
message("total marginal: ", tmargin)

apply(simul, 3, function(x){
	ks.test(as.vector(x), as.vector(m))$p.value
})

apply(simul, 3, function(x){
	ks.test(as.vector(x)[as.vector(x) != 0], as.vector(m)[as.vector(m) != 0])$p.value
})

m
simul[, , 1]