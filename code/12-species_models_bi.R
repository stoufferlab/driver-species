library(magrittr)
library(foreach)
library(doMC) 
library(glmulti)

registerDoMC(cores = as.numeric(commandArgs(trailingOnly = T)[1]))
setwd("~/github/driver-species/")

"./code/functions" %>% 
	list.files(full.names = T) %>%
	plyr::l_ply(source)

driver <- c("./data/processed/matching_frequency_bi")  %>% 
	read_allRDS()

d_sp <- driver %>%
	get_frequencies() %>%
	dplyr::mutate(type = "z_bi",
								scaled = FALSE)

# merge with species information
drivers <- dplyr::inner_join(d_sp, s_p) %>% 
	dplyr::mutate(d_s_m = d_strength / degree, 
								v_s_m = v_strength / degree)
net <- "./data/processed/networks" %>%
	read_networks()

guild <- c("pla", "pol") %>%
	plyr::ldply(function(y){
		o <- lapply(net, function(x) igraph::V(x)[igraph::V(x)$type == y]$name) %>%
			unlist() %>% unique()
		data.frame(guild = y, species = o)
	})

# insert guild information
drivers <- drivers %>%
	dplyr::inner_join(guild) %>% 
	dplyr::inner_join(meta) 
# drivers %>%
# dplyr::filter(threshold == 0.5,
#               type == "asymmetry",
#               scaled == F, 
#               study != "ballantyne") %>%
# ggplot(aes(x = guild, y = d_reim)) +
# geom_violin(aes(fill = guild))

# explore correlations
factor_correlations <- drivers %>%
	plyr::dlply("guild", function(x){
		x %>% 
			dplyr::select(nestedcontribution, degree, d_strength, v_strength, eig_cen) %>%
			as.matrix() %>%
			cor()
	})

# basic models
library(lme4)
setMethod('getfit', 'glmerMod', function(object, ...) {
	summ <- coef(summary(object))
	summ1 <- summ[,1:2,drop=FALSE]
	## if (length(dimnames(summ)[[1]])==1) {
	##     summ1 <- matrix(summ1, nr=1,
	##                     dimnames=list(c("(Intercept)"),
	##                     c("Estimate","Std. Error")))
	## }
	cbind(summ1, df=rep(times =10000,length(fixef(object))))
})


glmer.glmulti<-function(formula,data,random="",...){
	glmer(paste(deparse(formula, width.cutoff = 500),random),data=data,...)
}

glm.redefined = function(formula, data, always="", ...) {
	# print(formula)
	glm(as.formula(paste(deparse(formula, width.cutoff = 500), always)), data=data, ...)
}

invasive_sp <- dplyr::filter(meta, inv) %>%
	dplyr::mutate(species = plyr::mapvalues(invader, 
																					c("car", "op", "imp"),
																					c("p_4", "p_25", "Impatiens glandulifera")), 
								invasive = T) %>%
	dplyr::select(net_name, species, invasive)

# calculate a lot of models
mo <- drivers %>%
	dplyr::filter(threshold == 0.5,
								type == "z_bi",
								scaled == F, 
								study != "ballantyne") %>%
	dplyr::rename_("n" = "nestedcontribution",
								 "S_d" = "d_s_m",
								 "S_v" = "v_s_m",
								 "d" = "degree", 
								 "de" = "d_strength",
								 "vi" = "v_strength")  %>%
	dplyr::full_join(invasive_sp) %>% 
	dplyr::mutate(invasive = replace(invasive, is.na(invasive), FALSE)) %>%
	dplyr::mutate(n = scale(n)[,1],
								S_d = scale(S_d),
								S_v = scale(S_v),
								d = scale(d),
								eig_cen = scale(eig_cen),
								de = scale(de),
								vi = scale(vi))

m0 <- glmer(d_reim ~ d + de + guild + eig_cen + n + vi + invasive + (1 | net_name),
			data = mo, family = "binomial")
m1 <- glmer(d_reim ~ d + de + eig_cen + n + vi + invasive + (1 | net_name),
			data = mo, family = "binomial")
m2 <- glmer(d_reim ~ d + de + eig_cen + n + vi + (1 | net_name),
						data = mo, family = "binomial")

saveRDS(m2, file = "./data/processed/bi_importance_species_model.rds", ascii = T, compress = F)

