library(magrittr)
library(foreach)
library(doMC) 
library(glmulti)

registerDoMC(cores = as.numeric(commandArgs(trailingOnly = T)[1]))
setwd("~/github/driver-species/")

"./code/V2.0/functions" %>% 
  list.files(full.names = T) %>%
  plyr::l_ply(source)

driver <- c("./data/V2.0/matching_frequency") %>% 
  list.files(full.names = T) %>%
  lapply(list.files, full.names = T) %>% 
  unlist() %>% 
  lapply(read_allRDS)
prop <- expand.grid(c("asymmetry", "max_dep"), 
                    c("scaled_FALSE", "scaled_TRUE")) %>% plyr::alply(1)
driver <- mapply(function(x, y){
  attr(x, "type") <- as.character(y[,1])
  attr(x, "scaled") <- stringr::str_split(as.character(y[,2]), "_") %>% unlist() %>% extract(2) %>% as.logical()
  return(x)
}, driver, prop)

meta <- readr::read_csv("data/ntw_info.csv") %>% dplyr::tbl_df()
s_p <- readRDS("./data/V2.0/species_properties.rds") %>% dplyr::tbl_df()

# extract frequency
d_sp <- driver %>%
  plyr::ldply(function(x) {
    get_frequencies(x) %>%
      dplyr::mutate(type = attr(x, "type"),
                    scaled = attr(x, "scaled"))
  })

# merge with species information
drivers <- dplyr::inner_join(d_sp, s_p) %>% 
  dplyr::mutate(d_s_m = d_strength / degree, 
                v_s_m = v_strength / degree)
net <- "./data/V2.0/networks" %>%
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

# calculate a lot of models
mo <- drivers %>%
  dplyr::filter(threshold == 0.5,
                type == "asymmetry",
                scaled == F, 
                study != "ballantyne") %>%
  dplyr::rename_("n" = "nestedcontribution",
                 "S_d" = "d_s_m",
                 "S_v" = "v_s_m",
                 "d" = "degree", 
                 "de" = "d_strength",
                 "vi" = "v_strength")  %>%
  dplyr::mutate(n = scale(n),
                S_d = scale(S_d),
                S_v = scale(S_v),
                d = scale(d),
                eig_cen = scale(eig_cen),
                de = scale(de),
                vi = scale(vi))

to_exclude <- c("de", "eig_cen", "n", "vi") %>% 
  combn(2) %>% 
  apply(2, function(x) paste(x[1], x[2], sep = ":"))

mod <- glmulti("d_reim", c("de", "guild", "eig_cen","n", "vi"),
             # exclude = to_exclude, 
             data = mo,
             family = "binomial",
             fitfunction = glmer.glmulti, 
             level = 1,
             crit = "aic", 
             method = "h", 
             marginality = T,
             includeobjects = F, 
             random = " + (1 | net_name)")
# 
# objects <- lapply(mod@objects, function(x){
#   if(is.null(x@optinfo$conv$lme4$code)) return(NULL)
#   else return(x)
# }) 
# 
# failed <- lapply(mod@objects, function(x){
#   if(is.null(x@optinfo$conv$lme4$code)) return(F)
#   else return(T)
# }) %>% unlist()
# 
# x <- mod
# x@crits[failed] <- 9999
# 
# # remove models that didn't converge
# objects <- objects[!sapply(objects, is.null)] 
# 
# lapply(objects, formula) %>%
#   lapply(deparse, width.cutoff = 500) %>%
#   unlist() %>%
#   duplicated()
# formula(objects[[1]]) %>% deparse
# %>% MuMIn::model.avg()


# after some trials and trying the S_d vs d_strength we conclude that the second
# option is better for the plants and not much different for pollinators
# 
# f <- as.formula("d_reim ~  n + eig_cen + d + v_strength + d_strength")
# crit <- "aicc"
# level <- 2
# method <- "h"
# 
# mod <- glmulti(d_reim ~  n + eig_cen + v_strength + d_strength + guild,
#                data = mo,
#                family = "binomial",
#                fitfunction = glm.redefined, 
#                level = level,
#                crit = crit, 
#                method = method, 
#                marginality = T,
#                includeobjects = F)
# 
# mod2 <- glmulti(d_reim ~  n + eig_cen + v_strength + d_strength + guild,
#                 data = mo,
#                 family = "binomial",
#                 fitfunction = glm.redefined, 
#                 level = level,
#                 crit = "bic", 
#                 method = method, 
#                 marginality = T,
#                 includeobjects = F)
# 
# pol_mod <- glmulti(f,
#                    data = dplyr::filter(mo, guild == "pol"),
#                    family = "binomial",
#                    fitfunction = glm.redefined, 
#                    level = level,
#                    crit = crit, 
#                    method = method, 
#                    marginality = T,
#                    includeobjects = F)
# 
# models <- list(pla = pla_mod, pol = pol_mod)

saveRDS(mod, file = "./data/V2.0/detailed_species_models.rds", ascii = T, compress = F)
saveRDS(mo, file = "./data/V2.0/detailed_species_models_data.rds", ascii = T, compress = F)

saveRDS(factor_correlations, file = "./data/V2.0/species_models_correlations.rds", ascii = T, compress = F)

