library(magrittr)
library(foreach)
library(doMC) 
library(glmulti)

registerDoMC(cores = as.numeric(commandArgs(trailingOnly = T)[1]))
setwd("~/github/driver-species/")

"./code/V2.0/functions" %>% 
  list.files(full.names = T) %>%
  plyr::l_ply(source)

m_f <- readRDS("./data/V2.0/matching_frequency.rds")
meta <- readr::read_csv("data/ntw_info.csv") %>% dplyr::tbl_df()
s_p <- readRDS("./data/V2.0/species_properties.rds") %>% dplyr::tbl_df()

# extract frequency
drivers <- m_f[meta$net_name[meta$study != "ballantyne"]] %>%
  plyr::ldply(function(x){
    dplyr::data_frame(species = names(x),
                      d_freq = attr(x, "n_matchings") - x,
                      d_reim = 1 - x / attr(x, "n_matchings"))
  }) %>%
  dplyr::rename_("net_name" = ".id") %>% dplyr::tbl_df()

# merge with species information
drivers <- dplyr::inner_join(drivers, s_p) %>% 
  dplyr::mutate(d_s_m = d_strength / degree, 
                v_s_m = v_strength / degree)


# extract guild information
drivers <- drivers %>%
  tidyr::separate(species, c("guild"), sep = "_", remove = F)

# explore correlations
factor_correlations <- drivers %>%
  plyr::dlply("guild", function(x){
    x %>% 
      dplyr::select(-net_name, -species, -d_freq, -d_reim, -guild) %>%
      as.matrix() %>%
      cor()
  })

# basic models
library(lme4)
setMethod('getfit', 'merMod', function(object, ...) {
  summ <- coef(summary(object))
  summ1 <- summ[,1:2,drop=FALSE]
  ## if (length(dimnames(summ)[[1]])==1) {
  ##     summ1 <- matrix(summ1, nr=1,
  ##                     dimnames=list(c("(Intercept)"),
  ##                     c("Estimate","Std. Error")))
  ## }
  cbind(summ1, df=rep(10000,length(fixef(object))))
})

lmer.glmulti<-function(formula,data,random="",...){
  lmer(paste(deparse(formula),random),data=data,REML=F,...)
}

# calculate a lot of models
models <- drivers %>%
  dplyr::rename_("n" = "nestedcontribution",
                 "S_d" = "d_s_m",
                 "S_v" = "v_s_m",
                 "d" = "degree") %>%
  plyr::dlply(c("guild"), function(x){
    lmer("d_freq ~(n + S_d + S_v + d)^2 + (1|net_name)" %>% as.formula(), data = x)
#     glmulti("d_freq", c("n", "S_d", "S_v", "d"),
#             data = x,
#             # family = "binomial",
#             level = 2, crit = "aic", plotty = F, report = F,
#             fitfunction = "gls")
  }, .progress = "text") 


saveRDS(models, file = "./data/V2.0/detailed_species_models.rds", ascii = T, compress = F)
saveRDS(factor_correlations, file = "./data/V2.0/species_models_correlations.rds", ascii = T, compress = F)

