get_structural_rho_sensitivity <- function(sensitivity_range, dn){
  purrr::cross(sensitivity_range) %>%
    purrr::map(~get_all_struct(dn, .[[1]], .[[2]]))
}


get_all_struct <- function(directed_networks, rho, delta = 0){
  foreach(i = 1:length(directed_networks), .combine = rbind) %dopar% {
    gamma_critical <- network_stability(directed_networks[[i]], 
                                        delta = delta, 
                                        rho = rho)
    species_level_structural_stability(directed_networks[[i]], 
                                       delta = delta, 
                                       rho = rho, 
                                       gamma_avg = gamma_critical) %>% {
      dplyr::data_frame(net_name = names(directed_networks)[i], 
                        sp_name = names(.), 
                        feasibility = unlist(.), 
                        gamma_crit = gamma_critical)
    }
  }
}

make_binary <- function(x){
  x %>%  
    `class<-`("logical") %>%
    `class<-`("numeric")
} 


network_stability <- function(x, delta = 0, rho = 0.1){
  x %>%
    igraph::as_incidence_matrix(types = igraph::V(.)$type == "pla") %>%
    gamma_hat(delta = delta, rho = rho, spp_gr = NULL, bp = NULL)

}

species_level_structural_stability <- function(x, delta = 0, rho =0.1, gamma_avg){
  x %>%
    igraph::V(.) %>%
    purrr::map(~ species_structural_stability(x, ., delta = delta, rho = rho, gamma_avg = gamma_avg))
}

species_structural_stability <- function(x, sp, delta = 0, rho = 0.1, gamma_avg){
  x %>%
    igraph::delete_vertices(sp) %>%
    igraph::as_incidence_matrix(types = igraph::V(.)$type == "pla") %>%
    omega(delta = delta, gamma_avg = gamma_avg, rho = rho, spp_gr = NULL, bp = NULL)
}

gamma_hat <- function(web, delta, spp_gr = NULL, bp = NULL, fun = interaction_matrix, rho = NULL){
  f_eig <- function(gamma_avg, web, rho, delta){
    alpha <- fun(web = web, 
                 gamma_avg = gamma_avg, 
                 delta = delta, 
                 spp_gr = spp_gr, 
                 bp = bp, 
                 rho = rho)$alpha
    (min(Re(eigen(alpha)$values)))^2
  }
  optimize(f_eig,c(0,1000),web = web, rho = rho, delta = delta)$minimum
}

omega <- function(web, gamma_avg, delta, spp_gr = NULL, bp = NULL, fun = interaction_matrix, rho = NULL){
  alpha <- fun(web = web, gamma_avg = gamma_avg, rho = rho, delta = delta, spp_gr = spp_gr, bp = bp)$alpha
  S <- nrow(alpha)
  # Sigma <-solve(t(alpha) %*% alpha)
  Sigma <- corpcor::pseudoinverse(t(alpha) %*% alpha)
  m <- matrix(0,S,1)
  a <- matrix(0,S,1)
  b <- matrix(Inf,S,1)
  d <- mvtnorm::pmvnorm(lower = rep(0,S), upper = rep(Inf,S), mean = rep(0,S),
                        sigma = Sigma)
  log10(d[1])
}

# Calculates the interaction matrix by averaging mutualism coefficients between
# partitions and keeping competition constant
# web: pollination units by plant species matrix
# gamma_avg: mean mutualistic benefit
# rho: interspecific competition
# delta: mutualistic trade-off
# spp_gr: vector that maps rows in web to species eg c(1, 1, 2) for two pollinator species in which rows 1 and two belong to species 1
# bp: bodypart id. Not used in this approach
# reduce: wether to use Z and Y to reduce the final intereaction matrix or not
interaction_matrix <- function(web, gamma_avg, rho, delta, 
                                         spp_gr = NULL, bp = NULL, 
                                         rho_interphen = 1, reduce = TRUE,
                                         gamma_norm = c('after_dilution',
                                                        'before_dilution','none')){
  
  # # remove pollination units without pollen
  # spp_gr <- spp_gr[rowSums(web) != 0]
  # web <- web[rowSums(web) !=0, ]
  require(magrittr)
  # if not species vector is provided assume that every row is a unique species
  if(is.null(spp_gr)) spp_gr = 1:nrow(web)
  # if not body bart vector is provided assume all belong to the same body part
  if(is.null(bp)) bp = 1
  n_phen <- length(web) / (ncol(web) * length(unique(bp)))
  phen <- rep(1:n_phen, each = length(unique(bp)))
  
  # Competition matrices (just duplicate rows for partitions within a species)
  # generate species-speies matrix
  web_sp <- group_matrix(web, spp_gr) 
  web_sp_b <- web_sp %>% make_binary()
  SA <- nrow(web_sp)
  SP <- ncol(web_sp)
  alphaA <- matrix(rho,SA,SA) + (1-rho) * diag(rep(1,SA))
  alphaP <- matrix(rho,SP,SP) + (1-rho) * diag(rep(1,SP))
  # repeat stuff
  alphaA <- alphaA[rep(1:SA, rle(spp_gr)$lengths), rep(1:SA, rle(spp_gr)$lengths)]
  
  # mapping matrices
  Z <- make_z(web, spp_gr)
  Y <- t(MASS::ginv(Z))  # if Y is an unweighted average just take the inverse of Z
  Ya <- Y[1:ncol(web), 1:nrow(web)] 
  da <- rowSums(web)  # pollinator degrees
  dp <- colSums(Ya %*% web)  # plant degrees
  
  # Calculate gammas
  gammaA <- diag(rowSums(web_sp_b)^-delta, length(rowSums(web_sp_b))) %*% web_sp_b
  gammaA <- gammaA[rep(1:SA, rle(spp_gr)$lengths), ]
  gammaP <- diag(dp^-delta) %*% t(web)
  # make NAN -> 0 because the NAN are introduced for some species that have no interaction partners and hence the degre is 0 and you end up with 0/0
  gammaA[is.na(gammaA) | is.infinite(gammaA)] <- 0
  gammaP[is.na(gammaP) | is.infinite(gammaP)] <- 0
  
  # assemble complete interactiob matrix
  b <- rbind(cbind(alphaA,-gammaA),cbind(-gammaP,alphaP))
  
  # reduce matrix to species species if required
  b_full <- b
  b <- MASS::ginv(t(Z)) %*% b %*% t(Y)
  
  # extract gammas and alphas again
  gammaP <- -b[(SA + 1):(SA+SP), 1:SA]
  gammaA <- -b[1:SA, (SA + 1):(SA+SP)]
  alphaA <- b[1:SA, 1:SA]
  alphaP <- b[(SA+1):(SA+SP), (SA+1):(SA+SP)]
  
  # Make gamma_avg actually the average / normalise gammas
  f <- sum(gammaA[web_sp_b == 1] + gammaP[t(web_sp_b) == 1] ) / (2 * sum(web_sp_b==1))
  gammaA <- gamma_avg/f * gammaA
  gammaP <- gamma_avg/f * gammaP
  
  # assemble species species interaction matrix
  b <- rbind(cbind(alphaA,-gammaA),cbind(-gammaP,alphaP))
  
  list(alpha = b,
       alphaA = alphaA, alphaP = alphaP, gammaA = gammaA,
       gammaP = gammaP,
       alpha_full = b_full, 
       gamma_f = f)
}

# Collapse rows of a matrix given a mapping vector
group_matrix <- function(x, by, fun = sum){
  types <- unique(by)
  sp <- matrix(NA, length(types), ncol(x))
  for(i in 1:length(types)){
    spw <- x[by == types[i], , drop= FALSE]
    suppressWarnings({
      sp[i, ] <- apply(spw, 2, fun)
    })
  }
  sp
}

# Calculates mapping matrix
# web: pollination units by plant species matrix
# spp_gr: vector that maps rows in web to species eg c(1, 1, 2) for two pollinator species in which rows 1 and two belong to species 1
make_z <- function(web, spp_gr){
  # for the animals
  Z_a <- matrix(0, dplyr::n_distinct(spp_gr), nrow(web))
  l_a <- rle(spp_gr)$lengths
  p_a <- cumsum(l_a) - l_a + 1
  for(i in 1:nrow(Z_a)){
    Z_a[i, p_a[i]:(p_a[i]+l_a[i] -1)] <- 1
  }
  # for the plants
  Z_p <- diag(ncol(web))
  rbind(Z_a, matrix(0, nrow(Z_p), ncol(Z_a))) %>%
    cbind(rbind(matrix(0, nrow(Z_a), ncol(Z_p)), Z_p))
}
