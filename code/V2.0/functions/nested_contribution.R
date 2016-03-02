nested_contribution <- function (web, nsimul = 99) 
{
  web <- ifelse(web > 0, 1, 0)
  if (is.null(rownames(web))) 
    rownames(web) <- paste0("L", seq.int(nrow(web)))
  if (is.null(colnames(web))) 
    colnames(web) <- paste0("H", seq.int(ncol(web)))
  lower.out <- data.frame(row.names = rownames(web))
  lower.out$nestedcontribution <- NA
  higher.out <- data.frame(row.names = colnames(web))
  higher.out$nestedcontribution <- NA
  if (any(dim(web) < 2)) {
    warning("Your web is too small for a meaningful computation of nestedcontrrank (and probably other indices)!")
  }
  else {
    nested.orig <- vegan::nestednodf(web)$statistic["NODF"]
    for (i in rownames(web)) {
      message(i)
      probs <- (rowSums(web)[i]/ncol(web) + colSums(web)/nrow(web))/2
      nested.null <- sapply(1:nsimul, function(x) {
        web.null <- web
        web.null[i, ] <- rbinom(ncol(web), 1, probs)
        vegan::nestednodf(web.null)$statistic["NODF"]
      })
      lower.out[i, "nestedcontribution"] <- (nested.orig - 
                                               mean(nested.null))/sd(nested.null)
    }
    for (i in colnames(web)) {
      message(i)
      probs <- (rowSums(web)/ncol(web) + colSums(web)[i]/nrow(web))/2
      nested.null <- sapply(1:nsimul, function(x) {
        web.null <- web
        web.null[, i] <- rbinom(nrow(web), 1, probs)
        vegan::nestednodf(web.null)$statistic["NODF"]
      })
      higher.out[i, "nestedcontribution"] <- (nested.orig - 
                                                mean(nested.null))/sd(nested.null)
    }
  }
  out <- list(`higher level` = higher.out, `lower level` = lower.out)
  return(out)
}
