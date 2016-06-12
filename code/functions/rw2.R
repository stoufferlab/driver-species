# Weighted ranked rank correlation with ties

rw2 <- function(x, y, corrected = T, normalised = T){
  n <- length(x)
  stopifnot(n == length(y))
  rw2 <- (1 / 2) * (V(x, n, corrected) + V(y, n, corrected) - D(x, y, n)) / 
    (sqrt(V(x, n, corrected)) * sqrt(V(y, n, corrected)))
  if (normalised){
    A <- - (14 * n^2 + 30 * n + 19) / (16 * n^2 + 30 * n + 11)
    rw2 <- 2 * rw2 / (1 - A) - (1 + A) / (1 - A)
  }
  return(rw2)
}

V <- function(x, n, corrected){
  if (corrected) G(n) / (180 * n) - U(x, n) / n
  else G(n) / (180 * n)
}

U <- function(x, n){
  x <- rank(x, ties.method = "min")
  table(x) %>%
    as.data.frame() %>%
    apply(1, function(z){
      u <- as.numeric(z[2])
      k <- as.numeric(z[1]) - 1
      ((u - u^2 - u^3 + u^4) / 3 + (2 * n * (u - u^3)) / 3) * k +
        (u^3 - u) / 3 * k^2 +
        u^2 / 6 - u * n / 3 - 11 * u / 180 - u^3 / 36 - u^4 / 6 + 4 * u^5 / 45 + n * u^2 / 3  - n^2 * u / 3 + n * u^3 / 3 - n * u^4 / 3 + n^2 * u^3 / 3
    }) %>% 
    sum()
}

G <- function(n) {
  n * (n - 1) * (n + 1) * (2 * n + 1) * (8 * n + 11)
}

D <- function(x, y, n){
  d <- (x - y)^2 * (2 * n + 2 - x - y)^2
  sum(d) / n
}

