sim_snps <- function(nrows, ncols, AA = "AA", Aa = "AB", aa = "BB", prob = c(0.475, 0.05, 0.475)) {
  x <- matrix(NA, nrow = nrows, ncol = ncols)
  for (j in 1:ncols) {
    x[, j] <- sample(c(AA, Aa, aa), size = nrows, replace = T, prob = prob)
  }
  x <- as.data.frame(x)
  colnames(x) <- 1:ncols
  return(x)
}