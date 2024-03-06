filter_maf <- function(tab, cutoff) {
  ns <- nrow(tab)
  ss <- colSums(tab)
  freq <- 0.5 * ss / ns
  maf_matrix <- rbind(freq, 1 - freq, deparse.level = 0)
  maf <- apply(maf_matrix, 2, min)
  tab <- tab[-which(maf < cutoff), ]
  rownames(tab) <- NULL
  return(tab)
}

get_dosage <- function(tab) {
  possible_doses <- unique(unlist(tab))
  return(sort(possible_doses))
}