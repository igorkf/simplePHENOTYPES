bed_to_numeric <- function(file, ...) {
  fam_fn <- gsub(".bed$", ".fam", file)
  bim_fn <- gsub(".bed$", ".bim", file)
  temp <- tempfile(fileext = ".gds")
  SNPRelate::snpgdsBED2GDS(
    bed.fn = file,
    fam.fn = fam_fn,
    bim.fn = bim_fn,
    out.gdsfn = temp,
    snpfirstdim = FALSE,
    verbose = FALSE
  )
  result <- gds_to_numeric(temp, ...)
  
  # invert dosage
  meta_cols <- c("snp", "allele", "chr", "pos", "cm")
  sample_cols <- colnames(result)[!colnames(result) %in% meta_cols]
  possible_values <- sort(unique(unlist(result[, sample_cols])))
  if (all(possible_values == c(-1, 0, 1))) {
    # -101 dosage (-1 turns 1; 1 turns -1)
    result[, sample_cols] <- invert_dosage(result[, sample_cols], -1, 1)
  } else if (all(possible_values == c(0, 1, 2))) {
    # 012 dosage (0 turns 2; 2 turns 0)
    result[, sample_cols] <- invert_dosage(result[, sample_cols], 0, 2)
  }
  
  # unlink(temp, force = TRUE)
  return(list(result = result, temp = temp))
}

invert_dosage <- function(tab, a, b) {
  temp <- 99
  tab[tab == a] <- temp
  tab[tab == b] <- a
  tab[tab == temp] <- b
  return(tab)
}
