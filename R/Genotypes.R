genotypes <- function(geno_obj = NULL,
                      geno_path = NULL,
                      nrows = Inf,
                      na_string = "NA",
                      prefix = NULL,
                      maf_cutoff = NULL,
                      SNP_effect = "Add",
                      SNP_impute = "Middle",
                      verbose = TRUE,
                      chr_prefix = "chr") {
  
  meta_cols <- c("snp", "alleles", "chrom", "pos", "cm")
  
  if (!is.null(geno_obj) & is.null(geno_path)) input <- geno_obj
  if (is.null(geno_obj) & !is.null(geno_path)) input <- geno_path
  if (is.null(geno_obj) & is.null(geno_path)) stop("Pass \"geno_obj\" or \"geno_path\".") 
  tab <- as_numeric(input, model = SNP_effect, impute = SNP_impute)
  meta <- tab[, meta_cols]
  geno <- tab[, !colnames(tab) %in% meta_cols]
  if (!is.null(maf_cutoff)) geno <- filter_maf(geno, maf_cutoff)
  
  return(geno)
  # return(list(
  #   geno_obj = geno_obj,
  #   input_format =  hmp$input_format,
  #   out_name =  hmp$out_name,
  #   temp = hmp$temp
  # ))
}

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
