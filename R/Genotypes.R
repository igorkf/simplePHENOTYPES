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
  
  possible_meta_cols <- c(
    "rs#", "alleles", "chrom", "pos", "strand", "assembly#",
    "center", "protLSID", "assayLSID", "panelLSID", "QCcode",
    "snp", "allele", "chr", "pos", "cm"
  )
  
  if (is.null(geno_obj) & is.null(geno_path)) stop("Pass 'geno_obj' or 'geno_path'.")
  
  if (!is.null(geno_obj) & is.null(geno_path)) {
    dosage <- get_dosage(geno_obj[, !colnames(geno_obj) %in% possible_meta_cols])
    if (!is.numeric(dosage)) {
      tab <- as_numeric(geno_obj, model = SNP_effect)
    } else {
      dosage_expected <- identical(dosage, c(-1, 0, 1))
      if (!dosage_expected) {
        dosage_str <- paste0(dosage, collapse = "")
        stop(paste0("alleles are '", dosage_str, "' but '-101' was expected."))
      }
    }
  }
  
  if (is.null(geno_obj) & !is.null(geno_path)) {
    tab <- as_numeric(geno_path, model = SNP_effect)
  }
  
  meta <- tab[, colnames(tab) %in% possible_meta_cols]
  geno <- tab[, !colnames(tab) %in% possible_meta_cols]
  
  # MAF filtering
  if (!is.null(maf_cutoff)) geno <- filter_maf(geno, maf_cutoff)
  
  # imputation
  if (!is.null(SNP_impute)) geno <- impute(geno, method = SNP_impute)
  
  return(geno)
  # return(list(
  #   geno_obj = geno_obj,
  #   input_format =  hmp$input_format,
  #   out_name =  hmp$out_name,
  #   temp = hmp$temp
  # ))
}
