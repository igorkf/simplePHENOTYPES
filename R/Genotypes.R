#' Generate a numeric (dosage) HapMap file
#' @keywords internal
#' @param geno_obj = NULL,
#' @param geno_path = NULL,
#' @param nrows = Inf,
#' @param na_string = "NA",
#' @param prefix = NULL,
#' @param maf_cutoff = NULL,
#' @param SNP_effect = 'Add',
#' @param SNP_impute = 'Middle',
#' @param verbose = verbose
#' @param chr_prefix = "chr"
#' @return A numeric HapMap
#' @author Samuel Fernandes and Alexander Lipka
#' Last update: Apr 20, 2020
#'
#'------------------------------------------------------------------------------
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
      tab <- as_numeric(geno_obj, model = SNP_effect)$result
    } else {
      dosage_expected <- identical(dosage, c(-1, 0, 1))
      if (!dosage_expected) {
        dosage_str <- paste0(dosage, collapse = "")
        stop(paste0("alleles are '", dosage_str, "' but '-101' was expected."))
      }
    }
  }
  
  if (is.null(geno_obj) & !is.null(geno_path)) {
    tab <- as_numeric(geno_path, model = SNP_effect)$result
    temp <- NULL
  }
  
  meta <- tab[, colnames(tab) %in% possible_meta_cols]
  geno <- tab[, !colnames(tab) %in% possible_meta_cols]
  
  # MAF filtering
  if (!is.null(maf_cutoff)) geno <- filter_maf(geno, maf_cutoff)
  
  # imputation
  if (!is.null(SNP_impute)) geno <- impute(geno, method = SNP_impute)

  # if (grepl("\\.bed|\\.ped|\\.vcf", geno_path)) {
  #    selected
  # }
  
  # return(geno)
  return(list(
    geno_obj = geno
    # input_format = hmp$input_format,
    # out_name =  hmp$out_name,
    # temp = hmp$temp
  ))
}
