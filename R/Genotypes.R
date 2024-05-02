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
  
  # if passing a geno_obj
  if (!is.null(geno_obj) & is.null(geno_path)) {
    input_format <- NULL
    temp <- NULL
    dosage <- get_dosage(geno_obj[, !colnames(geno_obj) %in% possible_meta_cols])
    if (!is.numeric(dosage)) {
      r <- as_numeric(geno_obj, model = SNP_effect)
      tab <- r$result
      out_name <- paste0(deparse(substitute(geno_obj)), "_numeric.txt")
    } else {
      tab <- geno_obj
      dosage_expected <- identical(dosage, c(-1, 0, 1))
      if (!dosage_expected) {
        dosage_str <- paste0(dosage, collapse = "")
        stop(paste0("alleles are '", dosage_str, "' but '-101' was expected."))
      }
    }
  }
  
  # if passing a geno_path
  if (is.null(geno_obj) & !is.null(geno_path)) {
    r <- as_numeric(geno_path, model = SNP_effect)
    tab <- r$result
    input_format <- tools::file_ext(geno_path)  # does not capture multiple-dots extensions such as hmp.txt
    if (grepl("\\.hmp\\.txt", geno_path)) {
      input_format <- paste0("hmp.", input_format)
      temp <- NULL
      out_name = sub("\\.hmp\\.txt", ".txt", geno_path)
    } else {
      temp <- r$temp
      out_name = sub(input_format, ".txt", geno_path)
    }
  }
  
  meta <- tab[, colnames(tab) %in% possible_meta_cols]
  geno <- tab[, !colnames(tab) %in% possible_meta_cols]
  
  # MAF filtering
  if (!is.null(maf_cutoff)) geno <- filter_maf(geno, maf_cutoff)
  
  # imputation
  if (!is.null(SNP_impute)) geno <- impute(geno, method = SNP_impute)
  
  # return(geno)
  # TODO: check returns
  return(list(
    geno_obj = geno,
    input_format = input_format,
    out_name =  out_name,
    temp = temp
  ))
}
