#' Converts character SNP genotype to numerical (-1, 0, 1) where 1 is the major allele
#' @param xx table of genotypes.
#' @param code_as how numeric genotypes should be coded. Eiter "-101" or "012".
#' @param ref_allele a vector with the reference allele information
#' @param hets a vector with the genotype code for all possible heterozigotes in the dataset.
#' The default is used for hapmap format.
#' @param homo a vector with the genotype code for all possible homozigotes in the dataset.
#' The default is used for hapmap format.
#' @param model options: Add (AA = 1, Aa = 0, aa = -1), Dom (AA = 0, Aa = 1, aa = 0),
#'  Left (AA = 1, Aa = 1, aa = 0), Right (AA = 0, Aa = 1, aa = 1). Default is Add.
#' @param method method to define what is the major allele. Default is "frequency",
#' "reference" is another option. If reference is used, "ref_allele" must be provided.
#' @return Corresponding numerical value
#' Last update: Sep 29, 2021
#---------------------------------------------------------------------------
table_to_numeric <- function(xx,
                             code_as = "-101",
                             ref_allele = NULL,
                             hets = c("R", "Y", "S", "W", "K", "M", "AG", "CT", "CG", "AT", "GT", "AC"),
                             homo = c("A", "AA", "T", "TT", "C", "CC", "G", "GG"),
                             model = "Add",
                             method = "frequency",
                             verbose = FALSE,
                             drop_extra_cols = FALSE) {
  #---------------------------------------------------------------------------
  
  hapmap_cols <- c(
    "rs#", "alleles", "chrom", "pos", "strand", "assembly#",
    "center", "protLSID", "assayLSID", "panelLSID", "QCcode"
  )
  gdslike_cols <- c(
    "snp", "allele", "chr", "pos", "cm"
  )
  extra_cols_set <- list(
    hapmap = hapmap_cols, 
    gdslike = gdslike_cols
  )
  for (s in extra_cols_set) {
    has_extra_cols <- all(s %in% colnames(xx))
    if (has_extra_cols == T) {
      left_data <- xx[, s]
      xx[, s] <- NULL
      break
    }
  }
  
  cols <- colnames(xx)
  if (verbose) message("Numericalization in Progress...")
  if (code_as == "-101") {
    AA <- 1
    Aa <- 0
    aa <- -1
  } else if (code_as == "012") {
    AA <- 2
    Aa <- 1
    aa <- 0
  } else {
    stop("'code_as' should be either '-101' or '012'.")
  }
  if (method == "frequency") {
    xx_n <-  apply(xx, 1, function(o){
      make_numeric(a = o,
                   method = method,
                   model = model,
                   hets = hets,
                   homo = homo,
                   AA = AA,
                   Aa = Aa,
                   aa = aa)
    })
  } else if (method == "reference") {
    if (length(ref_allele) != nrow(xx)) {
      stop("The reference allele information should have the same length as the number of markers.")
    }
    xx$reference_allele <- ref_allele
    xx_n <-  apply(xx, 1, function(i) {
      make_numeric(a = i[-length(i)],
                   ref = i[["reference_allele"]],
                   method = method,
                   model = model,
                   hets = hets,
                   homo = homo,
                   AA = AA,
                   Aa = Aa,
                   aa = aa)
    })
  } else {
    stop("'method' should be either 'frequency' or 'reference'.")
  }
  xx_n <- t(xx_n)
  colnames(xx_n) <- cols
  if (has_extra_cols & drop_extra_cols == F) {
    # if hapmap, output only some columns
    if (all(hapmap_cols %in% colnames(left_data))) {
      left_data <- modify_hapmap(left_data)
    }
    xx_n <- cbind(left_data, xx_n)
  } else {
    xx_n <- as.data.frame(xx_n)
  }
  return(xx_n)
}

modify_hapmap <- function(tab) {
  tab <- tab[, c("rs#", "alleles", "chrom", "pos")]
  tab$cm <- NA
  colnames(tab) <- c("snp", "allele", "chr", "pos", "cm")
  return(tab)
}
