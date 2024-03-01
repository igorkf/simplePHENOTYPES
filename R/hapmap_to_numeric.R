#' Converts character SNP genotype to numerical (-1, 0, 1) were 1 is the major allele
#' @param xx table of genotypes.
#' @param code_as how numeric genotypes should be coded. Eiter "-101" or "012".
#' @param ref_allele a vector with the reference allele information
#' @param hets a vector with the genotype code for all possible heterozigotes in the dataset.
#' The default is used for hapmap format.
#' @param homo a vector with the genotype code for all possible homozigotes in the dataset.
#' The default is used for hapmap format.
#' @param model options: Add (AA = 1, Aa = 0, aa = -1), Dom (AA = 0, Aa = 1, aa = 0),
#'  Left (AA = 1, Aa = 1, aa = 0), Right (AA = 0, Aa = 1, aa = 1). Default is Add.
#' @param impute simple imputation method. It replaces missing values by "Major",
#' "Minor", and "Middle". The default is "None".
#' @param method method to define what is the major allele. Default is "frequency",
#' "reference" is another option. If reference is used, "ref_allele" must be provided.
#' @return Corresponding numerical value
#' Last update: Sep 29, 2021
#---------------------------------------------------------------------------
hapmap_to_numeric <-
  function(xx,
           code_as = "-101",
           ref_allele = NULL,
           hets = c("R", "Y", "S", "W", "K", "M", "AG", "CT", "CG", "AT", "GT", "AC"),
           homo = c("A", "AA", "T", "TT", "C", "CC", "G", "GG"),
           model = "Add",
           impute = "None",
           method = "frequency",
           verbose = FALSE) {
    #---------------------------------------------------------------------------

    hapmap_cols <- c(
      "rs#", "alleles", "chrom", "pos", "strand", "assembly#",
      "center", "protLSID", "assayLSID", "panelLSID", "QCcode"
    )
    has_hapmap_cols <- all(hapmap_cols %in% colnames(xx))
    if (has_hapmap_cols) {
      left_data <- xx[, hapmap_cols]
      xx[, hapmap_cols] <- NULL
    }

    if (verbose) message("Numericalization in Progress...")
    colnames <- colnames(xx)
    if (code_as == "-101") {
      AA <- 1
      Aa <- 0
      aa <- -1
    } else if (code_as == "012") {
      AA <- 2
      Aa <- 1
      aa <- 0
    } else {
      stop("\'code_as\' should be either \"-101\" or \"012\".")
    }
    if (method == "frequency") {
      xx_n <-  apply(xx, 1, function(o){
        make_numeric(a = o,
                     method = method,
                     model = model,
                     impute = impute,
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
                     impute = impute,
                     hets = hets,
                     homo = homo,
                     AA = AA,
                     Aa = Aa,
                     aa = aa)
      })
    } else {
      stop("\'method\' should be either \"frequency\" or \"reference\".")
    }
    xx_n <- t(xx_n)
    colnames(xx_n) <- colnames
    if (has_hapmap_cols) {
      xx_n <- cbind(left_data, xx_n)
    } else {
      xx_n <- as.data.frame(xx_n)
    }
    return(xx_n)
  }
