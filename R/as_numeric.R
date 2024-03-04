#' @export
#' @param x filename as a string or a data.frame object.
as_numeric <- function(x, ...) {
  
  if (inherits(x, "character")) {
    lower_x <- tolower(x)
    if (endsWith(lower_x, ".hmp.txt")) {
      tab <- data.table::fread(x, data.table = F)
      result <- table_to_numeric(tab, ...)
    } else if (endsWith(lower_x, ".vcf")) {
      result <- vcf_to_numeric(x, ...)
    } else if (endsWith(lower_x, ".bed")) {
      result <- bed_to_numeric(x, ...)
    } else if (endsWith(lower_x, ".ped")) {
      result <- ped_to_numeric(x, ...)
    } else {
      ext <- tools::file_ext(x)
      stop(paste0("The extension \"", ext, "\" is not accepted."))
    }
    
  } else if (inherits(x, "data.frame")) {
    cols <- colnames(x)
    
    # try to guess data structure
    # TODO: maybe user can say what is the data structure? (e.g. type = "vcf", or type = "hapmap")
    if (cols[1] %in% c("rs#", "snp.rs.id")) {
      result <- table_to_numeric(x, ...)
    } else {
      result <- withCallingHandlers(
        warning = function(cnd) {
          warning(strwrap(prefix = " ", initial = "", 
          "Assuming the data.frame only contains SNPs.
           You can change the reference allele using the argument \"ref_allele\"
           and change the possible hetero and homozygote genotypes from the arguments
          \"hets\" and \"homo\"."))
        },
        table_to_numeric(x, ...)
      )
    }
  }
  
  return(result)
}
