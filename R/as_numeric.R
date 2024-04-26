#' @export
#' @param x filename as a string or a data.frame object. If passing a data.frame, the rows must be SNP ids and the columns must be the individuals.
as_numeric <- function(x, ...) {
  
  if (inherits(x, "character")) {
    lower_x <- tolower(x)
    if (endsWith(lower_x, ".hmp.txt")) {
      tab <- data.table::fread(x, data.table = F)
      result <- table_to_numeric(tab, ...)
      temp <- NULL
    } else if (endsWith(lower_x, ".vcf")) {
      r <- vcf_to_numeric(x, ...)
      result <- r$result
      temp <- r$temp
    } else if (endsWith(lower_x, ".bed")) {
      r <- bed_to_numeric(x, ...)
      result <- r$result
      temp <- r$temp
    } else if (endsWith(lower_x, ".ped")) {
      r <- ped_to_numeric(x, ...)
      result <- r$result
      temp <- r$temp
    } else {
      ext <- tools::file_ext(x)
      stop(paste0("The extension \"", ext, "\" is not accepted."))
    }
    
  } else if (inherits(x, "data.frame")) {
    cols <- colnames(x)
    temp <- NULL
    
    # try to guess data structure
    if (cols[1] %in% c("rs#", "snp.rs.id", "snp")) {
      result <- table_to_numeric(x, ...)
    } else {
      with <- withCallingHandlers(
        warning = function(cnd) {
          warning(strwrap(prefix = " ", initial = "", 
          "Assuming the data.frame only contains SNPs.
           You can change the reference allele using the argument \"ref_allele\"
           and the possible hetero and homozygote genotypes from the arguments
          \"hets\" and \"homo\"."))
        },
        return(list(result = table_to_numeric(x, ...), temp = temp))
      )
    }
  }
  
  return(list(result = result, temp = temp))
}
