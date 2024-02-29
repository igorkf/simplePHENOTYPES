#' @export
as_numeric <- function(x, ...) {
  
  # convert from a filename
  if (inherits(x, "character")) {
    if (endsWith(x, ".hmp.txt")) {
      tab <- data.table::fread(x, data.table = F)
      result <- table_to_numeric(tab, ...)
    }
    if (endsWith(x, ".vcf")) {
      tab <- vcf_to_numeric(x, ...)
    }
    
  # convert from an object
  } else {
    tab <- table_to_numeric(x, ...)
  }
  return(tab)
}
