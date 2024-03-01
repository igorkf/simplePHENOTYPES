#' @export
#' @param x filename as a string.
as_numeric <- function(x, ...) {
  if (endsWith(x, ".hmp.txt")) {
    tab <- data.table::fread(x, data.table = F)
    result <- hapmap_to_numeric(tab, ...)
  } else if (endsWith(x, ".vcf")) {
    tab <- vcf_to_numeric(x, ...)
  } else {
    ext <- tools::file_ext(x)
    stop(paste0("The extension \"", ext, "\" is not implemented."))
  }
  return(tab)
}
