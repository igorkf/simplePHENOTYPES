#' @export
as_numeric <- function(x, ...) {
  
  # convert from a filename
  if (inherits(x, "character")) {
    if (endsWith(x, ".hmp.txt")) {
      tab <- data.table::fread(x, data.table = F)
      result <- table_to_numeric(tab, ...)
    }
    if (endsWith(x, ".txt") | endsWith(x, ".csv") ) {
      
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


# filenames
f1 <- system.file("test.hmp.txt", package = "simplePHENOTYPES")
f2 <- system.file("test.vcf", package = "simplePHENOTYPES")

# transformed from filenames
hapmap_num <- as_numeric(f1)

# objects
hapmap <- data.table::fread(f1, data.table = F)
tab <- hapmap[, -c(1:11)]  # remove hapmap columns

# transformed from object
hapmap_num <- as_numeric(hapmap)
tab_num <- as_numeric(tab)

