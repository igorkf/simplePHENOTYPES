gds_to_numeric <- function(file, code_as = "-101") {
  genofile <- SNPRelate::snpgdsOpen(file, allow.duplicate = TRUE)
  tab_geno <- SNPRelate::snpgdsGetGeno(genofile, snpfirstdim = TRUE, verbose = FALSE)  # 0,1,2
  if (code_as == "-101") {
    tab_geno <- tab_geno - 1
  }
  n <- nrow(tab_geno)
  
  child_nodes <- gdsfmt::ls.gdsn(genofile)
  if (!("snp.rs.id" %in% child_nodes) & !("snp.id" %in% child_nodes)) {
    stop("No SNP id information was found.")
  }
  
  result <- data.frame(
    snp.rs.id = rep(NA, n),
    snp.id = rep(NA, n),
    snp.allele = rep(NA, n),
    snp.chromosome = rep(NA, n),
    snp.position = rep(NA, n),
    cm = rep(NA, n)
  )
  if ("snp.rs.id" %in% child_nodes) {
    result$snp.rs.id <- gdsfmt::read.gdsn(gdsfmt::index.gdsn(genofile, "snp.rs.id"))
  } 
  if ("snp.id" %in% child_nodes) {
    result$snp.id <- gdsfmt::read.gdsn(gdsfmt::index.gdsn(genofile, "snp.id"))  
  }
  if ("snp.allele" %in% child_nodes) {
    result$snp.allele <- gdsfmt::read.gdsn(gdsfmt::index.gdsn(genofile, "snp.allele"))  
  }
  if ("snp.chromosome" %in% child_nodes) {
    result$snp.chromosome <- gdsfmt::read.gdsn(gdsfmt::index.gdsn(genofile, "snp.chromosome"))  
  }
  if ("snp.position" %in% child_nodes) {
    result$snp.position <- gdsfmt::read.gdsn(gdsfmt::index.gdsn(genofile, "snp.position"))  
  }
  colnames(result) <- c("snp.rs.id", "snp.id", "allele", "chr", "pos", "cm")
  result <- cbind(result, tab_geno)
  
  # rename samples if they exist
  if ("sample.id" %in% child_nodes) {
    sample.id <- gdsfmt::read.gdsn(gdsfmt::index.gdsn(genofile, "sample.id"))
    last_cols <- tail(colnames(result), ncol(tab_geno))
    colnames(result)[colnames(result) %in% last_cols] <- sample.id
  }
  SNPRelate::snpgdsClose(genofile)
  return(result)
}
