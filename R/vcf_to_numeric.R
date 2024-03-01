vcf_to_numeric <- function(file, code_as = "-101") {
  temp <- tempfile(fileext = ".gds")
  SNPRelate::snpgdsVCF2GDS(
    vcf.fn = file, 
    out.fn = temp,
    method = "copy.num.of.ref",
    snpfirstdim = FALSE,
    verbose = FALSE
  )
  result <- gds_to_numeric(temp)
  return(result)
}
