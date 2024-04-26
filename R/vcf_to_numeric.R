vcf_to_numeric <- function(file, ...) {
  temp <- tempfile(fileext = ".gds")
  SNPRelate::snpgdsVCF2GDS(
    vcf.fn = file, 
    out.fn = temp,
    method = "copy.num.of.ref",
    snpfirstdim = FALSE,
    verbose = FALSE
  )
  result <- gds_to_numeric(temp, ...)
  # unlink(temp, force = TRUE)
  return(list(result = result, temp = temp))
}
