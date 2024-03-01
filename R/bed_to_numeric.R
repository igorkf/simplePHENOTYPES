bed_to_numeric <- function(file, ...) {
  fam_fn <- gsub(".bed$", ".fam", file)
  bim_fn <- gsub(".bed$", ".bim", file)
  temp <- tempfile(fileext = ".gds")
  SNPRelate::snpgdsBED2GDS(
    bed.fn = file,
    fam.fn = fam_fn,
    bim.fn = bim_fn,
    out.gdsfn = temp,
    snpfirstdim = FALSE,
    verbose = FALSE
  )
  result <- gds_to_numeric(temp, ...)
  unlink(temp, force = TRUE)
  return(result)
}
