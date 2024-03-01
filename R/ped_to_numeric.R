ped_to_numeric <- function(file, ...) {
  map_fn <- gsub(".ped$", ".map", file)
  temp <- tempfile(fileext = ".gds")
  SNPRelate::snpgdsPED2GDS(
    ped.fn = file,
    map.fn = map_fn,
    out.gdsfn = temp,
    snpfirstdim = FALSE,
    verbose = FALSE
  )
  result <- gds_to_numeric(temp, ...)
  unlink(temp, force = TRUE)
  return(result)
}
