load_hapmap <- function() {
  path <- system.file("test.hmp.txt", package = "simplePHENOTYPES")
  hapmap <- data.table::fread(path, data.table = F)
}