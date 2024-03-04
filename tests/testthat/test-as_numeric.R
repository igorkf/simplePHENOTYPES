# filenames
hapmap_fn <- system.file("test.hmp.txt", package = "simplePHENOTYPES")
vcf_fn <- system.file("test.vcf", package = "simplePHENOTYPES")
bed_fn <- system.file("test.bed", package = "simplePHENOTYPES")
ped_fn <- system.file("test.ped", package = "simplePHENOTYPES")

# objects
# hapmap <- data.table::fread(f1, data.table = F)
# tab <- hapmap[, -c(1:11)]  # remove hapmap columns


##############
# as_numeric #
##############

test_that("hapmap file returns a data.frame", {
  result <- as_numeric(hapmap_fn)
  expect_equal(inherits(result, "data.frame"), TRUE)
})

test_that("vcf file returns a data.frame", {
  result <- as_numeric(vcf_fn)
  expect_equal(inherits(result, "data.frame"), TRUE)
})

test_that("bed file returns a data.frame", {
  result <- as_numeric(bed_fn)
  expect_equal(inherits(result, "data.frame"), TRUE)
})

test_that("ped file raises an error", {
  result <- as_numeric(ped_fn)
  expect_equal(inherits(result, "data.frame"), TRUE)
})

test_that("data.frame object returns data.frame", {
  tab <- data.table::fread(hapmap_fn, data.table = F)
  result <- as_numeric(tab)
  expect_equal(inherits(result, "data.frame"), TRUE)
})

test_that("not accepted file extension returns error", {
  ext <- "abcdefg"
  fn <- paste0("test.", ext)
  expect_error(
    as_numeric(fn), 
    paste0("^The extension \"", ext, "\" is not accepted.$")
  )
})
