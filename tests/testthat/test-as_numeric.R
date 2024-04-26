# filenames
hapmap_fn <- system.file("test.hmp.txt", package = "simplePHENOTYPES")
vcf_fn <- system.file("test.vcf", package = "simplePHENOTYPES")
bed_fn <- system.file("test.bed", package = "simplePHENOTYPES")
ped_fn <- system.file("test.ped", package = "simplePHENOTYPES")


##############
# as_numeric #
##############

test_that("not accepted file extension returns error", {
  ext <- "abcdefg"
  fn <- paste0("test.", ext)
  expect_error(
    as_numeric(fn), 
    paste0("^The extension \"", ext, "\" is not accepted.$")
  )
})

test_that("hapmap file returns a data.frame", {
  result <- as_numeric(hapmap_fn)$result
  expect_equal(inherits(result, "data.frame"), TRUE)
})

test_that("vcf file returns a data.frame", {
  result <- as_numeric(vcf_fn)$result
  expect_equal(inherits(result, "data.frame"), TRUE)
})

test_that("bed file returns a data.frame", {
  result <- as_numeric(bed_fn)$result
  expect_equal(inherits(result, "data.frame"), TRUE)
})

test_that("ped file raises an error", {
  result <- as_numeric(ped_fn)$result
  expect_equal(inherits(result, "data.frame"), TRUE)
})

test_that("SNPs data.frame object returns data.frame", {
  set.seed(1)
  tab <- sim_snps(nrows = 10, ncols = 50, AA = "AA", Aa = "AB", aa = "BB")
  result <- as_numeric(tab, hets = c("AB"), homo = c("AA", "BB"))$result
  expect_equal(inherits(result, "data.frame"), TRUE)
})

test_that("hapmap data.frame object returns data.frame", {
  tab <- data.table::fread(hapmap_fn, data.table = F)
  result <- as_numeric(tab)$result
  expect_equal(inherits(result, "data.frame"), TRUE)
})
