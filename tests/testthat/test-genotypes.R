hapmap_fn <- system.file("test.hmp.txt", package = "simplePHENOTYPES")
tab <- load_hapmap()


#############
# genotypes #
#############

test_that("passing geno_path returns data.frame", {
  result <- genotypes(geno_path = hapmap_fn)$geno_obj
  expect_equal(inherits(result, "data.frame"), TRUE)
})

test_that("MAF filters SNPs from the data", {
  result <- genotypes(geno_obj = tab, maf_cutoff = 0.3)$geno_obj
  expect_lt(nrow(result), nrow(tab))
})

test_that("not using geno_obj or geno_path raises an error", {
  expect_error(
    genotypes(geno_obj = NULL, geno_path = NULL), 
    "Pass 'geno_obj' or 'geno_path'."
  )
})

test_that("passing numerical data.frame with non-expected dosage raises an error", {
  code_as <- "012"
  tab <- as_numeric(tab, code_as = code_as)$result
  expect_error(
    genotypes(geno_obj = tab), 
    paste0("alleles are '", code_as, "' but '-101' was expected.")
  )
})

test_that("imputing method Middle returns non-NA genotypes", {
  # create some NAs
  tab[1:3, 13:16] <- NA
  tab[5:7, 15:18] <- NA
  result <- genotypes(geno_obj = tab, SNP_impute = "Middle")$geno_obj
  expect_equal(all(!is.na(result)), TRUE)
})