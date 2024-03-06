hapmap_fn <- system.file("test.hmp.txt", package = "simplePHENOTYPES")
tab <- load_hapmap()


#############
# genotypes #
#############

test_that("passing geno_path returns data.frame", {
  result <- genotypes(geno_path = hapmap_fn)
  expect_equal(inherits(result, "data.frame"), TRUE)
})

test_that("MAF filters SNPs from the data", {
  result <- genotypes(geno_obj = tab, maf_cutoff = 0.3)
  expect_lt(nrow(result), nrow(tab))
})

test_that("not using geno_obj or geno_path raises an error", {
  expect_error(
    genotypes(geno_obj = NULL, geno_path = NULL), 
    "Pass \"geno_obj\" or \"geno_path\"."
  )
})