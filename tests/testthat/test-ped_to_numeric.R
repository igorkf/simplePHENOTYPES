file <- system.file("test.ped", package = "simplePHENOTYPES")
meta_cols <- c("snp", "allele", "chr", "pos", "cm")


##################
# ped_to_numeric #
##################

test_that("code as -101 has expected possible values", {
  result <- ped_to_numeric(file, code_as = "-101")
  geno <- result[!colnames(result) %in% meta_cols]
  possible_values <- sort(unique(unlist(as.vector(geno))))
  expect_equal(possible_values, c(-1, 0, 1))
})

test_that("code as 012 has expected possible values", {
  result <- ped_to_numeric(file, code_as = "012")
  geno <- result[!colnames(result) %in% meta_cols]
  possible_values <- sort(unique(unlist(as.vector(geno))))
  expect_equal(possible_values, c(0, 1, 2))
})
