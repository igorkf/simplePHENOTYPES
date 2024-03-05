# defaults
d <- formals(table_to_numeric)

tab <- load_hapmap()
meta_cols <- c("snp", "alleles", "chrom", "pos", "cm")


####################
# table_to_numeric #
####################

test_that("default arguments match", {
  expect_equal(d$code_as, "-101")
  expect_equal(d$ref_allele, NULL)
  expect_equal(eval(d$hets), c("R", "Y", "S", "W", "K", "M", "AG", "CT", "CG", "AT", "GT", "AC"))
  expect_equal(eval(d$homo), c("A", "AA", "T", "TT", "C", "CC", "G", "GG"))
  expect_equal(d$model, "Add")
  expect_equal(d$impute, "None")
  expect_equal(d$method, "frequency")
  expect_equal(d$verbose, FALSE)
})

test_that("verbose shows message", {
  expect_message(
    table_to_numeric(tab, verbose = TRUE), 
    "Numericalization in Progress..."
  )
})

test_that("code_as invalid raises error", {
  expect_error(
    table_to_numeric(tab, code_as = "123456789"), 
    "^\'code_as\' should be either \"-101\" or \"012\".$"
  )
})

test_that("code as -101 has expected possible values", {
  result <- table_to_numeric(tab, code_as = "-101", drop_extra_cols = T)
  possible_values <- sort(unique(unlist(as.vector(result))))
  expect_equal(possible_values, c(-1, 0, 1))
})

test_that("code as 012 has expected possible values", {
  result <- table_to_numeric(tab, code_as = "012", drop_extra_cols = T)
  possible_values <- sort(unique(unlist(as.vector(result))))
  expect_equal(possible_values, c(0, 1, 2))
})

test_that("method invalid raises error", {
  expect_error(
    table_to_numeric(tab, method = "ABCDEFG"), 
    "^\'method\' should be either \"frequency\" or \"reference\".$"
  )
})

test_that("ref_allele invalid length raises error", {
  expect_error(
    table_to_numeric(tab, method = "reference"),
    "^The reference allele information should have the same length as the number of markers.$"
  )
})

test_that("passing hapmap columns keeps meta columns if chosen to not drop", {
  result <- table_to_numeric(tab, drop_extra_cols = F)
  expect_in(meta_cols, colnames(result))
})

test_that("output class is data.frame", {
  result <- table_to_numeric(tab)
  expect_equal(inherits(result, "data.frame"), TRUE)
  result <- table_to_numeric(tab[, -c(1:11)])  # remove hapmap first cols
  expect_equal(inherits(result, "data.frame"), TRUE)
})
