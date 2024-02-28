# defaults
d <- formals(table_to_numeric)

# hapmap to use through the tests
path <- system.file("test.hmp.txt", package = "simplePHENOTYPES")
xx <- data.table::fread(path, data.table = F)[, -c(1:11)]
# debug(table_to_numeric)
# table_to_numeric(xx, method = "frequency")
# undebug(table_to_numeric)


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
    table_to_numeric(xx, verbose = TRUE), 
    "Numericalization in Progress..."
  )
})

test_that("code_as invalid raises error", {
  expect_error(
    table_to_numeric(xx, code_as = "123456789"), 
    "^\'code_as\' should be either \"-101\" or \"012\".$"
  )
})

test_that("code as -101 has expected possible values", {
  result <- table_to_numeric(xx, code_as = "-101")
  possible_values <- sort(unique(as.vector(result)))
  expect_equal(possible_values, c(-1, 0, 1))
})

test_that("code as 012 has expected possible values", {
  result <- table_to_numeric(xx, code_as = "012")
  possible_values <- sort(unique(as.vector(result)))
  expect_equal(possible_values, c(0, 1, 2))
})

test_that("method invalid raises error", {
  expect_error(
    table_to_numeric(xx, method = "ABCDEFG"), 
    "^\'method\' should be either \"frequency\" or \"reference\".$"
  )
})

test_that("method invalid raises error", {
  expect_error(
    table_to_numeric(xx, method = "ABCDEFG"), 
    "^\'method\' should be either \"frequency\" or \"reference\".$"
  )
})

test_that("ref_allele invalid length raises error", {
  expect_error(
    table_to_numeric(xx, method = "reference"),
    "^The reference allele information should have the same length as the number of markers.$"
  )
})


################
# make_numeric #
################

test_that("non-biallelic SNP returns NA", {
  for (method in c("frequency", "reference")) {
    set.seed(1)
    snp <- unlist(xx[1, ])  # only first SNP
    random_idxs <- sample(1:length(snp), size = as.integer(length(snp) / 2))
    snp[random_idxs] <- "ABCDEFG"
    expect_message(
      result <- make_numeric(
        snp,
        method = method,
        ref = d$ref_allele,
        model = d$model,
        impute = d$impute,
        hets = eval(d$hets),
        homo = eval(d$homo),
        AA = d$AA,
        Aa = d$Aa,
        aa = d$aa
      ),
      "non-biallelic SNP set to NA"
    )
    expect_equal(result, NA)
  }
})
