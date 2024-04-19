# defaults
d <- formals(table_to_numeric)

tab <- load_hapmap()[-c(1:11)]  # remove hapmap meta columns
snp <- unlist(tab[1, ])  # just first SNP


################
# make_numeric #
################

test_that("SNP with NAs maintains NAs", { 
  result <- make_numeric(
    replace(snp, 10:20, NA),
    method = d$method,
    ref = NULL,
    model = d$model,
    hets = eval(d$hets),
    homo = eval(d$homo),
    AA = 1,
    Aa = 0,
    aa = -1
  )
  expect_equal(any(is.na(result)), TRUE)
})

test_that("model Add returns expected dosage with method frequency", {
  result <- make_numeric(
      snp,
      method = "frequency",
      ref = NULL,
      model = "Add",
      hets = eval(d$hets),
      homo = eval(d$homo),
      AA = 1,
      Aa = 0,
      aa = -1
  )
  possible_values <- sort(unique(result))
  expect_identical(possible_values, c(-1, 0, 1))
})

test_that("model Dom returns expected dosage with method frequency", {
  result <- make_numeric(
    snp,
    method = "frequency",
    ref = NULL,
    model = "Dom",
    hets = eval(d$hets),
    homo = eval(d$homo),
    AA = 1,
    Aa = 0,
    aa = -1
  )
  possible_values <- sort(unique(result))
  expect_identical(possible_values, c(0, 1))
})

test_that("model Left returns expected dosage with method frequency", {
  result <- make_numeric(
    snp,
    method = "frequency",
    ref = NULL,
    model = "Left",
    hets = eval(d$hets),
    homo = eval(d$homo),
    AA = 1,
    Aa = 0,
    aa = -1
  )
  possible_values <- sort(unique(result))
  expect_identical(possible_values, c(-1, 1))
})

test_that("model Right returns expected dosage with method frequency", {
  result <- make_numeric(
    snp,
    method = "frequency",
    ref = NULL,
    model = "Right",
    hets = eval(d$hets),
    homo = eval(d$homo),
    AA = 1,
    Aa = 0,
    aa = -1
  )
  possible_values <- sort(unique(result))
  expect_identical(possible_values, c(-1, 1))
})
