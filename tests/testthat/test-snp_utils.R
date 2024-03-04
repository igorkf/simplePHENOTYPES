# defaults
d <- formals(table_to_numeric)

tab <- load_hapmap()[-c(1:11)]  # remove hapmap meta columns


################
# make_numeric #
################

test_that("non-biallelic SNP returns NA", {
  for (method in c("frequency", "reference")) {
    set.seed(1)
    snp <- unlist(tab[1, ])  # only first SNP
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