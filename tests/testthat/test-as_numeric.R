# filenames
hapmap_fn <- system.file("test.hmp.txt", package = "simplePHENOTYPES")
vcf_fn <- system.file("test.vcf", package = "simplePHENOTYPES")
bed_fn <- system.file("test.bed", package = "simplePHENOTYPES")

# objects
# hapmap <- data.table::fread(f1, data.table = F)
# tab <- hapmap[, -c(1:11)]  # remove hapmap columns


##############
# as_numeric #
##############

test_that("hapmap file returns a data.frame", {
  tab <- as_numeric(hapmap_fn)
  expect_equal(inherits(tab, "data.frame"), TRUE)
})

test_that("vcf file returns a data.frame", {
  tab <- as_numeric(vcf_fn)
  expect_equal(inherits(tab, "data.frame"), TRUE)
})

test_that("bed file raises an error", {
  expect_error(
    as_numeric(bed_fn), 
    paste0("The extension \"bed\" is not implemented.")
  )
})


# transformed from filenames
# hapmap_num <- as_numeric(f1)
# 
# # transformed from object
# hapmap_num <- as_numeric(hapmap)
# tab_num <- as_numeric(tab)