# filenames
hapmap_fn <- system.file("test.hmp.txt", package = "simplePHENOTYPES")
vcf_fn <- system.file("test.vcf", package = "simplePHENOTYPES")

# objects
# hapmap <- data.table::fread(f1, data.table = F)
# tab <- hapmap[, -c(1:11)]  # remove hapmap columns


##############
# as_numeric #
##############

test_that("hapmap returns a data.frame", {
  tab <- as_numeric(hapmap_fn)
  expect_equal(inherits(tab, "data.frame"), TRUE)
})

# transformed from filenames
# hapmap_num <- as_numeric(f1)
# 
# # transformed from object
# hapmap_num <- as_numeric(hapmap)
# tab_num <- as_numeric(tab)