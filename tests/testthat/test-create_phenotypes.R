# from vignette
data("SNP55K_maize282_maf04")
SNP55K_maize282_maf04 <- SNP55K_maize282_maf04[order(SNP55K_maize282_maf04$snp), ]
rownames(SNP55K_maize282_maf04) <- NULL
SNP55K_maize282_maf04[1:8, 1:10]

# from inst
file <- system.file("test_SNP55K_maize282_AGPv2_20100513_1.hmp.txt", package = "simplePHENOTYPES")
SNP55K_maize282 <- data.table::fread(file, data.table = F)
SNP55K_maize282 <- SNP55K_maize282[SNP55K_maize282$rs %in% SNP55K_maize282_maf04$snp, ]
SNP55K_maize282 <- SNP55K_maize282[SNP55K_maize282$assayLSID != "PZE0004096321", ]  # duplicated snp
SNP55K_maize282 <- SNP55K_maize282[order(SNP55K_maize282$rs), ]
rownames(SNP55K_maize282) <- NULL
SNP55K_maize282 <- SNP55K_maize282[, -c(5:10)]
colnames(SNP55K_maize282)[1:5] <- colnames(SNP55K_maize282_maf04)[1:5]
SNP55K_maize282[1:8, 1:10]
all(SNP55K_maize282$rs == SNP55K_maize282_maf04$snp)

# constants for tests
geno_path <- "inst"

#####################
# create_phenotypes #
#####################

# test_that("multiplication works", {
#   nonnumeric <- TRUE
#   geno_file <- NULL
#   !is.null(geno_path) | !is.null(geno_file) | nonnumeric
#   results <- create_phenotypes(
#     geno_obj = SNP55K_maize282_maf04,
#     add_QTN_num = 3,
#     add_effect = 0.2,
#     big_add_QTN_effect = 0.9,
#     rep = 10,
#     h2 = 0.7,
#     model = "A",
#     home_dir = tempdir()
#   )
# })
