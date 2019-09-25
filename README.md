**simplePHENOTYPES**
---
## Simulation of Pleiotropic, Linked and Epistatic PHENOTYPES

author: Samuel B Fernandes
date: Sep 25, 2019

=========

This short tutorial presents some of the possible scenarios one would simulate, but it certainly does not explore all the possibilities. For more information on specific parameter input please check the help documentation (?create_phenotypes).

=========

### Installation
In order to install simplePHENOTYPES you will need to install SNPRelate and gdsfmt (both from Bioconductor), as well as lqmm and data.table (both from CRAN).  
```r
#IMPORTANT 1[space]2 is setting CRAN and Bioconductor as repositories
setRepositories()
1 2
##On Windows please include the following:
#options(download.file.method = "libcurl")

#Installing from bitbucket:
devtools::install_bitbucket("fernandessb/simplephenotypes", auth_user = "fernandessb", password = "RhtNaUHY66VdLESMRLCD", build_vignettes = TRUE, dependencies = TRUE)

#Installing from source package:
install.packages("gdsfmt")
install.packages("SNPRelate")
install.packages("lqmm")
install.packages("data.table")
install.packages("mvtnorm")
# please replace [PATH] by your path to the simplePHENOTYPES_0.1.0.tar.gz file 
# or install it using RStudio's menu option to install from Package Arquive File.
install.packages("[PATH]/simplePHENOTYPES_0.2.0.tar.gz", repos = NULL, type = "source")
```
=========

### Load example dataset
Note that the example dataset is alredy numericalized. HapMap format may also be used with options  ```genotypes_object```, ```genotypes_file``` or ```genotypes_path```.
```r
library(simplePHENOTYPES)
data("SNP55K_maize282_maf04")
SNP55K_maize282_maf04[1:8, 1:10]
cor_matrix <- matrix(c(   1, 0.4, -0.5,
                        0.4,   1,  0.7,
                       -0.5, 0.7,    1 ), 3)
```
=========

### Single Trait
Simulating 10 replications (experiments with the same genetic setting but varying the random residuals) of a single trait with a heritability of 0.7. A vector could also be used to simulate different heritabilities. In this setting, the simulated trait is controlled by 1 big effect QTN (allelic effect of 0.9) and 2 small effect QTNs with additive effects following a geometric series starting with 0.2.
Please see help files (?create_phenotypes) to check out default values being used.
```r
create_phenotypes(
  genotypes_object = SNP55K_maize282_maf04,
  additive_QTN_number = 3,
  additive_effect = 0.2,
  big_additive_QTN_effect = 0.9,
  rep = 10,
  h2 = 0.7)
```
=========

### Multiple Traits: Pleiotropic Model
Simulating 2 traits ```ntraits = 2``` controlled by the same 3 additive QTN and the same "big" effect QTN with effect of 0.3. The other 2 QTNs have allelic effects of 0.04 and 0.2, respectively. Heritability for the target trait is 0.2 and 0.4 for the correlated trait. Each replication is being recorded in a different file ```output_format = "multi-file"``` in a folder named "Results". The correlation between traits is just an artifact of the different allelic effects. No attempt is being done on simulating specific correlation between traits.
```r
create_phenotypes(
  genotypes_object = SNP55K_maize282_maf04,
  additive_QTN_number = 3,
  big_additive_QTN_effect = 0.3,
  h2 = 0.2,
  h2_MT = 0.4,
  additive_effect = c(0.04,0.2),
  ntraits = 2,
  rep = 10,
  output_format = "multi-file",
  model = "pleiotropic",
  output_dir = "Results_Pleiotropic"
)
```
=========

### Multiple Traits: Partially Pleiotropic Model
Simulating 3 traits controlled by 7, 13 and 4 QTNs, respectively. The first of the 3 pleiotropic QTNs ```overlap = 3``` will have a big additive QTN effect of 0.9. All other QTNs will have an allelic effect of 0.3 being used for the geometric series. Correlation among traits is assigned to be equal to the cor_matrix object. Each replication will be appended in a unique file in a long format and with an additional column named "Rep". Results will be saved at "Results_Partially". In this example, the genotype file will be saved as numeric.
```r
 sim_results <- create_phenotypes(
  genotypes_object = SNP55K_maize282_maf04,
  overlap = 3,
  specific_QTN_number = c(4,10, 1),
  big_additive_QTN_effect = 0.9,
  h2 = 0.2,
  h2_MT = c(0.4, 0.8),
  additive_effect = c(0.3, 0.3, 0.3),
  ntraits = 3,
  correlation = cor_matrix,
  rep = 20,
  output_dir = "Results_Partially",
  output_format = "long",
  model = "partially",
  out_geno = "numeric",
  to_r = TRUE
)
```
=========

### Multiple Traits: Linkage Dissequilibrium Model
Simulating 5 replications of 2 (current default is for 2 traits with the LD model) linked traits controlled by 3 additive QTNs with a big effect of 0.1 and a geometric series starting from 0.02. Starting seed number is 200 and output phenotypes are saved
at different columns of a single file. Plink fam, bim and bed files are also saved at Results_LD.

```r
create_phenotypes(
  genotypes_object = SNP55K_maize282_maf04,
  additive_QTN_number = 3,
  big_additive_QTN_effect = 0.1,
  h2 = 0.2,
  additive_effect = 0.02,
  rep = 5,
  seed = 200,
  output_format = "wide",
  model = "LD",
  output_dir = "Results_LD",
  out_geno = "plink",
  ld=0.8
)
```
=========

### Multiple Traits: Partially Pleiotropic Model with Epistatic effects
Simulating 3 traits controlled by 3 pleiotropic additive QTNs ```overlap = 3```, 2 pleiotropic epistatic QTNs ```overlapE = 2```,
4, 10 and 1 additive trait-specific QTN and 2, 1 and 5 epistatic trait-specific QTNs, respectively. Results will be saved as ".fam" files used as gemma input.
```r
create_phenotypes(
  genotypes_object = SNP55K_maize282_maf04,
  overlap = 3,
  overlap_e = 2,
  specific_QTN_number = c(4,10, 1),
  specific_e_QTN_number = c(2,1, 5),
  epistatic_effect = c(0.01, 0.4, 0.2),
  additive_effect = c(0.3, 0.2, 0.5),
  big_additive_QTN_effect = 0.9,
  h2 = 0.2,
  h2_MT = c(0.4, 0.8, 0.55),
  ntraits = 3,
  rep = 20,
  output_dir = "Results_Partially_E",
  output_format = "gemma",
  model = "partially"
)
```
=========

### Using your own data
If files are saved by chromosome, they may be readed from file using options ```genotypes_path``` (consider having all marker data files in a separated folder). If multiple files are saved on the same folder as the marker data, the parameter ```shared_name``` might be used to select only marker data. For example, if your data is saved as "WGS_chrm_1.hmp.txt", ..., "WGS_chrm_10.hmp.txt", one would use ```shared_name = "WGS_chrm_"``` (it simply uses regex to select files).
```r
create_phenotypes(
  genotypes_path = getwd(),
  shared_name = "WGS_chrm_",
  additive_QTN_number = 3,
  big_additive_QTN_effect = 0.1,
  h2 = 0.2,
  additive_effect = 0.02,
  rep = 5,
  seed = 200,
  output_format = "gemma",
  model = "pleiotropic",
  output_dir = "Results"
)
```
=========

### Contact
Questions, suggestions, and bug reports are welcome and appreciated.
- **Authors:** Samuel B Fernandes and Alexander E Lipka
- **Contact:** samuelf@illinois.edu
- **Institution:** [*University of Illinois at Urbana-Champaign*]
