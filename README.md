
<!-- README.md is generated from README.Rmd. Please edit that file -->

# simplePHENOTYPES

<!-- badges: start -->

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/simplePHENOTYPES)](https://CRAN.R-project.org/package=simplePHENOTYPES)
[![](https://img.shields.io/badge/Issues-%2B-brightgreen.svg)](https://github.com/samuelbfernandes/simplePHENOTYPES/issues)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/simplePHENOTYPES?color=blue)](https://cran.r-project.org/package=simplePHENOTYPES)

<!-- badges: end -->

simplePHENOTYPES aims to make it simple to simulate traits under
pleiotropy, partial-pleiotropy and linkage disequilibrium.

This short tutorial presents some of the possible genetic settings one
could simulate, but it certainly does not explore all the possibilities.
For more information on specific input parameters, please check the help
documentation (?create\_phenotypes).

# Installation

In order to install simplePHENOTYPES, the following r packages will also
be installed:

  - From Bioconductor:
      - SNPRelate
      - gdsfmt
  - From CRAN:
      - mvtnorm  
      - lqmm  
      - data.table

<!-- end list -->

``` r
setRepositories(ind = 1:2)
devtools::install_github("samuelbfernandes/simplePHENOTYPES", build_vignettes = TRUE)
```

# Load Sample Data set

Note that the data set used in all vignettes is already in numeric
format. In addition to the numeric format, simplePHENOTYPES’ parameter
`geno_obj` also takes an R object in HapMap format as input. Other input
options are VCF, GDS, and Plink bed/ped. These last formats should be
loaded from file with `geno_file` or `geno_path`.

``` r
library(simplePHENOTYPES)
data("SNP55K_maize282_maf04")
SNP55K_maize282_maf04[1:8, 1:10]
```

# Single Trait

The simplest option is the simulation of univariate traits. In the
example below, we are simulating ten single trait experiments with a
heritability of 0.7. In this setting, the simulated trait is controlled
by one large-effect QTN (`big_add_QTN_effect = 0.9`) and two small
effect QTNs. The additive effects of these last two QTNs follow a
geometric series starting with 0.2. Thus, the effect size of the first
of these two QTNs is 0.2, and the effect size of the second is
0.2<sup>2</sup>. Results are being saved at a temporary directory
(`home_dir = tempdir()`). Please see help files (?create\_phenotypes) to
see which default values are being used.

``` r
create_phenotypes(
  geno_obj = SNP55K_maize282_maf04,
  add_QTN_num = 3,
  add_effect = 0.2,
  big_add_QTN_effect = 0.9,
  rep = 10,
  h2 = 0.7,
  model = "A",
  home_dir = tempdir())
```

# Multiple Traits: Pleiotropy Architecture

simplePHENOTYPES provides three multi-trait simulation scenarios:
pleiotropy, partial pleiotropy, and spurious pleiotropy. In this
example, we are simulating three (`ntraits = 3`) pleiotropic
(`architecture = "pleiotropic"`) trait controlled by three additive and
four dominance QTNs. The effect size of the largest-effect additive QTN
is 0.3 for all traits (`big_add_QTN_effect = c(0.3, 0.3, 0.3)`), while
the additive and dominance effect sizes are 0.04, 0.2, and 0.1 for each
trait, respectively. Heritability for trait\_1 is 0.2, while the
heritability of the two correlated traits is 0.4. Each replicate is
being recorded in a different file (`output_format = "multi-file"`) in a
folder named “Results\_Pleiotropic”. In this setting, we do not specify
the correlation between traits; instead, the observed (realized)
correlation is an artifact of different allelic effects for each trait.
The same QTNs are used to generate phenotypes in all ten replications
(`vary_QTN = FALSE`)(default); alternatively, we could select different
QTNs in each replicate using `vary_QTN = TRUE`. As mentioned above, the
first QTN of each trait will get the effect provided by
big\_add\_QTN\_effect; all other QTNs will have the effect size assigned
by `add_effect` and `dom_effect`. The vector `add_effect` contains one
allelic effect for each trait, and a geometric series (default) is being
used to generate allelic effects for each one of the two additive QTNs
(`add_QTN_num = 3`) and three dominance QTNs (`dom_QTN_num = 4`). All
results will be saved to file, and a data.frame with all phenotypes will
be assigned to an object called “test1” (to\_r = TRUE).

``` r
 test1 <-  create_phenotypes(
    geno_obj = SNP55K_maize282_maf04,
    add_QTN_num = 3,
    dom_QTN_num = 4,
    big_add_QTN_effect = c(0.3, 0.3, 0.3),
    h2 = c(0.2, 0.4, 0.4),
    add_effect = c(0.04,0.2,0.1),
    dom_effect = c(0.04,0.2,0.1),
    ntraits = 3,
    rep = 10,
    vary_QTN = FALSE,
    output_format = "multi-file",
    architecture = "pleiotropic",
    output_dir = "Results_Pleiotropic",
    to_r = TRUE,
    seed = 10,
    model = "AD",
    sim_method = "geometric",
  home_dir = tempdir()
  )
```

Optionally, we may input a list of allelic effects (`sim_method =
"custom"`). In the example below, a geometric series (custom\_geometric)
is being assigned and should generate the same simulated data as the
previous example (all.equal(test1, test2)). Notice that since
`big_add_QTN_effect` is non-NULL, we only need to provide effects for
two out of the three simulated additive QTNs. On the other hand, all
four dominance QTN must have an effect assigned on the
custom\_geometric\_d list. Importantly, the allelic effects are assigned
to each trait based on the order they appear in the list and not based
on the names, i.e., ‘trait\_1’, ‘trait\_2’, and ‘trait\_3’.

``` r
 custom_geometric_a <- list(trait_1 = c(0.04, 0.0016),
                         trait_2 = c(0.2, 0.04),
                         trait_3 = c(0.1, 0.01))
 custom_geometric_d <- list(trait_1 = c(0.04, 0.0016, 6.4e-05, 2.56e-06),
                         trait_2 = c(0.2, 0.04, 0.008, 0.0016),
                         trait_3 = c(0.1, 0.01, 0.001, 1e-04))

 test2 <-  create_phenotypes(
   geno_obj = SNP55K_maize282_maf04,
   add_QTN_num = 3,
   dom_QTN_num = 4,
   big_add_QTN_effect = c(0.3, 0.3, 0.3),
   h2 = c(0.2,0.4, 0.4),
   add_effect = custom_geometric_a,
   dom_effect = custom_geometric_d,
   ntraits = 3,
   rep = 10,
   vary_QTN = FALSE,
   output_format = "multi-file",
   architecture = "pleiotropic",
   output_dir = "Results_Pleiotropic",
   to_r = T,
   sim_method = "custom",
   seed = 10,
   model = "AD",
  home_dir = tempdir()
 )
 
 all.equal(test1, test2)
```

# Multiple Traits: Partial Pleiotropy Architecture

In this example, we simulate 20 replicates of three partially
pleiotropic traits (`architecture = "partially"`), which are
respectively controlled by seven, 13, and four QTNs. All QTNs will have
additive effects that follow a geometric series, where the effect size
of the i<sup>th</sup> QTN is add\_effect^i. For instance, trait\_2 is
controlled by three pleiotropic additive QTNs and ten trait-specific
additive QTNs; consequently, the first pleiotropic additive QTN will
have an additive effect of 0.33 and the 13<sup>th</sup> trait-specific
additive QTN will have an effect of 0.33<sup>13</sup>. Correlation among
traits is assigned to be equal to the cor\_matrix object. All 20
replicates of these three simulated traits will be saved in one file,
specifically in a long format and with an additional column named “Rep”.
Results will be saved in a directory called “Results\_Partially”. In
this example, the genotype file will also be saved in numeric format.

``` r
cor_matrix <- matrix(c(   1, 0.3, -0.9,
                        0.3,   1,  -0.5,
                       -0.9, -0.5,    1 ), 3)

sim_results <- create_phenotypes(
  geno_obj = SNP55K_maize282_maf04,
  ntraits = 3,
  pleio_a = 3,
  pleio_e = 2,
  same_add_dom_QTN = TRUE,
  degree_of_dom = 0.5,
  trait_spec_a_QTN_num = c(4, 10, 1),
  trait_spec_e_QTN_num = c(3, 2, 5),
  h2 = c(0.2, 0.4, 0.8),
  add_effect = c(0.5, 0.33, 0.2),
  epi_effect = c(0.3, 0.3, 0.3),
  cor = cor_matrix,
  rep = 20,
  output_dir = "Results_Partially",
  output_format = "long",
  architecture = "partially",
  out_geno = "numeric",
  to_r = TRUE,
  model = "AE",
  home_dir = tempdir()
)
```

# Multiple Traits: Spurious Pleiotropy Architecture

Another architecture implemented is Spurious Pleiotropy. In this case,
we have two options: direct or indirect LD (`type_of_ld = "indirect"`).
In the example below, we simulate a case of indirect LD with five
replicates of two traits controlled by three additive QTNs each. For
each QTN, a marker is first selected (intermediate marker), and then two
separate markers (one upstream and another downstream) are picked to be
QTNs for each of the two traits. This QTN selection is based on an
r<sup>2</sup> threshold of at most 0.8 (`ld=0.8`) with the intermediate
marker. The three QTNs will have additive effects that follow a
geometric series, where the effect size of the i<sup>th</sup> QTN is
0.02<sup>i</sup> for one trait and 0.05<sup>i</sup> for the other trait.
Starting seed number is 200, and output phenotypes are saved in one
file, but in a “wide” format with each replicate of two traits being
added as additional columns. Plink fam, bim, and bed files are also
saved at Results\_LD.

``` r
create_phenotypes(
  geno_obj = SNP55K_maize282_maf04,
  add_QTN_num = 3,
  h2 = c(0.2, 0.4),
  add_effect = c(0.02, 0.05),
  rep = 5,
  seed = 200,
  output_format = "wide",
  architecture = "LD",
  output_dir = "Results_LD",
  out_geno = "plink",
  remove_QTN = TRUE,
  ld=0.8,
  model = "A",
  type_of_ld = "indirect",
  home_dir = tempdir()
)
```

# Multiple Traits: Partial Pleiotropy Architecture with other useful parameters

The example below simulates five replicates of three traits. In each
replicate, different SNPs are selected to be the QTNs for each
experiment (`vary_QTN = TRUE`). These traits are controlled by three
pleiotropic (`pleio = 3`) additive and dominance QTNs (`same_add_dom_QTN
= TRUE` and `degree_of_dom = 1`); two pleiotropic epistatic QTNs
(`pleio_e = 2`); four, ten and one trait-specific additive and dominance
QTNs (`trait_spec_a_QTN_num = c(4, 10, 1)`); and two, one and five
epistatic trait-specific epistatic QTNs (`trait_spec_e_QTN_num =
c(2, 1, 5)`). In addition to the default parameters, each genetic
architecture may be simulated with many auxiliary features. For
instance, we may be interested in outputting the amount of variance
explained by each simulated QTN (`QTN_variance = TRUE`) or setting a
residual correlation between traits (`cor_res = residual`) and thus,
change the default option of independent residuals. Notice that in this
example, the heritability is a 2x3 matrix (`h2 = heritability`). Each
column of the matrix “heritability” will be assigned to a different
trait. In this case, simplePHENOTYPES will loop over each row of `h2`,
keeping all other variables constant. Since rep = 5 and nrow(h2) = 2,
ten experiments will be simulated and saved in separate files. Simulated
results will be saved as “.fam” files used as GEMMA input.
Simultaneously, one genotypic file without the QTNs for the simulated
traits will be saved for each replication. Due to the option `vary_QTN =
TRUE`, each experiment will be simulated with different QTNs; thus, if
we opt for `remove_QTN = TRUE`, many potentially large files will be
saved in the output\_dir folder. By default, simplePHENOTYPES will ask
us if all these files should be saved. To avoid this question, we may
use `warning_file_saver = FALSE`. In the present example, ten plink bed
files (which is also the input for GEMMA) are saved. Genotypic files for
rep one will be named `SNP55K_maize282_maf04_noQTN_rep_1.bed`,
`SNP55K_maize282_maf04_noQTN_rep_1.bim`, and
`SNP55K_maize282_maf04_noQTN_rep_1.fam`, whereas the phenotypic file
will be saved as `Simulated_Data__Rep1_Herit_0.2_0.8_0.7.fam`.
Importantly, the file `SNP55K_maize282_maf04_noQTN_rep_1.fam` does not
contain the phenotypic data and needs to be replaced by
`Simulated_Data__Rep1_Herit_0.2_0.8_0.7.fam` prior to its use by GEMMA
or other software that uses bed files. A parameter particularly useful,
especially when simulating dominance, is `constraints`. Here we only
“include” heterozygote SNPs to be used as QTNs ( `constraints =
list(maf_above = 0.3, maf_below = 0.44, hets = "include")`). Optionally,
we may “remove” all the heterozygotes from consideration. The other
constrain options used here are to select only QTNs with minor allele
frequency between 0.3 and 0.44.

``` r
residual <- matrix(c(1, 0.1,-0.2,
                     0.1, 1,-0.1,-0.2,-0.1, 1), 3)
heritability <- matrix(c(0.2, 0.4, 0.8,
                         0.6, 0.7, 0.2), 2)
create_phenotypes(
  geno_obj = SNP55K_maize282_maf04,
  pleio_a = 3,
  pleio_e = 2,
  same_add_dom_QTN = TRUE,
  degree_of_dom = 1,
  trait_spec_a_QTN_num = c(4, 10, 1),
  trait_spec_e_QTN_num = c(2, 1, 5),
  epi_effect = c(0.01, 0.4, 0.2),
  add_effect = c(0.3, 0.2, 0.5),
  h2 = heritability,
  ntraits = 3,
  rep = 5,
  vary_QTN = TRUE,
  warning_file_saver = FALSE,
  output_dir = "Results_Partially_ADE",
  output_format = "gemma",
  architecture = "partially",
  model = "ADE",
  QTN_variance = TRUE,
  remove_QTN = TRUE,
  home_dir = tempdir(),
  constraints = list(
    maf_above = 0.3,
    maf_below = 0.44,
    hets = "include"
  ),
  cor_res = residual
)
```

# Using Multiple Marker Data Files

If files are saved by chromosome, they can be read directly into
create\_phenotypes using options `geno_path` (recommendation: consider
having all marker data files in a separate folder). If multiple files
are saved in the same folder as the marker data, the parameter `prefix`
might be used to select only the marker data. For example, if your data
is saved as “WGS\_chrm\_1.hmp.txt”, …, “WGS\_chrm\_10.hmp.txt”, one
would use `prefix = "WGS_chrm_"` .

``` r
create_phenotypes(
  geno_path = "PATH/TO/FILE",
  prefix = "WGS_chrm_",
  add_QTN_num = 3,
  h2 = 0.2,
  add_effect = 0.02,
  rep = 5,
  seed = 200,
  output_format = "gemma",
  output_dir = "Results",
  model = "ADE",
  home_dir = tempdir()
)
```

# Contact

Questions, suggestions, and bug reports are welcome and appreciated.

Author: Samuel B Fernandes and Alexander E Lipka

Contact: <samuelf@illinois.edu> or <fernandessb101@gmail.com>

Institution: University of Illinois at Urbana-Champaign
