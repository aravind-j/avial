# Prepare Input Files for `MStrat`

Prepare input files for `MStrat`, a software for building germplasm core
collections by maximizing allelic or phenotypic richness (Schoen and
Brown 1993; Gouesnard et al. 2001; Gouesnard et al. 2002) .

## Usage

``` r
prep_mstrat_input(
  data,
  genotype,
  qualitative,
  quantitative,
  active,
  target,
  center = TRUE,
  scale = TRUE,
  weights.qualitative = NULL,
  weights.quantitative = NULL,
  nclass.quantitative = NULL,
  always.selected = NULL,
  file.name = "MStrat_input",
  folder.path = getwd()
)
```

## Arguments

- data:

  The data as a data frame object. The data frame should possess columns
  with the genotype names and multiple quantitative and/or qualitative
  trait/variable data.

- genotype:

  Name of column with the genotype names as a character string.

- qualitative:

  Name of columns with the qualitative traits as a character vector.

- quantitative:

  Name of columns with the quantitative traits as a character vector.

- active:

  Name of traits/variables to be declared as active.

- target:

  Name of traits/variables to be declared as target.

- center:

  either a logical value or numeric-alike vector of length equal to the
  number of columns of `x`, where ‘numeric-alike’ means that
  [`as.numeric`](https://rdrr.io/r/base/numeric.html)`(.)` will be
  applied successfully if
  [`is.numeric`](https://rdrr.io/r/base/numeric.html)`(.)` is not true.

- scale:

  either a logical value or a numeric-alike vector of length equal to
  the number of columns of `x`.

- weights.qualitative:

  An vector of weight to be applied on a qualitative traits. Should be
  `NULL` or a numeric vector of the same length as the number of
  qualitative traits. If `NULL`, the default weight of 1 is given.

- weights.quantitative:

  An vector of weight to be applied on a quantitative traits. Should be
  `NULL` or a numeric vector of the same length as the number of
  quantitative traits. If `NULL`, the default weight of 1 is given.

- nclass.quantitative:

  The number of classes into which each quantitative trait data have to
  be divided into. Should be `NULL` or a integer vector of the same
  length as the number of quantitative traits. If `NULL`, the default of
  5 is applied. MStat limits the maximum number of classes to 1000.

- always.selected:

  A character vector with names of individuals in the `genotype` that
  should always be selected in the core collection. The maximum length
  accepted by `MStrat` is 500.

- file.name:

  A character string of name of file where the data will be saved.

- folder.path:

  The path to folder where the input files are to be saved.

## Note

Any `NA` values will be considered as missing value and converted to
9999 which is the code for the same in `MStrat`.

## References

Gouesnard B, Bataillon TM, Decoux G, Rozale C, Schoen DJ, David JL
(2001). “MSTRAT: An algorithm for building germ plasm core collections
by maximizing allelic or phenotypic richness.” *Journal of Heredity*,
**92**(1), 93–94.  
  
Gouesnard B, Bataillon TM, Decoux G, Rozale C, Schoen DJ, David JL
(2002). “MStrat Documentation 1.1.” Evolutionary genomics and population
management (GE\\^{\textrm{2}}\\pop), Institut Amelioration Genetique et
Adaptation des Plantes mediterraneennes et tropicales (agAp Institute),
CIRAD, Montpellier, France.  
  
Schoen DJ, Brown AH (1993). “Conservation of allelic richness in wild
crop relatives is aided by assessment of genetic markers.” *Proceedings
of the National Academy of Sciences*, **90**(22), 10623–10627.

## Examples

``` r
library(EvaluateCore)

data(cassava_EC)
data <- cassava_EC

quant <- c("NMSR", "TTRN", "TFWSR", "TTSW", "TTPW", "AVPW",
           "ARSR", "SRDM")
qual <- c("CUAL", "LNGS", "LFRT", "LBTEF", "CBTR", "NMLB",
          "ANGB", "CUAL9M", "LVC9M", "TNPR9M", "PL9M", "STRP", "STRC",
          "PSTR")

# Prepare genotype column
data$Accession <- rownames(data)
rownames(data) <- NULL
data$Accession <- as.factor(data$Accession)

# Convert qualitative data as factors
data[, qual] <- lapply(data[, qual],
                       function(x) factor(as.factor(x)))

active = c("LNGS", "LFRT", "LBTEF", "CBTR", "NMLB",
           "ANGB", "CUAL9M", "LVC9M", "TNPR9M",
           "TTRN", "TFWSR", "TTSW", "TTPW", "AVPW")
target = c("NMSR", "TTRN", "ARSR", "SRDM",
           "CUAL", "LNGS", "TNPR9M",
           "PL9M", "STRP", "STRC",
           "PSTR")

sel <- c("TMe-2906", "TMe-3412", "TMe-1374", "TMe-768", "TMe-14",
         "TMe-3284", "TMe-937", "TMe-1859", "TMe-3265", "TMe-1739",
         "TMe-972", "TMe-769", "TMe-3243", "TMe-3719", "TMe-1095",
         "TMe-893", "TMe-1262", "TMe-2083", "TMe-376", "TMe-3633",
         "TMe-1738", "TMe-2428", "TMe-259", "TMe-3457", "TMe-1406",
         "TMe-977", "TMe-3006", "TMe-925", "TMe-3671", "TMe-2779",
         "TMe-1293", "TMe-268", "TMe-266", "TMe-3562", "TMe-801")

prep_mstrat_input(data = data, genotype = "Accession",
                  qualitative = qual, quantitative = quant,
                  active = active, target = target,
                  center = TRUE, scale = TRUE,
                  weights.qualitative = NULL,
                  weights.quantitative = NULL,
                  nclass.quantitative = NULL, always.selected = sel,
                  file.name = "MStrat_input",
                  folder.path = tempdir())
#> The following MStrat input files created at /var/folders/9r/xzfp9lgn603578400ms53lr00000gn/T//RtmptsEueh:
#> MStrat_input_data.dat
#> MStrat_input_variable.var
#> MStrat_input_kernel.ker
```
