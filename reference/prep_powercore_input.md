# Prepare Input Files for `PowerCore`

Prepare input files for `PowerCore`, a program applying the advanced M
strategy with a heuristic search for establishing core sets (Kim et al.
2007; Kim et al. 2007) .

## Usage

``` r
prep_powercore_input(
  data,
  genotype,
  qualitative,
  quantitative,
  center = TRUE,
  scale = TRUE,
  always.selected = NULL,
  file.name = "PowerCore_input",
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

- center:

  either a logical value or numeric-alike vector of length equal to the
  number of columns of `x`, where ‘numeric-alike’ means that
  [`as.numeric`](https://rdrr.io/r/base/numeric.html)`(.)` will be
  applied successfully if
  [`is.numeric`](https://rdrr.io/r/base/numeric.html)`(.)` is not true.

- scale:

  either a logical value or a numeric-alike vector of length equal to
  the number of columns of `x`.

- always.selected:

  A character vector with names of individuals in the `genotype` that
  should always be selected in the core collection. The maximum length
  accepted by `MStrat` is 500.

- file.name:

  A character string of name of file where the data will be saved.

- folder.path:

  The path to folder where the input files are to be saved.

## References

Kim K, Chung H, Cho G, Ma K, Chandrabalan D, Gwag J, Kim T, Cho E, Park
Y (2007). “PowerCore: A program applying the advanced M strategy with a
heuristic search for establishing core sets.” *Bioinformatics*,
**23**(16), 2155–2162.  
  
Kim K, Chung H, Cho G, Ma K, Chandrabalan D, Gwag J, Kim T, Cho E, Park
Y (2007). “PowerCore (v. 1.0): A Program Applying the Advanced M
Strategy Using Heuristic Search for Establishing Core or Allele Mining
Sets - User Manual.” Genetic Resources Division, National Institute of
Agricultural Biotechnology (NIAB), Rural Development Administration
(RDA), R. Korea.

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

sel <- c("TMe-2906", "TMe-3412", "TMe-1374", "TMe-768", "TMe-14",
         "TMe-3284", "TMe-937", "TMe-1859", "TMe-3265", "TMe-1739",
         "TMe-972", "TMe-769", "TMe-3243", "TMe-3719", "TMe-1095",
         "TMe-893", "TMe-1262", "TMe-2083", "TMe-376", "TMe-3633",
         "TMe-1738", "TMe-2428", "TMe-259", "TMe-3457", "TMe-1406",
         "TMe-977", "TMe-3006", "TMe-925", "TMe-3671", "TMe-2779",
         "TMe-1293", "TMe-268", "TMe-266", "TMe-3562", "TMe-801")

prep_powercore_input(data = data, genotype = "Accession",
                     qualitative = qual, quantitative = quant,
                     center = TRUE, scale = TRUE,
                     always.selected = sel,
                     file.name = "PowerCore_input",
                     folder.path = tempdir())
#> PowerCore output file created at /var/folders/kg/7q73ww8s3llgyl61c9z_j5g40000gn/T//Rtmpqu70IK/PowerCore_input.csv
```
