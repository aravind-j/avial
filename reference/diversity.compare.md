# Compare Diversity Measures

Compare Diversity Measures

## Usage

``` r
diversity.compare(
  x,
  group,
  R = 1000,
  base = exp(1),
  na.omit = TRUE,
  p.adjust.method = c("bonferroni", "holm"),
  ci.conf = 0.95,
  ci.type = c("perc", "bca"),
  q = seq(0, 3, 0.1),
  parallel = c("no", "multicore", "snow"),
  ncpus = 1L,
  cl = NULL
)
```

## Arguments

- x:

  A factor vector of categories (e.g., species, traits). The frequency
  of each level is treated as the abundance of that category.

- group:

  A factor vector indicating the group of each observation. Must have
  the same length as `x`.

- R:

  Integer specifying the number of permutations. Default is 1000.

- base:

  The logarithm base to be used for computation of shannon family of
  diversity indices. Default is `exp(1)`.

- na.omit:

  logical. If `TRUE`, missing values (`NA`) are ignored and not included
  as a distinct factor level for computation. Default is `TRUE`.

- p.adjust.method:

  (perm.test.pairwise only) Method for adjusting p-values for multiple
  comparisons. Options include `"bonferroni"` and `"holm"`. Default is
  `"bonferroni"`.

- ci.conf:

  Confidence level of the bootstrap interval. Default is 0.95.

- ci.type:

  A vector of character strings representing the type of intervals
  required. The options are `c("perc", "bca")`.

- q:

  The order of the parametric index.

- parallel:

  The type of parallel operation to be used (if any). If missing, the
  default is taken from the option `"boot.parallel"` (and if that is not
  set, `"no"`).

- ncpus:

  integer: number of processes to be used in parallel operation:
  typically one would chose this to the number of available CPUs.

- cl:

  An optional parallel or snow cluster for use if `parallel = "snow"`.
  If not supplied, a cluster on the local machine is created for the
  duration of the `boot` call.

## Examples

``` r
library(EvaluateCore)

pdata <- cassava_CC

qual <- c("CUAL", "LNGS", "PTLC", "DSTA", "LFRT", "LBTEF", "CBTR", "NMLB",
          "ANGB", "CUAL9M", "LVC9M", "TNPR9M", "PL9M", "STRP", "STRC",
          "PSTR")

# Convert qualitative data columns to factor
pdata[, qual] <- lapply(pdata[, qual], as.factor)

str(pdata)
#> 'data.frame':    168 obs. of  26 variables:
#>  $ CUAL  : Factor w/ 4 levels "Dark green","Green purple",..: 3 1 2 2 2 2 4 2 2 1 ...
#>  $ LNGS  : Factor w/ 3 levels "Long","Medium",..: 3 1 2 2 2 2 2 1 1 1 ...
#>  $ PTLC  : Factor w/ 5 levels "Dark green","Green purple",..: 3 4 4 4 4 5 4 2 2 5 ...
#>  $ DSTA  : Factor w/ 5 levels "Absent","Central part",..: 1 5 5 5 5 5 5 4 2 5 ...
#>  $ LFRT  : Factor w/ 4 levels "25-50% leaf retention",..: 1 1 1 1 3 2 2 2 2 2 ...
#>  $ LBTEF : Factor w/ 6 levels "0","1","2","3",..: 3 1 2 1 4 5 4 4 3 2 ...
#>  $ CBTR  : Factor w/ 3 levels "Cream","White",..: 2 2 2 2 1 2 1 1 1 1 ...
#>  $ NMLB  : Factor w/ 9 levels "0","1","2","3",..: 3 1 2 1 4 4 4 3 3 4 ...
#>  $ ANGB  : Factor w/ 4 levels "150-300","450-600",..: 1 4 1 4 2 2 2 1 2 2 ...
#>  $ CUAL9M: Factor w/ 5 levels "Dark green","Green",..: 1 1 3 5 3 3 5 5 5 4 ...
#>  $ LVC9M : Factor w/ 5 levels "Dark green","Green",..: 4 3 3 3 3 1 3 1 4 3 ...
#>  $ TNPR9M: Factor w/ 5 levels "1","2","3","4",..: 5 5 4 2 5 4 2 5 5 5 ...
#>  $ PL9M  : Factor w/ 2 levels "Long (25-30cm)",..: 2 2 1 1 1 1 1 1 2 2 ...
#>  $ STRP  : Factor w/ 4 levels "Absent","Intermediate",..: 2 3 1 1 1 1 4 1 1 4 ...
#>  $ STRC  : Factor w/ 2 levels "Absent","Present": 2 2 1 2 1 1 2 1 1 2 ...
#>  $ PSTR  : Factor w/ 2 levels "Irregular","Tending toward horizontal": 1 2 2 2 1 2 2 2 1 2 ...
#>  $ NMSR  : num  6 2 6 2 20 13 4 14 10 5 ...
#>  $ TTRN  : num  3 0.5 3 2 5 ...
#>  $ TFWSR : num  1.4 2.6 1.2 1.6 5 7 4.2 2.8 2.8 4 ...
#>  $ TTRW  : num  0.7 0.65 0.6 1.6 1.25 ...
#>  $ TFWSS : num  1 2.8 2.8 2.4 16 12 9 4.4 6.2 5 ...
#>  $ TTSW  : num  0.5 0.7 1.4 2.4 4 ...
#>  $ TTPW  : num  2.4 5.4 4 4 21 19 13.2 7.2 9 9 ...
#>  $ AVPW  : num  1.2 1.35 2 4 5.25 4.75 3.3 2.4 1.8 2.25 ...
#>  $ ARSR  : num  2 0 2 0 3 0 0 6 0 0 ...
#>  $ SRDM  : num  42 39.8 29.7 43 37.9 37 38.9 36.9 41 37.9 ...

diversity.compare(x = pdata$CUAL, group = pdata$LNGS, R = 100,
                  base = exp(1), na.omit = TRUE)
#> Computing diversity indices.
#> Performing global permutation tests.
#> Performing pairwise permutation tests.
#> Computing bootstrap confidence intervals.
#> Generating diversity profiles.
#> Error in diversity.profile(x = x, group = group, q = q, conf = ci.conf,     R = R, parameter = "hill", ci.type = ci.type, parallel = parallel,     ncpus = ncpus, cl = cl): unused argument (conf = ci.conf)

diversity.compare(x = pdata$ANGB, group = pdata$LNGS, R = 100,
                  base = exp(1), na.omit = TRUE)
#> Computing diversity indices.
#> Performing global permutation tests.
#> Performing pairwise permutation tests.
#> Computing bootstrap confidence intervals.
#> Generating diversity profiles.
#> Error in diversity.profile(x = x, group = group, q = q, conf = ci.conf,     R = R, parameter = "hill", ci.type = ci.type, parallel = parallel,     ncpus = ncpus, cl = cl): unused argument (conf = ci.conf)
```
