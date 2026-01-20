# Permutation Tests

These functions perform permutation-based hypothesis tests to compare
groups with respect to a summary statistic (e.g., mean, diversity
index).

- `perm.test.global` performs a global test across all groups
  simultaneously, using a weighted sum of squares between group summary
  indices as the test statistic

- `perm.test.pairwise` performs pairwise tests between all combinations
  of groups, comparing the absolute difference in summary statistics and
  optionally adjusting p-values for multiple comparisons.

## Usage

``` r
perm.test.global(
  x,
  group,
  fun,
  R = 1000,
  parallel = c("no", "multicore", "snow"),
  ncpus = 1L,
  cl = NULL,
  ...
)

perm.test.pairwise(
  x,
  group,
  fun,
  R = 1000,
  p.adjust.method = c("bonferroni", "holm"),
  parallel = c("no", "multicore", "snow"),
  ncpus = 1L,
  cl = NULL,
  ...
)
```

## Arguments

- x:

  A numeric or factor vector of observations.

- group:

  A factor vector indicating the group of each observation. Must have
  the same length as `x`.

- fun:

  A function to summarize values within each group.

- R:

  Integer specifying the number of permutations. Default is 1000.

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

- ...:

  Additional arguments passed to `fun`.

- p.adjust.method:

  (perm.test.pairwise only) Method for adjusting p-values for multiple
  comparisons. Options include `"bonferroni"` and `"holm"`. Default is
  `"bonferroni"`.

## Value

- `perm.test.global`:

  A list of the following elements.

  test_stat

  :   The test statistic value.

  observed_values

  :   The observed values for each group.

  p_value

  :   The p value.

- `perm.test.pairwise`:

  A data frame of the following columns.

  Comparison

  :   The comparison.

  p.value

  :   The p value.

  adj.p.value

  :   The adjusted p value.

## Examples

``` r
library(EvaluateCore)

pdata <- cassava_CC

qual <- c("CUAL", "LNGS", "PTLC", "DSTA", "LFRT", "LBTEF", "CBTR", "NMLB",
          "ANGB", "CUAL9M", "LVC9M", "TNPR9M", "PL9M", "STRP", "STRC",
          "PSTR")

# Conver '#t qualitative data columns to factor
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

# Global tests ----

perm.test.global(x = pdata$NMSR, group = pdata$CUAL, fun = mean,
                 R = 100)
#> $test_stat
#> [1] 226.7998
#> 
#> $observed_values
#>   Dark green Green purple  Light green       Purple 
#>     9.612903    11.157303     6.333333    11.928571 
#> 
#> $p_value
#> [1] 0.3564356
#> 

perm.test.global(x = pdata$LNGS, group = pdata$CUAL, fun = shannon,
                 R = 100)
#> $test_stat
#> [1] 4.928841
#> 
#> $observed_values
#>   Dark green Green purple  Light green       Purple 
#>    1.5143563    1.4143497    0.6500224    1.2130604 
#> 
#> $p_value
#> [1] 0.05940594
#> 

perm.test.global(x = pdata$PTLC, group = pdata$CUAL, fun = simpson,
                 R = 100)
#> $test_stat
#> [1] 0.8980326
#> 
#> $observed_values
#>   Dark green Green purple  Light green       Purple 
#>    0.3048907    0.4412322    0.3888889    0.5272109 
#> 
#> $p_value
#> [1] 0.2475248
#> 

# Pairwise tests ----

perm.test.pairwise(x = pdata$NMSR, group = pdata$CUAL, fun = mean,
                   R = 100)
#>                    Comparison   p.value adj.p.value
#> 1  Dark green vs Green purple 0.4059406   1.0000000
#> 2   Dark green vs Light green 0.2178218   1.0000000
#> 3        Dark green vs Purple 0.1683168   1.0000000
#> 4 Green purple vs Light green 0.1386139   0.8316832
#> 5      Green purple vs Purple 0.6237624   1.0000000
#> 6       Light green vs Purple 0.1386139   0.8316832

perm.test.pairwise(x = pdata$LNGS, group = pdata$CUAL, fun = shannon,
                   R = 100)
#>                    Comparison    p.value adj.p.value
#> 1  Dark green vs Green purple 0.40594059   1.0000000
#> 2   Dark green vs Light green 0.01980198   0.1188119
#> 3        Dark green vs Purple 0.06930693   0.4158416
#> 4 Green purple vs Light green 0.08910891   0.5346535
#> 5      Green purple vs Purple 0.14851485   0.8910891
#> 6       Light green vs Purple 0.14851485   0.8910891

perm.test.pairwise(x = pdata$PTLC, group = pdata$CUAL, fun = simpson,
                   R = 100)
#>                    Comparison    p.value adj.p.value
#> 1  Dark green vs Green purple 0.11881188   0.7128713
#> 2   Dark green vs Light green 0.55445545   1.0000000
#> 3        Dark green vs Purple 0.03960396   0.2376238
#> 4 Green purple vs Light green 0.81188119   1.0000000
#> 5      Green purple vs Purple 0.36633663   1.0000000
#> 6       Light green vs Purple 0.37623762   1.0000000
```
