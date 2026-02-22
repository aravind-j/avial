# Bootstrap Confidence Intervals

This function generates bootstrap resamples using
[`boot`](https://rdrr.io/pkg/boot/man/boot.html) and computes confidence
intervals using several standard bootstrap methods via
[`boot.ci`](https://rdrr.io/pkg/boot/man/boot.ci.html). The indexing for
the statistic function is handled internally.

## Usage

``` r
bootstrap.ci(
  x,
  fun,
  R = 1000,
  conf = 0.95,
  na.omit = TRUE,
  type = c("norm", "basic", "stud", "perc", "bca"),
  parallel = c("no", "multicore", "snow"),
  ncpus = getOption("boot.ncpus", 1L),
  cl = NULL,
  seed = 123,
  ...
)
```

## Arguments

- x:

  A numeric or factor vector of observations.

- fun:

  A function to summarize the observations.

- R:

  Integer specifying the number of permutations. Default is 1000.

- conf:

  Confidence level of the interval. Default is 0.95.

- na.omit:

  logical. If `TRUE`, when `x` is a factor, missing values (`NA`) are
  ignored and not included as a distinct factor level for computation.
  Default is `TRUE`.

- type:

  A vector of character strings representing the type of intervals
  required. The value should be any subset of the values
  `c("norm", "basic", "stud", "perc", "bca")` or simply `"all"` which
  will compute all five types of intervals.

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

## Value

A a named list of confidence intervals, each containing lower and upper
bounds, with additional attributes storing the observed statistic and
the mean of the bootstrap replicates.

## Details

Supported interval types include normal approximation, basic,
studentized (bootstrap-t), percentile, and bias-corrected and
accelerated (BCa) intervals. If a requested interval type cannot be
computed (for example, studentized or BCa intervals), the function falls
back to percentile intervals.

## Examples

``` r
library(EvaluateCore)
#> Registered S3 method overwritten by 'vegan':
#>   method     from      
#>   rev.hclust dendextend
#> 
#> --------------------------------------------------------------------------------
#> Welcome to EvaluateCore version 0.1.4
#> 
#> 
#> # To know whats new in this version type:
#>   news(package='EvaluateCore')
#>   for the NEWS file.
#> 
#> # To cite the methods in the package type:
#>   citation(package='EvaluateCore')
#> 
#> # To suppress this message use:
#>   suppressPackageStartupMessages(library(EvaluateCore))
#> --------------------------------------------------------------------------------

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

# Bootstrap CIs ----

bootstrap.ci(pdata$NMSR, mean, type = "norm")
#> $norm
#>     lower     upper 
#>  9.617438 12.168276 
#> 
#> attr(,"observed")
#> [1] 10.89286
#> attr(,"mean")
#> [1] 10.90556
#> attr(,"fallback")
#> attr(,"fallback")$norm
#> [1] FALSE
#> 
#> attr(,"R")
#> [1] 1000
#> attr(,"conf")
#> [1] 0.95
bootstrap.ci(pdata$NMSR, mean, type = "basic")
#> $basic
#>    lower    upper 
#>  9.57753 12.20149 
#> 
#> attr(,"observed")
#> [1] 10.89286
#> attr(,"mean")
#> [1] 10.90556
#> attr(,"fallback")
#> attr(,"fallback")$basic
#> [1] FALSE
#> 
#> attr(,"R")
#> [1] 1000
#> attr(,"conf")
#> [1] 0.95
bootstrap.ci(pdata$NMSR, mean, type = "perc")
#> $perc
#>     lower     upper 
#>  9.584226 12.208185 
#> 
#> attr(,"observed")
#> [1] 10.89286
#> attr(,"mean")
#> [1] 10.90556
#> attr(,"fallback")
#> attr(,"fallback")$perc
#> [1] FALSE
#> 
#> attr(,"R")
#> [1] 1000
#> attr(,"conf")
#> [1] 0.95
bootstrap.ci(pdata$NMSR, mean, type = "bca")
#> $bca
#>     lower     upper 
#>  9.600968 12.213984 
#> 
#> attr(,"observed")
#> [1] 10.89286
#> attr(,"mean")
#> [1] 10.90556
#> attr(,"fallback")
#> attr(,"fallback")$bca
#> [1] FALSE
#> 
#> attr(,"R")
#> [1] 1000
#> attr(,"conf")
#> [1] 0.95

bootstrap.ci(pdata$NMSR, mean,
             type = c("norm", "basic", "perc", "bca"))
#> $norm
#>     lower     upper 
#>  9.617438 12.168276 
#> 
#> $basic
#>    lower    upper 
#>  9.57753 12.20149 
#> 
#> $perc
#>     lower     upper 
#>  9.584226 12.208185 
#> 
#> $bca
#>     lower     upper 
#>  9.600968 12.213984 
#> 
#> attr(,"observed")
#> [1] 10.89286
#> attr(,"mean")
#> [1] 10.90556
#> attr(,"fallback")
#> attr(,"fallback")$norm
#> [1] FALSE
#> 
#> attr(,"fallback")$basic
#> [1] FALSE
#> 
#> attr(,"fallback")$perc
#> [1] FALSE
#> 
#> attr(,"fallback")$bca
#> [1] FALSE
#> 
#> attr(,"R")
#> [1] 1000
#> attr(,"conf")
#> [1] 0.95

bootstrap.ci(pdata$LNGS, shannon, type = "norm")
#> $norm
#>    lower    upper 
#> 1.364427 1.533204 
#> 
#> attr(,"observed")
#> [1] 1.448816
#> attr(,"mean")
#> [1] 1.44097
#> attr(,"fallback")
#> attr(,"fallback")$norm
#> [1] FALSE
#> 
#> attr(,"R")
#> [1] 1000
#> attr(,"conf")
#> [1] 0.95
bootstrap.ci(pdata$PTLC, simpson, type = "basic")
#> $basic
#>     lower     upper 
#> 0.3508521 0.4809329 
#> 
#> attr(,"observed")
#> [1] 0.4213435
#> attr(,"mean")
#> [1] 0.4230488
#> attr(,"fallback")
#> attr(,"fallback")$basic
#> [1] FALSE
#> 
#> attr(,"R")
#> [1] 1000
#> attr(,"conf")
#> [1] 0.95
bootstrap.ci(pdata$LFRT, mcintosh_evenness, type = "perc")
#> $perc
#>     lower     upper 
#> 0.6289354 0.8262168 
#> 
#> attr(,"observed")
#> [1] 0.693727
#> attr(,"mean")
#> [1] 0.7067876
#> attr(,"fallback")
#> attr(,"fallback")$perc
#> [1] FALSE
#> 
#> attr(,"R")
#> [1] 1000
#> attr(,"conf")
#> [1] 0.95
bootstrap.ci(pdata$LBTEF, mcintosh_diversity, type = "bca")
#> $bca
#>     lower     upper 
#> 0.5861558 0.6150982 
#> 
#> attr(,"observed")
#> [1] 0.5983483
#> attr(,"mean")
#> [1] 0.5923744
#> attr(,"fallback")
#> attr(,"fallback")$bca
#> [1] FALSE
#> 
#> attr(,"R")
#> [1] 1000
#> attr(,"conf")
#> [1] 0.95

bootstrap.ci(pdata$LNGS, shannon,
             type = c("norm", "basic", "perc", "bca"), base = 2)
#> $norm
#>    lower    upper 
#> 1.364427 1.533204 
#> 
#> $basic
#>    lower    upper 
#> 1.380128 1.550485 
#> 
#> $perc
#>    lower    upper 
#> 1.347146 1.517504 
#> 
#> $bca
#>    lower    upper 
#> 1.358274 1.528812 
#> 
#> attr(,"observed")
#> [1] 1.448816
#> attr(,"mean")
#> [1] 1.44097
#> attr(,"fallback")
#> attr(,"fallback")$norm
#> [1] FALSE
#> 
#> attr(,"fallback")$basic
#> [1] FALSE
#> 
#> attr(,"fallback")$perc
#> [1] FALSE
#> 
#> attr(,"fallback")$bca
#> [1] FALSE
#> 
#> attr(,"R")
#> [1] 1000
#> attr(,"conf")
#> [1] 0.95

# Studentised intervals require a `fun` returning
# variances in addition to an estimate

bootstrap.ci(pdata$NMSR, mean, type = "stud")
#> Warning: Studentized CI requires fun() to return c(estimate, SE); using percentile instead.
#> $stud
#>     lower     upper 
#>  9.584226 12.208185 
#> 
#> attr(,"observed")
#> [1] 10.89286
#> attr(,"mean")
#> [1] 10.90556
#> attr(,"fallback")
#> attr(,"fallback")$stud
#> [1] TRUE
#> 
#> attr(,"R")
#> [1] 1000
#> attr(,"conf")
#> [1] 0.95

stat_fun_mean <- function(x) {
  est <- mean(x)
  se  <- sd(x) / sqrt(length(x))
  out <- c(est, se)
  # Important : Tells bootstrap.ci to consider second output as SE
  attr(out, "se") <- TRUE
  return(out)
}

bootstrap.ci(pdata$NMSR, stat_fun_mean, type = "stud")
#> $stud
#>     lower     upper 
#>  9.626269 12.268758 
#> 
#> attr(,"observed")
#> [1] 10.892857  0.631431
#> attr(,"observed")attr(,"se")
#> [1] TRUE
#> attr(,"mean")
#> [1] 10.9055595  0.6259041
#> attr(,"fallback")
#> attr(,"fallback")$stud
#> [1] FALSE FALSE
#> 
#> attr(,"R")
#> [1] 1000
#> attr(,"conf")
#> [1] 0.95

bootstrap.ci(pdata$DSTA, shannon, type = "stud")
#> Warning: Studentized CI requires fun() to return c(estimate, SE); using percentile instead.
#> $stud
#>    lower    upper 
#> 1.762730 2.059521 
#> 
#> attr(,"observed")
#> [1] 1.946525
#> attr(,"mean")
#> [1] 1.92782
#> attr(,"fallback")
#> attr(,"fallback")$stud
#> [1] TRUE
#> 
#> attr(,"R")
#> [1] 1000
#> attr(,"conf")
#> [1] 0.95

stat_fun_shannon <- function(x, base = 2) {
  tab <- tabulate(x)
  p <- tab / length(x)
  # Only keep p > 0 to avoid log(0)
  p <- p[p > 0]
  est <- -sum(p * log(p, base = base))
  # Approximate SE using sqrt(Var(p * log(p)))
  se <- sqrt(sum((p * log(p, base = base))^2) / length(x))
  out <- c(est, se)
  # Important : Tells bootstrap.ci to consider second output as SE
  attr(out, "se") <- TRUE
  return(out)
}

bootstrap.ci(pdata$DSTA, stat_fun_shannon, type = "stud")
#> $stud
#>    lower    upper 
#> 1.835888 2.136488 
#> 
#> attr(,"observed")
#> [1] 1.94652505 0.07067869
#> attr(,"observed")attr(,"se")
#> [1] TRUE
#> attr(,"mean")
#> [1] 1.92781960 0.07034802
#> attr(,"fallback")
#> attr(,"fallback")$stud
#> [1] FALSE FALSE
#> 
#> attr(,"R")
#> [1] 1000
#> attr(,"conf")
#> [1] 0.95
```
