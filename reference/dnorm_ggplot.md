# Function to Add Normal Distribution Curve in [`ggplot2`](https://ggplot2.tidyverse.org/reference/ggplot2-package.html) histogram

Enhancement of [`dnorm`](https://rdrr.io/r/stats/Normal.html) to plot
normal distribution or bell curve as an overlay in
[`ggplot2`](https://ggplot2.tidyverse.org/reference/ggplot2-package.html)
histograms according to number of records and bin width.

## Usage

``` r
dnorm_ggplot(x, mean, sd, n, bw)
```

## Arguments

- x:

  vector of values.

- mean:

  vector of means.

- sd:

  vector of standard deviations.

- n:

  The number of records or data points used to plot the histogram.

- bw:

  The bin width of the histogram.

## Value

The density for normal distribution.

## See also

[`geom_histogram`](https://ggplot2.tidyverse.org/reference/geom_histogram.html),
[`groupwise_histogram`](https://aravind-j.github.io/avial/reference/groupwise_histogram.md)

## Examples

``` r
dnorm(0) * 25 == dnorm_ggplot(0, mean = 0, sd = 1, n = 5, bw = 5)
#> [1] TRUE
dnorm(1) * 21 == dnorm_ggplot(1, mean = 0, sd = 1, n = 7, bw = 3)
#> [1] TRUE
```
