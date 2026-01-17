# Calculate the Bin Width for Plotting Histograms

Calculate the Bin Width for Plotting Histograms

## Usage

``` r
binw(x, method = c("fd", "scott", "sturges"))
```

## Arguments

- x:

  A numeric vector of the values from which histogram has to be
  generated.

- method:

  The method to compute the number of classes for the histogram.

## Value

The bin width.

## See also

`nclass`

## Examples

``` r
set.seed(1)
x <- stats::rnorm(1111)

binw(x = x, method = "fd")
#> [1] 0.2
binw(x = x, method = "scott")
#> [1] 0.5
binw(x = x, method = "sturges")
#> [1] 0.5
```
