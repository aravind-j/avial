# Bartlett's Test of Homogeneity of Error Variances

Perform chi-square test for homogeneity of variance (Bartlett's test) to
test equality of several error variances or mean squared errors (Gomez
and Gomez 1984) .

## Usage

``` r
mse.bartlett.test(mse, df)
```

## Arguments

- mse:

  A vector of error variances or mean squared errors from each
  environment (years and/or locations).

- df:

  A vector of degrees of freedom corresponding to `mse`.

## Value

A list with the chi-square value test statistic, corresponding degrees
of freedom and p value.

## References

Gomez KA, Gomez AA (1984). *Statistical Procedures for Agricultural
Research*, 2nd ed edition. Wiley, New York. ISBN 978-0-471-87092-0.

## See also

[`bartlett.test`](https://rdrr.io/r/stats/bartlett.test.html)

## Examples

``` r
# Examples from Page 467-471 Gomez KA and AA Gomez (1984) Statistical
# Procedures for Agricultural Research. 2nd ed. Wiley, New York, 680 p.

# Different degrees of freedom
mse <- c(6.73920, 1.93496, 1.15500, 10.58450)
df <- c(19, 16, 17, 19)

mse.bartlett.test(mse = c(6.73920, 1.93496, 1.15500, 10.58450),
                  df = c(19, 16, 17, 19))
#> $chisq.value
#> [1] 24.98754
#> 
#> $df
#> [1] 3
#> 
#> $p.value
#> [1] 1.553341e-05
#> 

# Same degrees of freedom
mse <- c(11.459848, 17.696970, 10.106818)
df <- c(20, 20, 20)

mse.bartlett.test(mse = c(11.459848, 17.696970, 10.106818),
                  df = c(20, 20, 20))
#> $chisq.value
#> [1] 1.814362
#> 
#> $df
#> [1] 2
#> 
#> $p.value
#> [1] 0.4036606
#> 
```
