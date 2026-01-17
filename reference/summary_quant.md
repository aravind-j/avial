# Summary Statistics of Qualitative and Quantitative trait data

Summary Statistics of Qualitative and Quantitative trait data

## Usage

``` r
summary_quant(data, group = NULL, trait, out.format = c("long", "wide"))

summary_qual(data, group = NULL, trait, out.format = c("long", "wide"))
```

## Arguments

- data:

  The data as a data frame object. The data frame should possess columns
  specifying the trait and group.

- group:

  Name of column specifying the group as a character string.

- trait:

  Name of column specifying the trait as a character string. The trait
  column should be of type `"numeric"` for quantitative traits and of
  type `"factor"` for qualitative traits.

- out.format:

  The output format.

## Value

A `tibble` data.frame with the summary statistics.

## Examples

``` r
library(agridat)
library(dplyr)

soydata <- australia.soybean

soydata$year <- as.factor(soydata$year)

quant_traits <- c("yield", "height", "lodging",
                  "size", "protein", "oil")
set.seed(123)
soydata <-
  soydata %>%
  mutate(
    across(
      .cols = all_of(quant_traits),
      .fns = ~factor(cut(.x, breaks = quantile(.x, na.rm = TRUE),
                         include.lowest = TRUE),
                     labels = sample(1:9, size = 4)),
      .names = "{.col}_score"
    )
  )

qual_traits <- c("yield_score", "height_score", "lodging_score",
                 "size_score", "protein_score", "oil_score")

summary_quant(data = soydata, trait = quant_traits)
#> # A tibble: 6 × 12
#>   Trait   count miss.count   mean    min   max    sd     se    skew skew.pvalue
#>   <chr>   <int>      <int>  <dbl>  <dbl> <dbl> <dbl>  <dbl>   <dbl>       <dbl>
#> 1 yield     464          0  2.05   0.282  4.38 0.752 0.0349 0.0249     8.24e- 1
#> 2 height    464          0  0.883  0.25   1.73 0.272 0.0126 0.278      1.47e- 2
#> 3 lodging   464          0  2.31   1      4.75 0.976 0.0453 0.414      3.66e- 4
#> 4 size      464          0 11.1    4     23.6  4.45  0.207  0.892      4.29e-12
#> 5 protein   464          0 40.3   33.2   48.5  2.93  0.136  0.280      1.42e- 2
#> 6 oil       464          0 19.9   13.0   26.8  2.67  0.124  0.00142    9.90e- 1
#> # ℹ 2 more variables: kurt <dbl>, kurt.pvalue <dbl>
summary_qual(data = soydata, trait = qual_traits)
#> # A tibble: 6 × 4
#>   Trait         count miss.count freq                              
#>   <chr>         <int>      <int> <chr>                             
#> 1 yield_score     464          0 3 (117), 6 (115), 9 (116), 2 (116)
#> 2 height_score    464          0 2 (116), 6 (116), 3 (117), 5 (115)
#> 3 lodging_score   464          0 4 (143), 6 (114), 8 (107), 1 (100)
#> 4 size_score      464          0 5 (116), 3 (118), 8 (116), 1 (114)
#> 5 protein_score   464          0 9 (116), 1 (120), 5 (114), 3 (114)
#> 6 oil_score       464          0 8 (116), 2 (116), 7 (116), 9 (116)

summary_quant(data = soydata, group = "loc", trait = quant_traits)
#> # A tibble: 24 × 13
#>    loc        Trait   count miss.count   mean    min   max    sd     se    skew
#>    <fct>      <chr>   <int>      <int>  <dbl>  <dbl> <dbl> <dbl>  <dbl>   <dbl>
#>  1 Brookstead yield     116          0  2.01   0.385  3.90 0.825 0.0766  0.108 
#>  2 Brookstead height    116          0  0.983  0.54   1.37 0.185 0.0172 -0.529 
#>  3 Brookstead lodging   116          0  2.52   1      4.5  0.744 0.0691 -0.176 
#>  4 Brookstead size      116          0 11.7    5.4   23.6  4.82  0.448   0.931 
#>  5 Brookstead protein   116          0 40.9   34.2   48.5  2.74  0.254   0.381 
#>  6 Brookstead oil       116          0 19.2   14.7   25.0  2.23  0.207   0.441 
#>  7 Lawes      yield     116          0  2.37   0.586  4.38 0.654 0.0607 -0.129 
#>  8 Lawes      height    116          0  1.02   0.405  1.73 0.324 0.0300  0.219 
#>  9 Lawes      lodging   116          0  2.82   1      4.75 1.11  0.103   0.0210
#> 10 Lawes      size      116          0 11.7    6.55  23.2  4.32  0.401   1.01  
#> # ℹ 14 more rows
#> # ℹ 3 more variables: skew.pvalue <dbl>, kurt <dbl>, kurt.pvalue <dbl>
summary_qual(data = soydata, group = "loc", trait = qual_traits)
#> # A tibble: 24 × 5
#>    loc        Trait         count miss.count freq                          
#>    <fct>      <chr>         <int>      <int> <chr>                         
#>  1 Brookstead yield_score     116          0 3 (35), 6 (23), 9 (31), 2 (27)
#>  2 Brookstead height_score    116          0 2 (12), 6 (17), 3 (39), 5 (48)
#>  3 Brookstead lodging_score   116          0 4 (18), 6 (24), 8 (53), 1 (21)
#>  4 Brookstead size_score      116          0 5 (24), 3 (31), 8 (31), 1 (30)
#>  5 Brookstead protein_score   116          0 9 (19), 1 (31), 5 (33), 3 (33)
#>  6 Brookstead oil_score       116          0 8 (36), 2 (41), 7 (21), 9 (18)
#>  7 Lawes      yield_score     116          0 3 (16), 6 (20), 9 (31), 2 (49)
#>  8 Lawes      height_score    116          0 2 (17), 6 (28), 3 (23), 5 (48)
#>  9 Lawes      lodging_score   116          0 4 (20), 6 (27), 8 (19), 1 (50)
#> 10 Lawes      size_score      116          0 5 (16), 3 (34), 8 (37), 1 (29)
#> # ℹ 14 more rows

summary_quant(data = soydata, group = c("loc", "year"), trait = quant_traits)
#> # A tibble: 48 × 14
#> # Groups:   loc [4]
#>    loc     year  Trait count miss.count   mean    min   max    sd     se    skew
#>    <fct>   <fct> <chr> <int>      <int>  <dbl>  <dbl> <dbl> <dbl>  <dbl>   <dbl>
#>  1 Brooks… 1970  yield    58          0  1.56   0.385  3.16 0.736 0.0967  0.392 
#>  2 Brooks… 1970  heig…    58          0  1.02   0.54   1.32 0.186 0.0244 -0.787 
#>  3 Brooks… 1970  lodg…    58          0  2.35   1      4.5  0.756 0.0993  0.0412
#>  4 Brooks… 1970  size     58          0 10.8    5.4   22.1  4.60  0.604   1.04  
#>  5 Brooks… 1970  prot…    58          0 40.1   34.2   45.8  2.43  0.319   0.299 
#>  6 Brooks… 1970  oil      58          0 19.5   15.7   25.0  2.16  0.284   0.595 
#>  7 Brooks… 1971  yield    58          0  2.46   1.11   3.90 0.649 0.0852  0.481 
#>  8 Brooks… 1971  heig…    58          0  0.949  0.57   1.37 0.179 0.0235 -0.345 
#>  9 Brooks… 1971  lodg…    58          0  2.69   1      4.5  0.699 0.0917 -0.359 
#> 10 Brooks… 1971  size     58          0 12.6    6.85  23.6  4.93  0.647   0.853 
#> # ℹ 38 more rows
#> # ℹ 3 more variables: skew.pvalue <dbl>, kurt <dbl>, kurt.pvalue <dbl>
summary_qual(data = soydata, group = c("loc", "year"), trait = qual_traits)
#> # A tibble: 48 × 6
#> # Groups:   loc [4]
#>    loc        year  Trait         count miss.count freq                         
#>    <fct>      <fct> <chr>         <int>      <int> <chr>                        
#>  1 Brookstead 1970  yield_score      58          0 3 (33), 6 (9), 9 (9), 2 (7)  
#>  2 Brookstead 1970  height_score     58          0 2 (4), 6 (8), 3 (16), 5 (30) 
#>  3 Brookstead 1970  lodging_score    58          0 4 (12), 6 (14), 8 (25), 1 (7)
#>  4 Brookstead 1970  size_score       58          0 5 (18), 3 (14), 8 (12), 1 (1…
#>  5 Brookstead 1970  protein_score    58          0 9 (12), 1 (19), 5 (17), 3 (1…
#>  6 Brookstead 1970  oil_score        58          0 8 (17), 2 (18), 7 (13), 9 (1…
#>  7 Brookstead 1971  yield_score      58          0 3 (2), 6 (14), 9 (22), 2 (20)
#>  8 Brookstead 1971  height_score     58          0 2 (8), 6 (9), 3 (23), 5 (18) 
#>  9 Brookstead 1971  lodging_score    58          0 4 (6), 6 (10), 8 (28), 1 (14)
#> 10 Brookstead 1971  size_score       58          0 5 (6), 3 (17), 8 (19), 1 (16)
#> # ℹ 38 more rows
```
