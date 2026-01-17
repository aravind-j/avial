# Permutation tests

These functions perform permutation-based hypothesis tests to compare
groups with respect to a summary statistic (e.g., mean, diversity
index).

## Usage

``` r
perm.test.global(x, group, fun, R = 1000, ...)

perm.test.pairwise(
  x,
  group,
  fun,
  R = 1000,
  p.adjust.method = c("bonferroni", "holm"),
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

- ...:

  Additional arguments passed to `fun`.

- p.adjust.method:

  (perm.test.pairwise only) Method for adjusting p-values for multiple
  comparisons. Options include `"bonferroni"` and `"holm"`. Default is
  `"bonferroni"`.

## Details

- `perm.test.global` performs a global test across all groups
  simultaneously, using a weighted sum of squares between group summary
  indices as the test statistic

- `perm.test.pairwise` performs pairwise tests between all combinations
  of groups, comparing the absolute difference in summary statistics and
  optionally adjusting p-values for multiple comparisons.
