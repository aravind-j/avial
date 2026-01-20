# Diversity Index Functions

To be used by
[`diversity.calc`](https://aravind-j.github.io/avial/reference/diversity.calc.md).

## Usage

``` r
berger_parker(x)

berger_parker_reciprocal(x)

simpson(x)

gini_simpson(x)

simpson_max(x)

simpson_reciprocal(x)

simpson_relative(x)

simpson_evenness(x)

shannon(x, base = 2)

shannon_max(x, base = 2)

shannon_relative(x, base = 2)

shannon_ens(x, base = 2)

mcintosh_diversity(x)

mcintosh_evenness(x)

smith_wilson(x)

heip_evenness(x)

margalef_index(x)

menhinick_index(x)

brillouin_index(x)

hill_number(x, q = 1)

renyi_entropy(x, q = 1)

tsallis_entropy(x, q = 1)

hill_evenness(x, q = 1)
```

## Arguments

- x:

  A factor vector of categories (e.g., species, traits). The frequency
  of each level is treated as the abundance of that category.

- base:

  The logarithm base to be used for computation of shannon family of
  diversity indices. Default is `exp(1)`.

## Value

The calculated diversity index value.
