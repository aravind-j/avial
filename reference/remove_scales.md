# Remove scales from `ggplot` objects

Useful for avoiding the warning
`Scale for * is already present. Adding another scale for *, which will replace the existing scale.`

## Usage

``` r
remove_scales(g, scales)
```

## Arguments

- g:

  A `ggplot` object.

- scales:

  The scales to be removed as a character vector.

## Value

A `ggplot` object without the scales specified.
