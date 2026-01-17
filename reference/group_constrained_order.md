# Ordering by keeping defined groups together

This function modifies the order of a vector so that specified groups of
elements remain together. This is useful for keeping specific dendrogram
leaves together. The location of each group block is determined by the
earliest member of that group in original vector. For elements not
involved in any group, their relative order is unchanged.

## Usage

``` r
group_constrained_order(x, groups, group_order = c("groups", "original"))
```

## Arguments

- x:

  The original vector of elements in order.

- groups:

  The groups of elements that are to be kept together as a list of
  vectors. The groups must be disjoint.

- group_order:

  The group internal order should follow the one in `"groups"` or the
  `"original"` order.

## Value

A vector of the original elements where the specified groups of elements
are kept together.

## Examples

``` r
set.seed(123)

# data
mat <- matrix(rnorm(8 * 4), nrow = 8)
rownames(mat) <- LETTERS[1:8]

# hierarchical clustering
hc <- hclust(dist(mat))
plot(hc, main = "Original dendrogram")


# groups of leaves to keep together (may not be adjacent in dendrogram)
groups <- list(
  g1 = c("B", "F"),
  g2 = c("C", "D", "E")
)

# Dendrogram original order
orig_order <- hc$labels[hc$order]
orig_order
#> [1] "C" "F" "H" "B" "E" "A" "D" "G"

# Get group constrained order
new_order <-
  group_constrained_order(x = orig_order,
                          groups = groups,
                          group_order = "original")

new_order
#> [1] "C" "E" "D" "F" "B" "H" "A" "G"

# Reorder dendrogram labels
hc2 <- hc
hc2$order <- match(new_order, hc$labels)

plot(hc2, main = "Dendrogram with grouped leaves")

```
