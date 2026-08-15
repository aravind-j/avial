# Generate Zenodo metadata

Generate a `.zenodo.json` file from either an R package DESCRIPTION file
or a Citation File Format (CFF) file.

## Usage

``` r
generate_zenodo_json(
  description_file = "DESCRIPTION",
  cff_file = "CITATION.cff",
  output_file = ".zenodo.json",
  from = c("description", "cff"),
  github_topics = TRUE
)
```

## Arguments

- description_file:

  Path to the `DESCRIPTION` file.

- cff_file:

  Path to the `CITATION.cff` file.

- output_file:

  Path to the generated Zenodo JSON file.

- from:

  Source of metadata: either `"description"` or `"cff"`.

- github_topics:

  Logical; if `TRUE`, attempt to retrieve GitHub repository topics and
  add them as keywords.

## Value

Invisibly returns the generated Zenodo metadata as a list.
