# Custom tidyverse style guide

Creates a modified version of \[styler::tidyverse_style()\] with custom
line-breaking and argument-packing rules.

## Usage

``` r
avial_custom_style()
```

## Value

A styler style-guide object, i.e. a named list of transformer functions
suitable for use with \[styler::style_text()\],
\[styler::style_file()\], \[styler::style_dir()\], and related styler
functions.

## Details

This style guide retains the standard tidyverse spacing, indentation,
token, and line-breaking conventions except for the following
modifications:

- Function arguments and vector elements are packed onto lines subject
  to an 80-character width limit.

- Function calls do not introduce a line break immediately after the
  opening parenthesis or immediately before the closing parenthesis.

- Arguments that would otherwise be formatted one per line are grouped
  onto the same line when they fit within the width limit.

- The right-hand side of an assignment is moved to a new line when it is
  a function call.

- The base \`function()\` syntax is retained; the shorthand lambda
  syntax is not introduced.

The resulting style guide can be supplied to styler functions through
the \`transformers\` argument or configured for the styler RStudio
Addin.

## Examples

``` r
if (requireNamespace("styler", quietly = TRUE)) {
  style <- avial_custom_style()

  styler::style_text(
    "result <- some_function(first_argument = value_one,
                             second_argument = value_two)",
    transformers = style
  )
}
#> result <- some_function(first_argument = value_one, second_argument = value_two)
```
