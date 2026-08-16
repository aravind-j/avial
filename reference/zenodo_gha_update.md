# Install a GitHub Actions workflow to create a `.zenodo.json` file

This function installs a [GitHub
Actions](https://github.com/features/actions) workflow in your
repository. The workflow updates your
[`.zenodo.json`](https://help.zenodo.org/docs/github/describe-software/zenodo-json/)
file from the package `DESCRIPTION` when any of these events occur:

- You publish a new release of the package.

- Your `DESCRIPTION` or `inst/CITATION` file is modified.

- The action can be run manually.

## Usage

``` r
zenodo_gha_update(path = ".", overwrite = FALSE)
```

## Arguments

- path:

  Project root directory.

- overwrite:

  A logical value. If `TRUE`, overwrite an existing workflow.

## Details

Workflow triggers can be modified. See [Events that trigger
workflows](https://docs.github.com/en/actions/learn-github-actions/events-that-trigger-workflows).

## Examples

``` r
if (FALSE) { # \dontrun{
zenodo_gha_update()
} # }
```
