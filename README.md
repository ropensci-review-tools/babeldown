
<!-- README.md is generated from README.Rmd. Please edit that file -->

# babeldown

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/ropensci-review-tools/babeldown/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ropensci-review-tools/babeldown/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of babeldown is to support workflows that include automatic
translation of Markdown-based R content, through DeepL API.

## Installation

You can install the development version of babeldown from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ropensci-review-tools/babeldown")
```

## Examples

### Glossary creation

``` r
filename <- system.file("example-es-en.tsv", package = "babeldown")

# file contents for info
readr::read_tsv(filename, show_col_types = FALSE)
#> # A tibble: 2 × 2
#>   Spanish     English   
#>   <chr>       <chr>     
#> 1 paquete     package   
#> 2 repositorio repository

# create (or update) glossary
babeldown::deepl_upsert_glossary(
  system.file("example-es-en.csv", package = "babeldown"),
  glossary_name = "rstats-glosario",
  target_lang = "Spanish",
  source_lang = "English"
)
#> Updating glossary rstats-glosario
```
