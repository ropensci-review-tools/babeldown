
<!-- README.md is generated from README.Rmd. Please edit that file -->

# babeldown

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/ropensci-review-tools/babeldown/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ropensci-review-tools/babeldown/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of babeldown is to support workflows that include automatic
translation of Markdown-based R content, through DeepL API.

## Installation and setup

You can install the development version of babeldown from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ropensci-review-tools/babeldown")
```

### API URL

The DeepL API URL depends on your API plan. babeldown uses the DeepL
*free* API URL by default. If you use a Pro plan, set the API URL via

``` r
Sys.setenv("DEEPL_API_URL" = "https://api.deepl.com")
```

### API key

Set your API key via the environment variable `DEEPL_API_KEY`. You could
store it with the keyring package and retrieve it like so:

``` r
Sys.setenv(DEEPL_API_KEY = keyring::key_get("deepl"))
```

## Examples

### File translation

``` r
english_lines <- c(
  "## A cool section", "",
  "This is the first paragraph. `system.file()` is cool, right?", "",
  "Another paragraph. I really enjoy developing R packages.", "",
  "Do you enjoy debugging?"
)
file <- withr::local_tempfile()
writeLines(english_lines, file)

out_path <- withr::local_tempfile()

babeldown::deepl_translate(
  path = file,
  out_path = out_path,
  source_lang = "EN",
  target_lang = "ES",
  formality = "less"
)

readLines(out_path)
#> [1] "## Una sección genial"                                       
#> [2] ""                                                            
#> [3] "Este es el primer párrafo. `system.file()` es guay, ¿verdad?"
#> [4] ""                                                            
#> [5] "Otro párrafo. Me gusta mucho desarrollar paquetes de R."     
#> [6] ""                                                            
#> [7] "¿Disfrutas depurando?"                                       
#> [8] ""                                                            
#> [9] ""
```

### Glossary creation

``` r
filename <- system.file("example-es-en.csv", package = "babeldown")

# file contents for info
readr::read_tsv(filename, show_col_types = FALSE)
#> # A tibble: 2 × 1
#>   `Spanish,English`     
#>   <chr>                 
#> 1 paquete,package       
#> 2 repositorio,repository

# create (or update) glossary
babeldown::deepl_upsert_glossary(
  filename,
  glossary_name = "rstats-glosario",
  target_lang = "Spanish",
  source_lang = "English"
)
#> Creating glossary rstats-glosario
```

### File translation with glossary

``` r
file <- withr::local_tempfile()
writeLines(english_lines, file)

out_path <- withr::local_tempfile()

babeldown::deepl_translate(
  path = file,
  out_path = out_path,
  source_lang = "EN",
  target_lang = "ES",
  formality = "less",
  glossary = "rstats-glosario"
)

readLines(out_path)
#> [1] "## Una sección genial"                                       
#> [2] ""                                                            
#> [3] "Este es el primer párrafo. `system.file()` es guay, ¿verdad?"
#> [4] ""                                                            
#> [5] "Otro párrafo. Me gusta mucho desarrollar paquetes de R."     
#> [6] ""                                                            
#> [7] "¿Disfrutas depurando?"                                       
#> [8] ""                                                            
#> [9] ""
```
