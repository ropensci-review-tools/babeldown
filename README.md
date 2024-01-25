
<!-- README.md is generated from README.Rmd. Please edit that file -->

# babeldown

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/ropensci-review-tools/babeldown/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ropensci-review-tools/babeldown/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of babeldown is to support workflows that include automatic
translation of Markdown-based R content, through [DeepL
API](https://www.deepl.com/en/docs-api/). With babeldown you can
translate: Markdown strings
(`babeldown::deepl_translate_markdown_string()`) and Markdown files
(`babeldown::deepl_translate()`) in general; Quarto book chapters
(`babeldown::deepl_translate_quarto()`) and Hugo blog posts
(`babeldown::deepl_translate_hugo()`) in particular.

## Installation and setup

You can install the development version of babeldown from [rOpenSci
R-universe](https://ropensci.r-universe.dev/):

``` r
install.packages('babeldown', repos = c('https://ropensci.r-universe.dev', 'https://cloud.r-project.org'))
```

Or from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("ropensci-review-tools/babeldown")
```

### API URL

The DeepL API URL depends on your API plan. babeldown uses the DeepL
*free* API URL by default. If you use a Pro plan, set the API URL via

``` r
Sys.setenv("DEEPL_API_URL" = "https://api.deepl.com")
```

### API key

Set your API key via the environment variable `DEEPL_API_KEY`. You could
store it with the [keyring](https://r-lib.github.io/keyring/index.html)
package and retrieve it like so:

``` r
Sys.setenv(DEEPL_API_KEY = keyring::key_get("deepl"))
```

### Line wrapping

We recommend to start a new line in your Markdown document after each
sentence or sentence part (after a comma for instance), for more
informative Git diffs.

We also recommend not starting a new line in the middle of something
that’s between square brackets (in particular references and in-line
footnotes) as it would break the way babeldown tries to protect those.

#### RStudio Visual Editor

If you use RStudio Visual Editor, you can choose [“sentence” as
line-wrapping
option](https://rstudio.github.io/visual-markdown-editing/markdown.html#line-wrapping).

## Troubleshooting

Getting an HTTP error 456 means you’ve used up all your API credits. Use
`deepl_usage()` (or the online DeepL API interface) to get your usage
data.

The DeepL API might mix up some punctuation, for instance:

``` r
babeldown::deepl_translate_markdown_string(
  "[So _incredibly_ **wonderful**](https://ropensci.org)!",
  source_lang = "EN",
  target_lang = "ES"
)
#> [1] "[Así que *increíblemente* **maravilloso**](https://ropensci.org) ¡!"
babeldown::deepl_translate_markdown_string(
  "[So _incredibly_ **wonderful**!](https://ropensci.org)",
  source_lang = "EN",
  target_lang = "ES"
)
#> [1] "[Así que *increíblemente* **maravilloso** ¡!](https://ropensci.org)"
babeldown::deepl_translate_markdown_string(
  "[So _incredibly_ **wonderful!**](https://ropensci.org)",
  source_lang = "EN",
  target_lang = "ES"
)
#> [1] "[Así que *increíblemente* **¡maravilloso!**](https://ropensci.org)"
```

## Examples

### Markdown string translation

``` r
babeldown::deepl_translate_markdown_string(
  "[So _incredibly_ **wonderful**](https://ropensci.org)",
  source_lang = "EN",
  target_lang = "ES"
)
#> [1] "[Así que *increíblemente* **maravilloso**](https://ropensci.org)"
```

### File translation

``` r
english_lines <- c(
  "## A cool section", "",
  "This is the first paragraph. `system.file()` is cool, right?", "",
  "Another paragraph. I really enjoy developing R packages.", "",
  "Do you enjoy debugging?"
)
file <- withr::local_tempfile()
brio::write_lines(english_lines, file)

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

You can also indicate YAML fields to translate, using the `yaml_fields`
argument. By default, `title` and `description` are translated.

``` r
english_lines <- c(
  "---",
  "title: Nice document",
  "description: An example for sure",
  "---",
  "## A cool section", "",
  "This is the first paragraph. `system.file()` is cool, right?", "",
  "Another paragraph. I really enjoy developing R packages.", "",
  "Do you enjoy debugging?"
)
file <- withr::local_tempfile()
brio::write_lines(english_lines, file)

out_path <- withr::local_tempfile()

babeldown::deepl_translate(
  path = file,
  out_path = out_path,
  source_lang = "EN",
  target_lang = "ES",
  formality = "less"
)

readLines(out_path)
#>  [1] "---"                                                         
#>  [2] "title: Buen documento"                                       
#>  [3] "description: Un ejemplo seguro"                              
#>  [4] "---"                                                         
#>  [5] ""                                                            
#>  [6] "## Una sección genial"                                       
#>  [7] ""                                                            
#>  [8] "Este es el primer párrafo. `system.file()` es guay, ¿verdad?"
#>  [9] ""                                                            
#> [10] "Otro párrafo. Me gusta mucho desarrollar paquetes de R."     
#> [11] ""                                                            
#> [12] "¿Disfrutas depurando?"                                       
#> [13] ""                                                            
#> [14] ""
```

### Glossary creation or update

``` r
filename <- system.file("example-es-en.csv", package = "babeldown")

# file contents for info
readr::read_csv(filename, show_col_types = FALSE)
#> # A tibble: 2 × 2
#>   Spanish     English   
#>   <chr>       <chr>     
#> 1 paquete     package   
#> 2 repositorio repository

# create (or update) glossary
babeldown::deepl_upsert_glossary(
  filename,
  glossary_name = "rstats-glosario",
  target_lang = "Spanish",
  source_lang = "English"
)
#> Updating glossary rstats-glosario
```

### File translation with glossary

``` r
file <- withr::local_tempfile()
brio::write_lines(english_lines, file)

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
#>  [1] "---"                                                         
#>  [2] "title: Buen documento"                                       
#>  [3] "description: Un ejemplo seguro"                              
#>  [4] "---"                                                         
#>  [5] ""                                                            
#>  [6] "## Una sección genial"                                       
#>  [7] ""                                                            
#>  [8] "Este es el primer párrafo. `system.file()` es guay, ¿verdad?"
#>  [9] ""                                                            
#> [10] "Otro párrafo. Me gusta mucho desarrollar paquetes de R."     
#> [11] ""                                                            
#> [12] "¿Disfrutas depurando?"                                       
#> [13] ""                                                            
#> [14] ""
```

### Quarto book chapters

You can use babeldown to translate chapters of a Quarto multilingual
book set up with babelquarto. See the article describing [the
workflow](https://docs.ropensci.org/babeldown/articles/quarto.html)

### Hugo posts

You can also use babeldown to translate blog posts of a Hugo
multilingual website using leaf bundles (posts as `index.md` inside a
content subfolder) and saving translations along each other (for
instance Spanish post as `index.es.md` inside the same subfolder). See
`babeldown::deepl_translate_hugo()`.

If your Hugo website is set up differently, either open an issue or use
`babeldown::deepl_translate()`.

#### Hugo shortcodes

Hugo shortcodes are supported but not very flexibly: you need to use
`param="value"` with no space, and double quotes.
