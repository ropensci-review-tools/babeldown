#' Translate a Quarto book chapter
#'
#' @description
#' This assumes the book is set up à la [babelquarto](https://github.com/ropensci-review-tools/babelquarto).
#'
#'
#' @param book_path Path to book source
#' @param chapter Chapter name
#' @param force Whether to overwrite the chapter in the target language.
#' @param render Whether to run `babelquarto::render_bool()` after translation.
#' @inheritParams deepl_translate
#'
#' @return None
#' @export
#'
#' @examples
#' \dontrun{
#' temp_dir <- withr::local_tempdir()
#' babelquarto::quarto_multilingual_book(
#'   parent_dir = temp_dir,
#'   book_dir = "blop",
#'   main_language = "en",
#'   further_languages = "es"
#' )
#' book_path <- file.path(temp_dir, "blop")
#' deepl_translate_quarto(
#'   book_path = book_path,
#'   chapter = "intro.qmd",
#'   force = TRUE, # the existing chapter is a placeholder
#'   render = TRUE,
#'   source_lang = "EN",
#'   target_lang = "ES",
#'   formality = "less"
#' )
#' # have a look at the translation
#' readLines(file.path(book_path, "intro.es.qmd"))
#' # servr::httw(file.path(book_path, "_book"))
#' }
deepl_translate_quarto <- function(book_path,
                                   chapter,
                                   force = FALSE,
                                   render = TRUE,
                                   yaml_fields = c("title", "description"),
                                   glossary_name = NULL,
                                   source_lang = NULL,
                                   target_lang = NULL,
                                   formality = c("default", "more", "less", "prefer_more", "prefer_less")) {
  target_lang_code <- examine_target_lang(target_lang)

  chapter_path <- file.path(book_path, chapter)
  chapter_path_ext <- fs::path_ext(chapter_path)
  target_chapter_path <- file.path(
    book_path,
    sprintf(
      "%s.%s.%s",
      fs::path_ext_remove(fs::path_file(chapter_path)),
      tolower(target_lang_code),
      chapter_path_ext
    )
  )

  if (!force && fs::file_exists(target_chapter_path)) {
    cli::cli_abort(c(
      "Can't create {target_chapter_path} as it already exists",
      i = "Delete it or use {.code force=TRUE}."
    ))
  }

  deepl_translate(
    path = chapter_path,
    out_path = target_chapter_path,
    yaml_fields = yaml_fields,
    glossary_name = glossary_name,
    source_lang = source_lang,
    target_lang = target_lang,
    formality = formality
  )

  if (render) {
    babelquarto::render_book(book_path = book_path)
  }
}
