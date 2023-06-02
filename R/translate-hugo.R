#' Translate a Hugo post
#'
#' @description
#' This assumes the Hugo website uses
#' - leaf bundles (each post in a folder, `leaf-bundle/index.md`);
#' - multilingualism so that a post in say Spanish lives in `leaf-bundle/index.es.md`.
#'
#'
#' @param post_path Path to post. If RStudio IDE is installed, it will default to the
#' currently open document.
#' If you use Quarto or R Markdown for Hugo, translate the source file (qmd or Rmd)
#' and then render it to Markdown.
#' @param force Whether to overwrite the post in the target language.
#' @inheritParams deepl_translate
#'
#' @return None
#' @export
#'
#' @examples
#' \dontrun{
#' dir <- withr::local_tempdir()
#' blogdown::new_site(dir = dir)
#' deepl_translate_hugo(
#'   file.path(dir, "content", "post", "2016-12-30-hello-markdown", "index.md"),
#'   source_lang = "en",
#'   target_lang = "fr",
#'   formality = "less"
#')
#' readLines(file.path(dir, "content", "post", "2016-12-30-hello-markdown", "index.fr.md"))
#' }
deepl_translate_hugo <- function(post_path = NULL,
                                 force = FALSE,
                                 yaml_fields = c("title", "description"),
                                 glossary_name = NULL,
                                 source_lang = NULL,
                                 target_lang = NULL,
                                 formality = c("default", "more", "less", "prefer_more", "prefer_less")) {

  if (is.null(post_path)) {
    if (!rstudioapi::isAvailable()) {
      cli::cli_abort("Must provide a {.code post_path}.")
    } else {
      post_path <- rstudioapi::documentPath(rstudioapi::documentId(allowConsole = FALSE))
    }
  }

  target_lang_code <- examine_target_lang(target_lang)

  post_ext <- fs::path_ext(post_path)
  post_dir <- fs::path_dir(post_path)
  post_file <- fs::path_file(post_path)
  target_post_path <- file.path(
    post_dir,
    sprintf(
      "%s.%s.%s",
      fs::path_ext_remove(post_file),
      tolower(target_lang_code),
      post_ext
    )
  )

  if (!force && fs::file_exists(target_post_path)) {
    cli::cli_abort(c(
      "Can't create {target_post_path} as it already exists",
      i = "Delete it or use {.code force=TRUE}."
      )
      )
  }

  deepl_translate(
    path = post_path,
    out_path = target_post_path,
    yaml_fields = yaml_fields,
    glossary_name = glossary_name,
    source_lang = source_lang,
    target_lang = target_lang,
    formality = formality
  )

  # translate slug
  if ("title" %in% yaml_fields) {
    wool <- tinkr::yarn$new(target_post_path)
    yaml <- yaml::yaml.load(wool$yaml)
    if (!is.null(yaml[["title"]])) {
      yaml[["slug"]] <- snakecase::to_snake_case(yaml[["title"]])
    }
    yaml_file <- withr::local_tempfile()
    yaml::write_yaml(yaml, yaml_file)
    wool$yaml <- c("---", brio::read_lines(yaml_file), "---")

    wool$write(target_post_path)
  }

}
