#' Translate a VTT subtitles file
#'
#' @inheritParams deepl_translate
#' @inheritParams deepl_upsert_glossary
#'
#' @return None
#' @export
#'
#' @examples
deepl_translate_vtt <- function(path,
                            out_path,
                            glossary_name = NULL,
                            source_lang = NULL,
                            target_lang = NULL,
                            formality = c("default", "more", "less", "prefer_more", "prefer_less")) {

  # check arguments ----

  if (!file.exists(path)) {
    cli::cli_abort("Can't find {.field path} {.val {path}}.")
  }

  source_lang_code <- examine_source_lang(source_lang)
  target_lang_code <- examine_target_lang(target_lang)

  if (!is.null(glossary_name)) {
    glossary_id <- get_glossary_id(
      glossary_name,
      source_lang = source_lang_code,
      target_lang = target_lang_code
    )
    if (is.null(glossary_id)) {
      cli::cli_abort(
        c(
          "Can't find {.field glossary_name} {.val {glossary_name}}.",
          i = "Check the spelling, or create it with {.fun upsert_glossary}."
        )
      )
    }
  } else {
    glossary_id <- NULL
  }

  formality <- rlang::arg_match(
    formality,
    values = c("default", "more", "less", "prefer_more", "prefer_less")
  )

  # translate content ----
browser()
  lines <- brio::read_lines(path)
  first_text_index <- which(lines == "0") + 2
  text_indices <- seq(from = first_text_index, by = 4, to = length(lines))
  text <- sprintf("<text ind='%s'>%s</text>", text_indices, lines[text_indices]) |>
    paste(collapse = "")
  body_params <- list(
    text = text,
    source_lang = source_lang,
    target_lang = target_lang,
    tag_handling = "xml",
    non_splitting_tags = "text,softbreak",
    formality = formality,
    glossary_id = glossary_id,
    outline_detection = 0,
    ignore_tags = "code,code_block,curly"
  ) |>
    purrr::compact()

  doc <- deepl_form_request("translate", !!!body_params)
  new_text <- doc$translations[[1]]$text


  brio::write_lines(lines, out_path)

}

translate_part <- function(xml, glossary_id, source_lang, target_lang, formality) {
  temp_file <- withr::local_tempfile()
  file.create(temp_file)
  woolish <- tinkr::yarn$new(path = temp_file)
  woolish$body <- fakify_xml(xml)

  ## protect content inside curly braces ----
  woolish$body <- tinkr::protect_curly(woolish$body)
  curlies <- xml2::xml_find_all(woolish$body, "//*[@curly]")
  replace_curly <- function(curly) {
    xml2::xml_name(curly) <- "curly"
  }
  purrr::walk(curlies, replace_curly)

  ## translate ----
  .translate <- function(text, glossary_id, source_lang, target_lang, formality) {

    body_params <- list(
        text = text,
        source_lang = source_lang,
        target_lang = target_lang,
        tag_handling = "xml",
        non_splitting_tags = "text,softbreak",
        formality = formality,
        glossary_id = glossary_id,
        ignore_tags = "code,code_block,curly"
      ) |>
      purrr::compact()

    doc <- deepl_form_request("translate", !!!body_params)
    doc$translations[[1]]$text
  }
  translate <- memoise::memoise(.translate)

  woolish$body <- xml2::read_xml(
    translate(
      as.character(woolish$body),
      source_lang = source_lang,
      target_lang = target_lang,
      formality = formality,
      glossary_id = glossary_id
    )
  )

  ## Make curly tags text tags again ----
  curlies <- xml2::xml_find_all(woolish$body, "//*[@curly]")
  replace_curly <- function(curly) {
    xml2::xml_name(curly) <- "text"
  }
  purrr::walk(curlies, replace_curly)

  woolish[["body"]]
}
