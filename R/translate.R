#' Translate a Markdown file
#'
#' @param path Path to the Markdown file to be translated (character).
#' @param out_path Path where the new translated file should be saved (character).
#' @param glossary_name Name of the glossary to be used, if any (character).
#' @inheritParams deepl_upsert_glossary
#' @param formality Formality level to use (character), one of
#' * "default" (default)
#' * "more" -- for a more formal language
#' * "less" -- for a more informal language
#' * "prefer_more" -- for a more formal language if available, otherwise fallback to default formality
#' * "prefer_less" -- for a more informal language if available, otherwise fallback to default formality
#'
#' @return None
#' @export
#'
#' @examples
deepl_translate <- function(path,
                            out_path,
                            glossary_name = NULL,
                            source_lang = NULL,
                            target_lang = NULL,
                            formality = c("default", "more", "less", "prefer_more", "prefer_less")) {

  # check arguments ----

  if (!file.exists(path)) {
    cli::cli_abort("Can't find {.field path} {.val {path}}.")
  }

  if (!is.null(glossary_name)) {
    glossary_id <- get_glossary_id(glossary_name)
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

  # create tinkr object for splitting ----
  wool <- tinkr::yarn$new(path = path)
  children_pods <- split(
    xml2::xml_children(wool[["body"]]),
    ceiling(seq_along(xml2::xml_children(wool[["body"]]))/50)
  )

  translated_children_pods <- purrr::map(
    children_pods,
    translate_part,
    glossary_id = glossary_id,
    source_lang = source_lang,
    target_lang = target_lang,
    formality = formality
  )

  temp_xml <- xml2::xml_new_document()

  add_nodes <- function(xml_document, temp_xml) {
    purrr::walk(
      xml2::xml_children(xml_document),
      function(x, temp_xml) xml2::xml_add_child(temp_xml, x),
      temp_xml = temp_xml
    )
  }
  purrr::walk(translated_children_pods, add_nodes, temp_xml = temp_xml)
  wool[["body"]] <- temp_xml
  wool$write(out_path)

}

translate_part <- function(xml, glossary_id, source_lang, target_lang, formality) {
  temp_file <- withr::local_tempfile()
  file.create(temp_file)
  woolish <- tinkr::yarn$new(path = temp_file)

  temp_xml <- xml2::xml_new_document()
  purrr::walk(
    xml,
    function(x, temp_xml) xml2::xml_add_child(temp_xml, x),
    temp_xml = temp_xml
  )

  woolish$body <- temp_xml

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
        ignore_tags = "code,curly"
      ) |>
      purrr::compact()

    doc <- deepl_form_request("v2/translate", !!!body_params)
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
