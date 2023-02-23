#' Translate a Markdown file
#'
#' @param path Path to the Markdown file to be translated (character).
#' @param out_path Path where the new translated file should be saved (character).
#' @param glossary_name Name of the glossary to be used, if any (character).
#' @param yaml_fields Vector of character names of YAML fields to translate.
#' @inheritParams deepl_upsert_glossary
#' @param formality Formality level to use (character), one of
#' * "default" (default)
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
                            yaml_fields = c("title", "description"),
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

  # protect Hugo shortcodes ----
  temp_markdown_file <- withr::local_tempfile()
  markdown_lines <- brio::read_lines(path)
  shortcodes_no <- which(grepl("\\{\\{<", markdown_lines))
  shortcodes_present <- length(shortcodes_no > 0)
  if (shortcodes_present) {
    # FIXME add translation
    shortcodes <- markdown_lines[shortcodes_no]
    markdown_lines[shortcodes_no] <- sprintf("`%s`", purrr::map_chr(shortcodes, digest::digest))
    brio::write_lines(markdown_lines, temp_markdown_file)
    wool <- tinkr::yarn$new(path = temp_markdown_file)
  } else {
    wool <- tinkr::yarn$new(path = path)
  }

  # translate some YAML fields ----

  yaml <- yaml::yaml.load(wool$yaml)
  if (!is.null(yaml_fields) && !is.null(yaml)) {

  for (yaml_field in yaml_fields) {
    if (is_non_empty_string(yaml[[yaml_field]])) {
      yaml[[yaml_field]] <- deepl_translate_markdown_string(
        yaml[[yaml_field]],
        glossary_name = glossary_name,
        source_lang = source_lang,
        target_lang = target_lang,
        formality = formality
      )
    }
  }
  yaml_file <- withr::local_tempfile()
  yaml::write_yaml(yaml, yaml_file)
  wool$yaml <- c("---", brio::read_lines(yaml_file), "---")
  }

  # translate content ----
  # the API doesn't document what is too much
  # so this is more or less a random number :-)
  split_size <- 10

  children_pods <- split(
    xml2::xml_children(wool[["body"]]),
    ceiling(seq_along(xml2::xml_children(wool[["body"]]))/split_size)
  )

  translated_children_pods <- purrr::map(
    children_pods,
    translate_part,
    glossary_id = glossary_id,
    source_lang = source_lang,
    target_lang = target_lang,
    formality = formality
  )

  # write back, unprotect Hugo shortcodes ----
  wool[["body"]] <- fakify_xml(translated_children_pods)

  temp_markdown_file <- withr::local_tempfile()
  wool$write(temp_markdown_file)
  markdown_lines <- brio::read_lines(temp_markdown_file)
  if (shortcodes_present) {
    for (shortcode in shortcodes) {
        digested_shortcode <- sprintf("`%s`", digest::digest(shortcode))
        markdown_lines[markdown_lines == digested_shortcode] <- shortcode
    }
  }

  lines_to_write <- if (length(wool$yaml) > 0) {
    c(wool$yaml, "", markdown_lines)
  } else {
    markdown_lines
  }
  brio::write_lines(lines_to_write, out_path)

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

fakify_xml <- function(nodes_list) {
  temp_file <- withr::local_tempfile()
  lines <- paste(
    readLines(system.file("template.xml", package = "babeldown")),
    collapse = "\n"
  )
  fill <- if (inherits(nodes_list, "xml_nodeset")) {
    paste(as.character(nodes_list), collapse = "\n")
  } else {
    paste(
      purrr::map_chr(nodes_list, ~paste(as.character(xml2::xml_children(.x)), collapse = "\n")),
      collapse = "\n"
    )
  }
  lines <- sub("FILLHERE", fill, lines)
  writeLines(lines, temp_file)
  xml2::read_xml(temp_file)
}

#' Title
#'
#' @param markdown_string Markdown string to translate
#' @inheritParams deepl_translate
#'
#' @return Translated Markdown string
#' @export
#'
#' @examples
#' \dontrun{
#' deepl_translate_markdown_string(
#'   "[So _incredibly_ **wonderful**](https://ropensci.org)!",
#'   source_lang = "EN",
#'   target_lang = "FR",
#'   formality = "less"
#' )
#' }
deepl_translate_markdown_string <- function(markdown_string,
                                      glossary_name = NULL,
                                      source_lang,
                                      target_lang,
                                      formality = c("default", "more", "less", "prefer_more", "prefer_less")) {
  file <- withr::local_tempfile()
  brio::write_lines(markdown_string, file)
  deepl_translate(
    path = file,
    out_path = file,
    glossary_name = glossary_name,
    source_lang = source_lang,
    target_lang = target_lang,
    formality = formality,
    yaml_fields = NULL
  )

  lines <- brio::read_lines(file)

  if (length(lines) > 1) {
   lines <- paste(lines, collapse = "\n")
  }

  lines <- gsub("\n*$", "", lines)

  lines
}
