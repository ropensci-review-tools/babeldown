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
#' \dontrun{
#' english_lines <- c(
#'   "## A cool section", "",
#'   "This is the first paragraph. `system.file()` is cool, right?", "",
#'   "Another paragraph. I really enjoy developing R packages.", "",
#'   "Do you enjoy debugging?"
#' )
#' file <- withr::local_tempfile()
#' brio::write_lines(english_lines, file)
#' out_path <- withr::local_tempfile()
#' babeldown::deepl_translate(
#'   path = file,
#'   out_path = out_path,
#'   source_lang = "EN",
#'   target_lang = "ES",
#'   formality = "less"
#' )
#' readLines(out_path)
#' }
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

  glossary_id <- examine_glossary(
    glossary_name = glossary_name,
    source_lang_code = source_lang_code,
    target_lang_code = target_lang_code
  )

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
    shortcodes <- markdown_lines[shortcodes_no]

    ## translate shortcodes
    shortcodes <- purrr::map_chr(
      shortcodes,
      translate_shortcode,
      glossary_name = glossary_name,
      source_lang = source_lang,
      target_lang = target_lang,
      formality = formality
    )

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
        yaml[[yaml_field]] <- purrr::map_chr(
          yaml[[yaml_field]],
          deepl_translate_markdown_string,
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
    ceiling(seq_along(xml2::xml_children(wool[["body"]])) / split_size)
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

  brio::write_lines(markdown_lines, out_path)
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

  ## handle blocks that are not actual code ----
  non_code_blocks <- xml2::xml_find_all(
    woolish$body,
    "//d1:code_block[@language='block']"
  )
  replace_non_code_block <- function(non_code_block) {
    xml2::xml_name(non_code_block) <- "non_code_block"
  }
  purrr::walk(non_code_blocks, replace_non_code_block)

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

  ## Make non code blocks code blocks again ----
  non_code_blocks <- xml2::xml_find_all(woolish$body, "//d1:non_code_block")
  replace_non_code_block <- function(non_code_block) {
    xml2::xml_name(non_code_block) <- "code_block"
  }
  purrr::walk(non_code_blocks, replace_non_code_block)

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
  } else if (inherits(nodes_list, "xml_node")) {
    as.character(nodes_list)
  } else
    {
    paste(
      purrr::map_chr(nodes_list, ~ paste(as.character(xml2::xml_children(.x)), collapse = "\n")),
      collapse = "\n"
    )
  }
  lines <- sub("FILLHERE", fill, lines, fixed = TRUE)
  brio::write_lines(lines, temp_file)
  xml2::read_xml(temp_file)
}

#' Translate Markdown string
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

translate_shortcode <- function(shortcode,
                                glossary_name = NULL,
                                source_lang,
                                target_lang,
                                formality = c("default", "more", "less", "prefer_more", "prefer_less")) {
  for (param in c("alt", "caption", "title")) {
    m <- regexpr(sprintf(' %s=".*?"', param), shortcode)
    param_value <- regmatches(shortcode, m)
    if (length(param_value) > 0) {
      translated_param_value <- deepl_translate_markdown_string(
        param_value,
        glossary_name = glossary_name,
        source_lang = source_lang,
        target_lang = target_lang,
        formality = formality
      )

      translated_param_value <- sub(
        '\\"\\.$',
        '\\."',
        translated_param_value
      )
      shortcode <- sub(
        param_value,
        sprintf(" %s ", translated_param_value),
        shortcode,
        fixed = TRUE
      )
    }
  }
  shortcode
}
