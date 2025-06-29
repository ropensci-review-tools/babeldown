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

  out_dir <- dirname(out_path)
  if (!dir.exists(out_dir)) {
    cli::cli_abort(
      "The folder to save {.field out_path} ({.val {out_dir}}) does not exist.
      You might have mistyped it, or it needs to be created."
    )
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
  shortcodes_indices <- grep("^\\{\\{<.*>\\}\\}", markdown_lines)
  shortcodes_present <- length(shortcodes_indices) > 0
  if (shortcodes_present) {
    shortcodes <- markdown_lines[shortcodes_indices]

    ## translate shortcodes
    shortcodes <- purrr::map_chr(
      shortcodes,
      translate_shortcode,
      glossary_name = glossary_name,
      source_lang = source_lang,
      target_lang = target_lang,
      formality = formality
    )

    markdown_lines[shortcodes_indices] <- sprintf("`%s`", purrr::map_chr(shortcodes, digest::digest))
    brio::write_lines(markdown_lines, temp_markdown_file)
    wool <- tinkr::yarn$new(path = temp_markdown_file)
  } else {
    wool <- tinkr::yarn$new(path = path)
  }

  # translate some YAML fields ----
  uses_yaml <- (!is.na(wool$frontmatter_format) && wool$frontmatter_format == "YAML")
  if (!is.null(yaml_fields) && uses_yaml) {
    yaml <- yaml::yaml.load(wool$frontmatter, handlers = list(seq = function(x) x))
    if (!is.null(yaml)) {
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
    yaml::write_yaml(
      x = yaml, file = yaml_file,
      handlers = list(logical = yaml::verbatim_logical)
    )
    wool$frontmatter <- c("---", brio::read_lines(yaml_file), "---")
  }
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
    formality = formality,
    glossary_name = glossary_name
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

translate_part <- function(xml,
                           glossary_id,
                           source_lang,
                           target_lang,
                           formality,
                           glossary_name) {
  temp_file <- withr::local_tempfile()
  file.create(temp_file)
  woolish <- tinkr::yarn$new(path = temp_file)
  woolish$body <- fakify_xml(xml)

  ## protect content inside curly braces and math ---c-
  ## protect content inside curly braces and math ----
  woolish$protect_math()$protect_curly()$protect_fences()

  curlies <- woolish$get_protected("curly")
  purrr::walk(curlies, protect_curly)

  maths <- woolish$get_protected("math")
  purrr::walk(maths, protect_math)

  fences <- woolish$get_protected("fence")
  purrr::walk(fences, protect_fence)

  ## protect content inside square brackets ----
  contain_square_brackets <- xml2::xml_find_all(
    woolish$body,
    '//*[contains(text(), "[") and contains(text(), "]")]'
  )
  # for some reason purrr::discard does not work
  contain_square_brackets <- contain_square_brackets[!xml2::xml_name(contain_square_brackets) %in% c("code", "code_block")]

  if (length(contain_square_brackets) > 0) {
    purrr::walk(contain_square_brackets, protect_squaries)
  }

  ## handle blocks that are not actual code ----
  non_code_blocks <- xml2::xml_find_all(
    woolish$body,
    "//d1:code_block[@language='block']"
  )

  purrr::walk(non_code_blocks, protect_non_code_block)

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
      ignore_tags = "code,code_block,curly,math,notranslate,fence"
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

  ### special case for fig-alt
  purrr::walk(
    curlies, translate_alt_curly,
    glossary_name = glossary_name,
    source_lang = source_lang,
    target_lang = target_lang,
    formality = formality
  )
  purrr::walk(curlies, unprotect_curly)
  ## Make math tags text tags again ----
  maths <- xml2::xml_find_all(woolish$body, "//d1:math")
  purrr::walk(maths, unprotect_math)
  ## Make fence tags text tags again ---- 
  fences <- xml2::xml_find_all(woolish$body, "//d1:fence")
  purrr::walk(fences, unprotect_fence)

  ## Unprotect notranslate ----
  notranslates <- xml2::xml_find_all(woolish$body, ".//d1:notranslate")
  purrr::walk(notranslates, unprotect_notranslate)
  nested_text_nodes <- xml2::xml_find_all(woolish$body, ".//d1:squary/d1:text")
  nested_parents <- xml2::xml_parent(nested_text_nodes)
  purrr::walk(nested_parents, untangle_text)

  ## Unprotect square brackets ----
  squaries <- c(
    xml2::xml_find_all(woolish$body, ".//d1:squary"),
    xml2::xml_find_all(woolish$body, ".//squary")
  )
  purrr::walk(squaries, unprotect_squary)
  nested_text_nodes <- xml2::xml_find_all(woolish$body, ".//d1:text/d1:text")
  nested_parents <- xml2::xml_parent(nested_text_nodes)
  purrr::walk(nested_parents, untangle_text)

  ## Make non code blocks code blocks again ----
  non_code_blocks <- xml2::xml_find_all(woolish$body, "//d1:non_code_block")
  purrr::walk(non_code_blocks, unprotect_non_code_block)

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

translate_alt_curly <- function(curly, glossary_name, source_lang, target_lang, formality) {
  has_alt <- !is.na(xml2::xml_attr(curly, "alt"))
  if (has_alt) {
    translated_alt <- deepl_translate_markdown_string(
      xml2::xml_attr(curly, "alt"),
      glossary_name = glossary_name,
      source_lang = source_lang,
      target_lang = target_lang,
      formality = formality
    )
    xml2::xml_text(curly) <- sprintf(
      '{fig-alt="%s"}',
      translated_alt
    )
  }
}

protect_curly <- function(curly) {
  xml2::xml_name(curly) <- "curly"
}
unprotect_curly <- function(curly) {
  xml2::xml_name(curly) <- "text"
}

protect_fence <- function(fence) {
  xml2::xml_name(fence) <- "fence"
}
unprotect_fence <- function(fence) {
  xml2::xml_name(fence) <- "text"
  # remove superfluous new line
  xml2::xml_text(fence) <- sub("\\\n$", "", xml2::xml_text(fence))
}

protect_math <- function(math) {
  if (xml2::xml_name(math) == "softbreak") {
    xml2::xml_attr(math, "softbreak") <- "yes"
  }

  xml2::xml_name(math) <- "math"

}
unprotect_math <- function(math) {
  if (xml2::xml_has_attr(math, "softbreak")) {
    xml2::xml_name(math) <- "softbreak"
  } else {
    xml2::xml_name(math) <- "text"
  }
}
protect_squaries <- function(node) {
  text <- xml2::xml_text(node)
  text <- gsub('\\[', '</text><squary>', text)
  text <- gsub('\\]', '</squary><text>', text)
  text <- sprintf('<text xml:space="preserve">%s</text>', text)
  text <- gsub('<text xml:space="preserve"><\\/text>', '', text)
  text <- sprintf('<text xml:space="preserve">%s</text>', text)
  # hack to preserve colon that DeepL API kills
  text <- gsub("</squary><text>:", ":</squary><text>", text)

  # https://rstudio.github.io/visual-markdown-editing/citations.html#inserting-citations
  at_things <- regmatches(text, gregexpr("@[[:alnum:]\\-\\_\\:\\.\\#\\$\\%\\&\\-\\+\\?\\<\\>\\~\\/]*", text))[[1]]
  # https://github.com/ropensci-review-tools/babeldown/issues/78
  at_things <- sub("</squary><text>", "", at_things)

  footnote_things <- regmatches(text, gregexpr("\\^[[:alnum:]]*", text))[[1]]
  text <- purrr::reduce(
    c(at_things, sub("\\^", "\\\\^", footnote_things)),
    \(text, x) gsub(x, sprintf("<notranslate>%s</notranslate>", x), text),
    .init = text
  )
  text <- xml2::read_xml(text)
  xml2::xml_ns_strip(text)
  text_node <- xml2::xml_find_first(text, "//text")
  xml2::xml_replace(node, .value = text_node)
}

unprotect_squary <- function(node) {
  xml2::xml_name(node) <- "text"
  node_text <- trimws(xml2::xml_text(node))
  if (!grepl(":$", node_text)) {
    xml2::xml_text(node) <- sprintf("[%s]", node_text)
  } else {
    node_text <- sub(":$", "", node_text)
    xml2::xml_text(node) <- sprintf("[%s]:", node_text)
  }
}

unprotect_notranslate <- function(node) {
  xml2::xml_name(node) <- "text"
}

protect_non_code_block <- function(non_code_block) {
  xml2::xml_name(non_code_block) <- "non_code_block"
}

unprotect_non_code_block <- function(non_code_block) {
  xml2::xml_name(non_code_block) <- "code_block"
}

untangle_text <- function(node) {
  text <- xml2::xml_text(node)
  text <- gsub("\\s+", " ", text) # like str_squish w/o str_trim
  # trying to only leave space where needed
  no_left_sibling <- (length(xml2::xml_find_first(node, "preceding-sibling::*")) == 0)
  which <- if (no_left_sibling) {
    "both"
  } else {
    "right"
  }
  text <- trimws(text, which = which)
  xml2::xml_remove(xml2::xml_children(node))
  xml2::xml_replace(
    node,
    xml2::xml_name(node),
    `xml:space`="preserve",
    asis = 'true',
    gsub("\\\n", "", text)
  )
}
