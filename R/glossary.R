#' Create or update glossary
#'
#' @param filename Name of the glossary file.
#' See [DeepL docs on supported formats](https://www.deepl.com/en/docs-api/glossaries/formats/).
#' babeldown is stricter: the columns need to be named like `source_lang` and `target_lang`.
#' @param glossary_name Name for the glossary. Defaults to the filename without extension.
#' @param source_lang Name or code of source language. See [DeepL docs](https://www.deepl.com/docs-api/general/get-languages/).
#' @param target_lang Name or code of source language. See [DeepL docs](https://www.deepl.com/docs-api/general/get-languages/).
#'
#' @return glossary ID
#' @export
#'
#' @examples
#' \dontrun{
#' deepl_upsert_glossary(
#'   system.file("example-es-en.csv", package = "babeldown"),
#'   glossary_name = "rstats-glosario",
#'   target_lang = "Spanish",
#'   source_lang = "English"
#' )
#' }
deepl_upsert_glossary <- function(
  filename,
  glossary_name = NULL,
  source_lang,
  target_lang
) {
  # args checking and input preparation ---------------

  format <- tools::file_ext(filename)
  if (!is.element(format, c("tsv", "csv"))) {
    rlang::abort(sprintf("Can't use file format %s, only csv or tsv.", format))
  }

  if (!file.exists(filename)) {
    rlang::abort(sprintf("Can't find file %s.", filename))
  }

  glossary_name <- glossary_name %||%
    tools::file_path_sans_ext(basename(filename))

  source_lang_code <- examine_source_lang(source_lang)
  target_lang_code <- examine_target_lang(target_lang)

  glossary_id <- get_glossary_id(
    glossary_name = glossary_name,
    source_lang = source_lang_code,
    target_lang = target_lang_code
  )

  # prepare entries
  entries <- switch(
    format,
    csv = readr::read_csv(filename, show_col_types = FALSE),
    tsv = readr::read_tsv(filename, show_col_types = FALSE)
  )

  if (!is.element(source_lang, names(entries))) {
    rlang::abort(sprintf("Can't find %s in glossary variables.", source_lang))
  }

  if (!is.element(target_lang, names(entries))) {
    rlang::abort(sprintf("Can't find %s in glossary variables.", target_lang))
  }

  source_target_entries <- entries[, c(source_lang, target_lang)]
  temp_file <- withr::local_tempfile()

  if (format == "csv") {
    readr::write_csv(source_target_entries, temp_file)
    source_target_entries <- glue::glue_collapse(
      brio::read_lines(temp_file)[-1],
      sep = "\n"
    )
  } else {
    readr::write_tsv(source_target_entries, temp_file)
    source_target_entries <- glue::glue_collapse(
      brio::read_lines(temp_file)[-1],
      sep = "\t"
    )
  }

  target_source_entries <- entries[, c(target_lang, source_lang)]
  temp_file <- withr::local_tempfile()

  if (format == "csv") {
    readr::write_csv(target_source_entries, temp_file)
    target_source_entries <- glue::glue_collapse(
      brio::read_lines(temp_file)[-1],
      sep = "\n"
    )
  } else {
    readr::write_tsv(target_source_entries, temp_file)
    target_source_entries <- glue::glue_collapse(
      brio::read_lines(temp_file)[-1],
      sep = "\t"
    )
  }

  # delete existing glossary if necessary ------
  if (!is.null(glossary_id)) {
    deepl_request(
      path = sprintf("glossaries/%s", glossary_id),
      method = "DELETE"
    )
    message(sprintf("Updating glossary %s", glossary_name))
  } else {
    message(sprintf("Creating glossary %s", glossary_name))
  }

  # create glossary ---------------------
  glossary <- deepl_json_request(
    path = "glossaries",
    data = list(
      name = glossary_name,
      dictionaries = list(
        list(
          source_lang = tolower(source_lang_code),
          target_lang = tolower(target_lang_code),
          entries = source_target_entries,
          entries_format = format
        ),
        list(
          source_lang = tolower(target_lang_code),
          target_lang = tolower(source_lang_code),
          entries = target_source_entries,
          entries_format = format
        )
      )
    )
  )
  return(invisible(glossary[["glossary_id"]]))
}

get_glossary_id <- function(glossary_name, source_lang, target_lang) {
  glossaries <- deepl_request("glossaries")[["glossaries"]]

  if (length(glossaries) == 0) {
    return(NULL)
  }

  glossaries <- do.call(
    rbind,
    purrr::map(glossaries, tibblify_glossary)
  )

  glossaries <- glossaries[glossaries[["source_lang"]] == source_lang, ]
  glossaries <- glossaries[glossaries[["target_lang"]] == target_lang, ]

  if (!is.element(glossary_name, glossaries[["name"]])) {
    return(NULL)
  }

  if (length(intersect(glossary_name, glossaries[["name"]])) > 1) {
    rlang::abort(
      sprintf(
        "There are %s glossaries with the name %s, please fix.",
        length(intersect(glossary_name, glossaries[["name"]])) > 1,
        glossary_name
      )
    )
  }

  glossaries[["id"]][glossaries[["name"]] == glossary_name]
}

tibblify_glossary <- function(glossary_list) {
  info <- do.call(
    rbind,
    purrr::map(glossary_list[["dictionaries"]], tibblify_dictionary)
  )
  info[["id"]] <- glossary_list[["glossary_id"]]
  info[["name"]] <- glossary_list[["name"]]

  info
}

tibblify_dictionary <- function(dictionary_list) {
  tibble::tibble(
    source_lang = dictionary_list[["source_lang"]],
    target_lang = dictionary_list[["target_lang"]]
  )
}

examine_glossary <- function(
  glossary_name,
  source_lang_code,
  target_lang_code,
  call = rlang::caller_env()
) {
  if (is.null(glossary_name)) {
    return(NULL)
  }

  glossary_id <- get_glossary_id(
    glossary_name,
    source_lang = source_lang_code,
    target_lang = target_lang_code
  )
  if (is.null(glossary_id)) {
    cli::cli_abort(
      c(
        "Can't find glossary called {.val {glossary_name}} ({.field glossary_name} argument).",
        i = "Check the spelling, or create it with {.fun upsert_glossary}."
      ),
      call = call
    )
  }

  glossary_id
}
