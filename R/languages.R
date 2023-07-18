
.deepl_languages <- function(type = c("target", "source")) {
  type <- rlang::arg_match(type, values = c("target", "source"))
  languages <- deepl_request("languages", type = type)
  do.call(rbind, purrr::map(languages, tibble::as_tibble))
}
#' Languages supported by DeepL API
#'
#' @param type Either "target" or "source"
#'
#' @importFrom memoise memoise
#' @examples
#' \dontrun{
#' deepl_languages(type = "source")
#' deepl_languages(type = "target")
#' }
#'
#' @return A data.frame of languages (language code as "language",
#' name as "name", whether formality is supported as "supports_formality").
#' @export
deepl_languages <- memoise::memoise(.deepl_languages)

examine_source_lang <- function(source_lang) {
  source_languages <- deepl_languages(type = "source")

  if (toupper(source_lang) %in% source_languages[["language"]]) {
    return(toupper(source_lang))
  }

  if (tolower(source_lang) %in% tolower(source_languages[["name"]])) {
    return(source_languages[["language"]][tolower(source_languages[["name"]]) == tolower(source_lang)])
  }

  cli::cli_abort(
    c(
      "Can't find language {.val {source_lang}} as source language code or name.",
      "Maybe a typo or a missing regional code?",
      i = "Run {.run babeldown::deepl_languages(type = 'source')} to get supported languages."
    )
  )
}

examine_target_lang <- function(target_lang) {
  target_languages <- deepl_languages(type = "target")

  if (toupper(target_lang) %in% target_languages[["language"]]) {
    return(toupper(target_lang))
  }

  if (tolower(target_lang) %in% tolower(target_languages[["name"]])) {
    return(target_languages[["language"]][tolower(target_languages[["name"]]) == tolower(target_lang)])
  }

  cli::cli_abort(
    c(
      "Can't find language {.val {target_lang}} as target language code or name.",
      "Maybe a typo or a missing regional code?",
      i = "Run {.run babeldown::deepl_languages(type = 'target')} to get supported languages."
    )
  )
}
