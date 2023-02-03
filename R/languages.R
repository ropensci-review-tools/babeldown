#' @importFrom memoise memoise

.deepl_languages <- function(type = c("target", "source")) {
  languages <- deepl_request("v2/languages", type = type)
  do.call(rbind, purrr::map(languages, tibble::as_tibble))
}

deepl_languages <- memoise::memoise(.deepl_languages)

examine_source_lang <- function(source_lang) {
  source_languages <- deepl_languages(type = "source")

  if (source_lang %in% source_languages[["language"]]) {
    return(source_lang)
  }

  if (source_lang %in% source_languages[["name"]]) {
    return(source_languages[["language"]][source_languages[["name"]] == source_lang])
  }

  rlang::abort(
    sprintf(
      "Can't find language %s as source language code or name.",
      source_lang
    )
  )
}

examine_target_lang <- function(target_lang) {
  target_languages <- deepl_languages(type = "target")

  if (target_lang %in% target_languages[["language"]]) {
    return(target_lang)
  }

  if (target_lang %in% target_languages[["name"]]) {
    return(target_languages[["language"]][target_languages[["name"]] == target_lang])
  }

  rlang::abort(
    sprintf(
      "Can't find language %s as target language code or name.",
      target_lang
    )
  )
}
