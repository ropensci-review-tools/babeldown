read_config <- function(path) {
  config_path <- file.path(path, "_deepl.yml")

  if (!file.exists(config_path)) {
    cli::cli_abort("Can't find {.path config_path}")
  }

  yaml::read_yaml(config_path)
}

read_preferences <- function(path = ".", source_lang, target_lang) {
  config <- read_config(path) |>
    purrr::pluck("preferences") |>
    purrr::keep(\(x) x$source == source_lang) |>
    purrr::keep(\(x) x$target == target_lang)
  if (length(config) == 0) {
    cli::cli_abort(
      "Can't find preferences for source language {source_lang}
    and target language {target_lang}."
    )
  }

  if (length(config) > 1) {
    cli::cli_abort(
      "More than one list of preferences for source language {.val {source_lang}}
    and target language {.val {target_lang}}."
    )
  }

  unlist(config, recursive = FALSE)
}

read_excludes <- function(path = ".") {
  read_config(path) |>
    purrr::pluck("excludes")
}

read_excludes <- function(path = ".") {
  read_config(path) |>
    purrr::pluck("targets")
}

read_extension <- function(path = ".", extension) {
  config <- read_config(path) |>
    purrr::pluck("languages") |>
    purrr::keep(\(x) x$extension == extension)
  if (length(config) == 0) {
    cli::cli_abort(
      "Can't find settings for extension {.val {extension}}."
    )
  }

  if (length(config) > 1) {
    cli::cli_abort(
      "More than one list of settings for extension {.val {extension}}."
    )
  }

  unlist(config, recursive = FALSE)
}
