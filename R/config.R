read_config <- function(repo) {
  config_repo <- file.path(repo, "_deepl.yml")

  if (!file.exists(config_repo)) {
    cli::cli_abort("Can't find {.repo {config_repo}}")
  }

  yaml::read_yaml(config_repo)
}

read_preferences <- function(repo = ".", source_lang, target_lang) {
  config <- read_config(repo) |>
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

read_excludes <- function(repo = ".") {
  read_config(repo) |>
    purrr::pluck("excludes")
}

read_extension <- function(repo = ".", extension) {
  config <- read_config(repo) |>
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
