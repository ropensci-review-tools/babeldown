#' @section API URL:
#'
#' The DeepL API URL depends on your API plan.
#' babeldown uses the DeepL _free_ API URL by default.
#' If you use a Pro plan, set the API URL via
#'
#' ```r
#' Sys.setenv("DEEPL_API_URL" = "https://api.deepl.com")
#' ```
#'
#' @section API key:
#'
#' Set your API key via the environment variable `DEEPL_API_KEY`.
#' You could store it with the keyring package and retrieve it like so:
#'
#' ```r
#' Sys.setenv(DEEPL_API_KEY = keyring::key_get("deepl"))
#' ```
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
