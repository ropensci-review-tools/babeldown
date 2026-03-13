#' @importFrom memoise memoise

deepl_url <- function() {
  Sys.getenv("DEEPL_API_URL", "https://api-free.deepl.com")
}

deepl_key <- function() {
  key <- Sys.getenv("DEEPL_API_KEY")

  if (!nzchar(key)) {
    rlang::abort("Can't find an `DEEPL_API_KEY` environment variable.")
  }

  key
}

deepl_api_version <- function(path) {
  if (grepl("glossar", path)) {
    "v3"
  } else {
    "v2"
  }
}

deepl_request_basic <- function(path, method) {
  httr2::request(deepl_url()) |>
    httr2::req_url_path_append(deepl_api_version(path)) |>
    httr2::req_url_path_append(path) |>
    httr2::req_headers(
      "Authorization" = sprintf("DeepL-Auth-Key %s", deepl_key())
    ) |>
    httr2::req_method(method) |>
    httr2::req_throttle(capacity = 30, fill_time_s = 60) |>
    httr2::req_retry(max_tries = 3)
}

deepl_request <- function(path, method = "GET", ...) {
  resp <- deepl_request_basic(path, method) |>
    httr2::req_url_query(...) |>
    httr2::req_perform()

  if (!is.na(httr2::resp_content_type(resp))) {
    httr2::resp_body_json(resp)
  }
}

deepl_json_request <- function(path, ...) {
  browser()
  deepl_request_basic(path, method = "POST") |>
    httr2::req_body_json(...) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}


deepl_form_request <- function(path, ...) {
  deepl_request_basic(path, method = "POST") |>
    httr2::req_body_form(...) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}
