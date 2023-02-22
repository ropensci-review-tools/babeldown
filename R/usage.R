#' Get DeepL API usage data
#'
#' @return A list with DeepL API usage data, depending on the account type.
#' @export
#'
deepl_usage <- function() {
  deepl_request("usage")
}
