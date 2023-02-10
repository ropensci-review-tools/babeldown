#' Get usage data
#'
#' @return A list with usage data, depending on the account type.
#' @export
#'
deepl_usage <- function() {
  deepl_request("v2/usage")
}
