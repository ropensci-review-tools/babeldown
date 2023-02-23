is_non_empty_string <- function(x) {
  !is.null(x) && nzchar(x)
}
