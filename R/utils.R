is_non_empty_string <- function(x) {
  !is.null(x) && any(nzchar(x))
}
