is_non_empty_string <- function(x) {
  !is.null(x) && any(nzchar(x))
}

path_rel <- function(path, start) {
  fs::path_tidy(fs::path_rel(path, start))
}
