is_non_empty_string <- function(x) {
  !is.null(x) && any(nzchar(x))
}

is_path_in <- function(path, paths) {
  really_normalize_path(path) %in% really_normalize_path(paths)
}

really_normalize_path <- function(path) {
  tolower(normalizePath(path, winslash = '/'))
}
