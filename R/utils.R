is_non_empty_string <- function(x) {
  !is.null(x) && any(nzchar(x))
}

is_path_in <- function(path, paths) {
  normalizePath(path, winslash = '/') %in% normalizePath(paths, winslash = '/')
}
