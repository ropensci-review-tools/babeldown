is_non_empty_string <- function(x) {
  !is.null(x) && any(nzchar(x))
}

is_path_in <- function(path, paths) {
  gsub("\\\\", "/", path) %in% paths
}
