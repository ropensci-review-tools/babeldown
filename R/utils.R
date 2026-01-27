is_non_empty_string <- function(x) {
  !is.null(x) && any(nzchar(x))
}

is_path_in <- function(path, paths) {
  tolower(fs::path_tidy(path)) %in% tolower(fs::path_tidy(paths))
}
