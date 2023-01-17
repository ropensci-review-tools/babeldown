library(httptest2)
with_mock_dir <- function(path, ...) {
  httptest2::with_mock_dir(file.path("fixtures", path), ...)
}
