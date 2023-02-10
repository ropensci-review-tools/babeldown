library(httptest2)
with_mock_dir <- function(path, ...) {
  httptest2::with_mock_dir(file.path("fixtures", path), ...)
}

if (!nzchar(Sys.getenv("DEEPL_API_KEY"))) {
  Sys.setenv("DEEPL_API_KEY" = "lalala")
}
