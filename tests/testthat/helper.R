library(httptest2)
with_mock_dir <- function(path, ...) {
  httptest2::with_mock_dir(file.path("fixtures", path), ...)
}

httptest2::set_redactor(function (x) {
  x <- gsub_response(x, "https\\://api-free.deepl.com/v2/", "api/")
  x <- gsub_response(x, "https\\://api.deepl.com/v2/", "api/")
})

if (!nzchar(Sys.getenv("DEEPL_API_KEY"))) {
  Sys.setenv("DEEPL_API_KEY" = "lalala")
}

if (!nzchar(Sys.getenv("DEEPL_API_URL"))) {
  Sys.setenv("DEEPL_API_URL" = "https://api.deepl.com")
}
