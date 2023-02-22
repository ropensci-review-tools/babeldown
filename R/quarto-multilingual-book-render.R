#' Render a Quarto multilingual book
#'
#' @param book_folder Path where the book source is located
#' @param output_folder Path of the folder where the book will be rendered
#'
#' @export
#'
render_quarto_multilingual_book <- function(book_folder, language_codes) {

  # start from blank slate ----
  # TODO read actual destination in _quarto.yml config!
  if (fs::dir_exists("_book")) fs::dir_delete("_book")

  # render book ----
  withr::with_dir(book_folder, {
    quarto::quarto_render(as_job = FALSE)
  })

  purrr::walk(
    language_codes,
    render_quarto_lang_book,
    book_folder = book_folder
  )

  # Add the language switching link to the sidebar ----
  # TODO this does not add all combinations
  purrr::walk(fs::dir_ls("_book", glob = "*.html"), add_link, language_code = "es")
  purrr::walk(
    language_codes,
    ~ purrr::walk(fs::dir_ls(sprintf("_book/%s", .x), glob = "*.html"), add_link, language_code = .x)
  )

}

render_quarto_lang_book <- function(language_code, book_folder) {

  temporary_directory <- withr::local_tempdir()
  fs::dir_copy(book_folder, temporary_directory)
  book_name <- fs::path_file(book_folder)

  config <- yaml::read_yaml(file.path(temporary_directory, book_name, "_quarto.yml"))
  config$lang <- language_code

  config$book$chapters <- purrr::map(
    config$book$chapters,
    use_lang_chapter,
    language_code = language_code,
    book_name = book_name,
    directory = temporary_directory
  )

  yaml::write_yaml(config, file.path(temporary_directory, book_name, "_quarto.yml"))

  # fix for Boolean that is yes and should be true
  config_lines <- brio::read_lines(file.path(temporary_directory, book_name, "_quarto.yml"))
  config_lines[grepl("code-link", config_lines)] <- sub("yes", "true", config_lines[grepl("code-link", config_lines)])
  config_lines[grepl("reader-mode", config_lines)] <- sub("yes", "true", config_lines[grepl("reader-mode", config_lines)])
  brio::write_lines(config_lines, file.path(temporary_directory, book_name, "_quarto.yml"))

  # Render language book
  withr::with_dir(file.path(temporary_directory, book_name), {
    quarto::quarto_render(as_job = FALSE)
  })

  # Copy it to local not temporary _book/<language-code>
  fs::dir_copy(file.path(temporary_directory, book_name, "_book"), file.path(book_folder, "_book", language_code))

}

use_lang_chapter <- function(chapters_list, language_code, book_name, directory) {

    original_chapters_list <- chapters_list

    if (is.list(chapters_list)) {
      chapters_list$chapters <- gsub("\\.Rmd", sprintf(".%s.Rmd", language_code), chapters_list$chapters)
      chapters_list$chapters <- gsub("\\.qmd", sprintf(".%s.qmd", language_code), chapters_list$chapters)
      if (any(!fs::file_exists(chapters_list$chapters))) {
        chapters_not_translated <- !fs::file_exists(file.path(directory, book_name, chapters_list$chapters))
        chapters_list$chapters[chapters_not_translated]  <- original_chapters_list[chapters_not_translated]
      }
    } else {

      chapters_list <- gsub("\\.Rmd", sprintf(".%s.Rmd", language_code), chapters_list)
      chapters_list <- gsub("\\.qmd", sprintf(".%s.qmd", language_code), chapters_list)
      if (!fs::file_exists(file.path(directory, book_name, chapters_list))) {
        chapters_list <- original_chapters_list
      }
    }

    chapters_list
  }

add_link <- function(path, language_code = "en") {
  html <- xml2::read_html(path)
  sidebar_action_links <- xml2::xml_find_all(html, "//div[@class='action-links']")

  if (lang == "en") {
    new_path <- sub("\\...\\.html", ".html", basename(path))
    xml2::xml_add_child(
      sidebar_action_links,
      "a",
      sprintf("Version in <b>%s</b>", toupper(language_code)),
      class = "toc-action",
      href = sprintf("/%s", new_path)
    )
  } else {
    new_path <- fs::path_ext_set(basename(path), sprintf(".%s.html", language_code))
    xml2::xml_add_child(
      sidebar_action_links,
      "a",
      sprintf("Version in <b>%s</b>", toupper(language_code)),
      class = "toc-action",
      href = sprintf("/%s/%s", language_code, new_path)
    )
  }
  a <- xml2::xml_children(sidebar_action_links)[length(xml2::xml_children(sidebar_action_links))]
  xml2::xml_add_parent(a, "p")


  xml2::write_html(html, path)
}
