#' Render a Quarto multilingual book
#'
#' @param book_folder Path where the book source is located
#' @param output_folder Path of the folder where the book will be rendered
#'
#' @export
#'
render_quarto_multilingual_book <- function(book_folder,
                                            main_language = "en",
                                            language_codes) {

  # start from blank slate ----
  # TODO read actual destination in _quarto.yml config!
  book_output_folder <- file.path(book_folder, "_book")
  if (fs::dir_exists(book_output_folder)) fs::dir_delete(book_output_folder)

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
  ## For the main language ----
  purrr::walk(
    language_codes,
    ~ purrr::walk(
      fs::dir_ls(book_output_folder, glob = "*.html"),
      add_link,
      main_language = main_language,
      language_code = .x
    )
  )

  ## For other languages ----
  for (other_lang in language_codes) {
    languages_to_add <- c(main_language, language_codes[language_codes != other_lang])
    purrr::walk(
      languages_to_add,
      ~ purrr::walk(
        fs::dir_ls(file.path(book_output_folder, other_lang), glob = "*.html"),
        add_link,
        main_language = main_language,
        language_code = .x
        )
    )
  }

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
  fs::dir_copy(
    file.path(temporary_directory, book_name, "_book"),
    file.path(book_folder, "_book", language_code)
  )

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

add_link <- function(path, main_language = main_language, language_code = "en") {
  html <- xml2::read_html(path)

  left_sidebar <- xml2::xml_find_first(html, "//div[@class='sidebar-menu-container']")

  languages_links_div_exists <- (length(xml2::xml_find_first(html, "//div[@id='languages-links']")) > 0)

  if (!languages_links_div_exists) {
    xml2::xml_add_sibling(
      left_sidebar,
      "div",
      class = "sidebar-menu-container",
      id = "languages-links",
      .where = "before"
    )
    xml2::xml_add_child(
      xml2::xml_find_first(html, "//div[@id='languages-links']"),
      "ul",
      class = "list-unstyled mt-1",
      id = "language-links-ul"
    )
  }

  languages_links <- xml2::xml_find_first(html, "//ul[@id='language-links-ul']")

  if (language_code == main_language) {
    new_path <- fs::path_ext_set(basename(path), sprintf(".%s.html", language_code))
    href <- sprintf("/%s", new_path)
  } else {
    new_path <- sub("\\...\\.html", ".html", basename(path))
    href <- sprintf("/%s/%s", language_code, new_path)
  }

  xml2::xml_add_child(
    languages_links,
    "a",
    sprintf("Version in %s", toupper(language_code)),
    class = "toc-action",
    href = href,
    id = sprintf("language-link-%s", language_code)
  )

  just_added_link <- xml2::xml_find_first(html, sprintf("//a[@id='language-link-%s']", language_code))
  xml2::xml_add_parent(just_added_link, "li", id = sprintf("language-link-li-%s", language_code))

  just_added_link_item <- xml2::xml_find_first(html, sprintf("//li[@id='language-link-li-%s']", language_code))
  xml2::xml_add_child(just_added_link_item, "span", " ", .where = 0)
  xml2::xml_add_child(just_added_link_item, "i", class = "bi bi-globe2", .where = 0)

  xml2::write_html(html, path)
}
