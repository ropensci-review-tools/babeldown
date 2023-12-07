#' Update a translation of a file in a Git repo
#'
#' Re-use existing translation where possible
#' (at the node level: paragraph, heading, etc.)
#'
#' @details
#' The function looks for the latest commit that updated the source file,
#' and for the latest commit that updated the target file.
#' If the target file was updated later than the source file,
#' or at the same time,
#' nothing happens: you might need to
#' reorder the Git history with rebase for instance.
#'
#'
#' @inheritParams deepl_translate
#'
#' @return None
#' @export
#'
#'
deepl_update <- function(path,
                         out_path,
                         yaml_fields = c("title", "description"),
                         glossary_name = NULL,
                         source_lang = NULL,
                         target_lang = NULL,
                         formality = c("default", "more", "less", "prefer_more", "prefer_less")) {


  rlang::check_installed("rprojroot")
  rlang::check_installed("gert")

  formality <- rlang::arg_match(
    formality,
    values = c("default", "more", "less", "prefer_more", "prefer_less")
  )

  source_lang_code <- examine_source_lang(source_lang)
  target_lang_code <- examine_target_lang(target_lang)

  glossary_id <- examine_glossary(
      glossary_name,
      source_lang_code = source_lang_code,
      target_lang_code = target_lang_code
  )

  translated_lines <- brio::read_lines(out_path)

  repo <- rprojroot::find_root(rprojroot::is_git_root, path)

  # determine whether out_path is out of date
  # TODO or not, make it work for over 100 commits?
  log <- gert::git_log(repo = repo)

  found_source <- FALSE
  latest_source_commit_index <- 0
  while (!found_source) {
    latest_source_commit_index <- latest_source_commit_index + 1
    diff_info <- gert::git_diff(log[["commit"]][[latest_source_commit_index]], repo = repo)
    # TODO or not, won't work if it was renamed in the important timeframe
    found_source <- (fs::path_file(path) %in% diff_info[["new"]])
  }

  found_target <- FALSE
  latest_target_commit_index <- 0
  while (!found_target) {
    latest_target_commit_index <- latest_target_commit_index + 1
    diff_info <- gert::git_diff(log[["commit"]][[latest_target_commit_index]], repo = repo)
    # TODO or not, won't work if it was renamed in the important timeframe
    found_target <- (fs::path_file(out_path) %in% diff_info[["new"]])
  }

  if (latest_source_commit_index >= latest_target_commit_index) {
    return(NULL)
  }

  dir_at_target_latest_update <- withr::local_tempdir()
  fs::dir_copy(repo, dir_at_target_latest_update, overwrite = TRUE)
  gert::git_reset_hard(
    ref = log[["commit"]][[latest_target_commit_index]],
    repo = dir_at_target_latest_update
  )
  old_source <- tinkr::yarn$new(
    file.path(dir_at_target_latest_update, fs::path_file(path))
  )

  new_source <- tinkr::yarn$new(file.path(repo, fs::path_file(path)))

  old_target <- tinkr::yarn$new(file.path(out_path))

  same_structure <-
    (xml2::xml_length(old_source$body) == xml2::xml_length(old_target$body)) &&
    all(
      purrr::map_chr(xml2::xml_children(old_source$body), xml2::xml_name) ==
        purrr::map_chr(xml2::xml_children(old_target$body), xml2::xml_name)
    )

  if (!same_structure) {
    cli::cli_abort("Old version of {path}, and current {out_path}, do not have an equivalent XML structure.")
  }

  new_target <- new_source
  tags_seq <- seq_len(length(xml2::xml_children(new_target$body)))
  for (tag_index in tags_seq) {
    same_tag <- purrr::map_lgl(
      xml2::xml_children(old_source$body),
      tags_the_same,
      xml2::xml_children(new_target$body)[[tag_index]]
    )
    existing_translation <- any(same_tag)
    if (existing_translation) {
      same_index <- which(same_tag)[1]
      xml2::xml_replace(
        xml2::xml_children(new_target$body)[[tag_index]],
        xml2::xml_children(old_target$body)[[same_index]]
      )
    } else {
      translation <- translate_part(
        xml2::xml_children(new_target$body)[[tag_index]],
        glossary_id = glossary_id,
        source_lang = source_lang,
        target_lang = target_lang,
        formality = formality
      )
      translation_kiddo <- xml2::xml_child(translation)
      xml2::xml_replace(
        xml2::xml_children(new_target$body)[[tag_index]],
        translation_kiddo
      )
    }
  }

  new_target$write(out_path)
}

tags_the_same <- function(tag1, tag2) {
  # TODO: or just compare as.character() of tags??
  xml2::xml_text(tag1) == xml2::xml_text(tag2) &&
    all(
      purrr::map_chr(xml2::xml_children(tag1), xml2::xml_name) ==
        purrr::map_chr(xml2::xml_children(tag2), xml2::xml_name)
    )
}
