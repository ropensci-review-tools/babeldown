#' Update a translation of a file in a Git repo
#'
#' Re-use existing translation where possible
#' (at the node level: paragraph, heading, etc.)
#' `deepl_update()` is based on the commit history in a single branch,
#' `deepl_branch_update()` is based on the commit history in a branch
#' against another (in a GitHub PR for instance).
#'
#' @details
#' `deepl_update()` looks for the latest commit that updated the source file,
#' and for the latest commit that updated the target file.
#' If the target file was updated later than the source file,
#' or at the same time,
#' nothing happens: you might need to
#' fix up the Git history with rebase for instance.
#' Also note that if there were some commits irrelevant to the translation
#' but containing edits to the source file, like typo fixes,
#' too much of the target file will be translated automatically again.
#'
#' `deepl_branch_update()` is more automatic.
#' It will run `deepl_update()`
#'
#'
#' @inheritParams deepl_translate
#' @param max_commits Maximal number of commits to go back to to find
#' when the target and source files were updated.
#' You might need to increase it if your project is very active.
#'
#' @return None
#' @export
#'
#'
deepl_update <- function(
  path,
  out_path,
  yaml_fields = c("title", "description"),
  glossary_name = NULL,
  source_lang = NULL,
  target_lang = NULL,
  formality = c("default", "more", "less", "prefer_more", "prefer_less"),
  max_commits = 100L
) {
  if (!fs::file_exists(path)) {
    cli::cli_abort("Can't find {.var path} {path}.")
  }

  if (!fs::file_exists(out_path)) {
    cli::cli_abort("Can't find {.var out_path} {out_path}.")
  }

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

  repo <- fs::path_tidy(rprojroot::find_root(rprojroot::is_git_root, path))

  path <- fs::path_rel(fs::path_expand(fs::path_tidy(path)), start = repo)
  out_path <- fs::path_rel(
    fs::path_expand(fs::path_tidy(out_path)),
    start = repo
  )

  # determine whether out_path is out of date
  log <- gert::git_log(repo = repo, max = max_commits)
  found_source <- FALSE
  latest_source_commit_index <- 0
  while (!found_source) {
    latest_source_commit_index <- latest_source_commit_index + 1
    diff_info <- gert::git_diff(
      log[["commit"]][[latest_source_commit_index]],
      repo = repo
    )
    # TODO or not, won't work if it was renamed in the important timeframe
    found_source <- is_path_in(path, diff_info[["new"]])
  }

  found_target <- FALSE
  latest_target_commit_index <- 0
  while (!found_target) {
    latest_target_commit_index <- latest_target_commit_index + 1
    diff_info <- gert::git_diff(
      log[["commit"]][[latest_target_commit_index]],
      repo = repo
    )
    # TODO or not, won't work if it was renamed in the important timeframe
    found_target <- is_path_in(out_path, diff_info[["new"]])
  }

  if (latest_source_commit_index >= latest_target_commit_index) {
    return(NULL)
  }

  deepl_part_translate(
    path = path,
    out_path = out_path,
    repo = repo,
    yaml_fields = yaml_fields,
    glossary_id = glossary_id,
    source_lang = source_lang,
    target_lang = target_lang,
    formality = formality,
    tail_commit = log[["commit"]][[latest_target_commit_index]],
    tip_commit = log[["commit"]][[latest_source_commit_index]]
  )
}

tags_the_same <- function(tag1, tag2) {
  # TODO: or just compare as.character() of tags??
  xml2::xml_text(tag1) == xml2::xml_text(tag2) &&
    all(
      purrr::map_chr(xml2::xml_children(tag1), xml2::xml_name) ==
        purrr::map_chr(xml2::xml_children(tag2), xml2::xml_name)
    )
}

deepl_part_translate <- function(
  path,
  out_path,
  repo,
  yaml_fields,
  glossary_id,
  source_lang,
  target_lang,
  formality,
  tip_commit,
  tail_commit
) {
  dir_at_target_latest_update <- withr::local_tempdir()
  fs::dir_copy(repo, dir_at_target_latest_update, overwrite = TRUE)
  gert::git_reset_hard(
    ref = tail_commit,
    repo = dir_at_target_latest_update
  )
  old_source <- tinkr::yarn$new(
    file.path(dir_at_target_latest_update, path)
  )

  new_source <- tinkr::yarn$new(file.path(repo, path))

  old_target <- tinkr::yarn$new(file.path(repo, out_path))

  same_structure <-
    (xml2::xml_length(old_source$body) == xml2::xml_length(old_target$body)) &&
    all(
      purrr::map_chr(xml2::xml_children(old_source$body), xml2::xml_name) ==
        purrr::map_chr(xml2::xml_children(old_target$body), xml2::xml_name)
    )

  if (!same_structure) {
    cli::cli_abort(
      "Old version of {path}, and current {out_path}, do not have an equivalent XML structure."
    )
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

  new_target$write(file.path(repo, out_path))
}

deepl_branch_update <- function(path = ".", max = 100) {
  current_branch <- gert::git_branch(repo = path)
  tip_commit <- gert::git_commit_id(current_branch, repo = path)
  tail_commit <- git_merge_find_base(
    current_branch,
    target = .git_default_branch(),
    repo = path
  )
  excludes <- read_excludes(path) |>
    purrr::map(\(x) fs::dir_ls(path = path, glob = x)) |>
    unlist()

  log <- gert::git_log(ref = tip_commit, max = max, repo = path)

  # commit after the merge base
  first_branch_commit_index <- which(log[["commit"]] == tail_commit) - 1
  updated_files <- log[["commit"]][seq_len(first_branch_commit_index)] |>
    purrr::map(\(x) commit_files(x, repo = path)) |>
    unlist() |>
    purrr::keep(\(x) fs::path_ext(x) %in% c("md", "Rmd", "qmd")) |>
    setdiff(excludes)

  if (length(updated_files) == 0) {
    return(TRUE)
  }

  purrr::walk(
    updated_files,
    update_file_translations,
    path = path,
    tip_commit = tip_commit,
    tail_commit = tail_commit
  )
}

update_file_translations <- function(file, path, tip_commit, tail_commit) {
  file_targets <- file_targets(file, path)
  purrr::walk(
    file_targets,
    guess_translate,
    path = path,
    source = file,
    tip_commit = tip_commit,
    tail_commit = tail_commit
  )
}

get_extension <- function(file) {
  no_ext_file <- fs::path_ext_remove(file)
  if (grepl("\\.", no_ext_file)) {
    sub(".*\\.", "", no_ext_file)
  } else {
    ""
  }
}

guess_translate <- function(
  target,
  source,
  path,
  tip_commit,
  tail_commit
) {
  target_language <- read_extension(path, get_extension(target))[["target"]]
  source_language <- read_extension(path, get_extension(source))[["source"]]

  preferences <- read_preferences(
    path = path,
    source_lang = source_language,
    target_lang = target_language
  )
  browser()
  glossary_id <- babeldown:::get_glossary_id(
      preferences[["glossary"]],
      source_lang = source_language,
      target_lang = target_language
    )
  }
  deepl_part_translate(
    path = file.path(path, source),
    out_path = file.path(path, target),
    repo = path,
    source_lang = source_language,
    target_lang = target_language,
    tail_commit = tail_commit,
    tip_commit = tip_commit,
    formality = preferences[["formality"]],
    yaml_fields = preferences[["yaml_fields"]],
    glossary_id = glossary_id
  )
}

file_targets <- function(file, path) {
  setdiff(
    fs::dir_ls(path = path, regex = fs::path_ext_remove(fs::path_file(file))),
    file
  )
}

commit_files <- function(commit, repo) {
  gert::git_diff(commit, repo = repo)[["new"]]
}


.git_default_branch <- function(path = ".") {
  if (gert::git_branch_exists("main", repo = path)) {
    return("main")
  }

  if (gert::git_branch_exists("master", repo = path)) {
    return("master")
  }

  cli::cli_abort(
    "Can't guess default branch in {.path {path}}, 
  not main, not master."
  )
}
