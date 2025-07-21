#' Update a translation of a file in a Git repo
#'
#' Re-use existing translation where possible
#' (at the node level: paragraph, heading, item of list at the first level, etc.)
#' `deepl_update()` is based on the commit history in a single branch,
#' `deepl_branch_update()` is based on the commit history in a branch
#' against another (in a GitHub PR for instance), and requires a
#' configuration file (see "Configuration" section).
#'
#' @section Limitations of `deepl_update()`:
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
#' @section Configuration for `deepl_branch_update()`:
#' `deepl_branch_update()` is more automatic but needs configuration.
#' In a branch, for any edits made to any Rmd/qmd/md file, it will update
#' the translations of the Rmd/qmd/md file with the same name and a language code.
#' For instance if a branch had commits updating `pkg_building.Rmd`,
#' `deepl_branch_update()` will update `pkg_building.es.Rmd`.
#' Do not run it if your PR updated both `pkg_building.Rmd` and
#' `pkg_building.es.Rmd`.
#' Make sure to read the section about Markdown formatting.
#'
#' The configuration needed must live under `_deepl.yml` at the root of the Git repo.
#' It must contains those fields:
#' - "preferences" that for any source-target pair, indicates the preference for the
#' `formality`, `glossary` and `yaml_fields` arguments of `deepl_translate()`.
#' - "excludes" that indicates which files to exclude from the automatic detection
#' (all Rmd/qmd/md files are otherwise considered). It is used as `glob` arguments in
#' calls to `fs::dir_ls()`.
#' - "languages" that for any language code used in file names, indicates what
#' DeepL API code to use when it is a source or target language.
#'
#' Example (`_deepl.yml`):
#' `````{r, echo=FALSE,results = 'asis', warning=FALSE}
#' cat("\n\n```yaml\n")
#' cat(paste(
#' readLines(system.file("_deepl.yml", package = "babeldown"))), sep = "\n"
#' )
#' cat("\n```\n")
#' `````
#'
#'
#' @inheritParams deepl_translate
#' @param max_commits Maximal number of commits to go back to to find
#' when the target and source files were updated.
#' You might need to increase it if your project is very active.
#' @param repo path to the Git repository.
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
    glossary = glossary,
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
  glossary,
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
    file.path(dir_at_target_latest_update, path),
    sourcepos = TRUE
  )

  new_source <- tinkr::yarn$new(file.path(repo, path), sourcepos = TRUE)

  old_target <- tinkr::yarn$new(file.path(repo, out_path), sourcepos = TRUE)
  old_source_nodes <- purrr::map_chr(
    xml_kiddos(old_source$body),
    xml2::xml_name
  )
  old_target_nodes <- purrr::map_chr(
    xml_kiddos(old_target$body),
    xml2::xml_name
  )
  same_structure <-
    (xml_kiddos_length(old_source$body) ==
      xml_kiddos_length(old_target$body)) &&
    all(old_source_nodes == old_target_nodes)
  if (!same_structure) {
    present_node <- function(node) {
      cli::cli_alert_info(
        "{xml2::xml_name(node)} ({substr(xml2::xml_text(node), 1, 50)})"
      )
    }
    first_bad <- min(which(old_source_nodes != old_target_nodes))
    present_node(xml_kiddos(old_source$body)[[first_bad]])
    present_node(xml_kiddos(old_target$body)[[first_bad]])
    cli::cli_abort(
      "Old version of {path}, and current {out_path}, do not have an equivalent XML structure."
    )
  }

  new_target <- new_source
  for (tag in xml_kiddos(new_target$body)) {
    same_tag <- purrr::map_lgl(
      xml_kiddos(old_source$body),
      tags_the_same,
      tag
    )
    existing_translation <- any(same_tag)
    if (existing_translation) {
      same_index <- which(same_tag)[1]
      xml2::xml_replace(
        tag,
        xml_kiddos(old_target$body)[[same_index]]
      )
    } else {
      # get translation
      markdown_lines <- if (first_line(tag) == last_line(tag)) {
        brio::read_lines(path)[first_line(tag) + length(new_target$frontmatter)]
      } else {
        brio::read_lines(path)[
          (first_line(tag):last_line(tag) + length(new_target$frontmatter))
        ]
      }

      translation <- deepl_translate_markdown_string(
        paste(markdown_lines, collapse = "\n"),
        glossary = glossary,
        source_lang = source_lang,
        target_lang = target_lang,
        formality = formality
      )
      xml2::xml_set_attr(tag, "translation", translation)

      # update sourcepos for ordering
      preceding_siblings <- xml2::xml_find_all(tag, "preceding-sibling::*")
      just_before_sibling <- preceding_siblings[length(preceding_siblings)]
      xml2::xml_set_attr(
        tag,
        "sourcepos",
        increment_sourcepos(just_before_sibling)
      )
    }
  }

  current_lines <- brio::read_lines(out_path)

  kiddos <- xml_kiddos(new_target$body)

  new_lines <- purrr::map(
    kiddos,
    tackle_node,
    current_lines = current_lines
  ) |>
    unlist()

  new_lines <- c(old_target$frontmatter, new_lines)
  brio::write_lines(new_lines, out_path)
}

tackle_node <- function(node, current_lines) {
  if (xml2::xml_has_attr(node, "translation")) {
    return(xml2::xml_attr(node, "translation"))
  }

  current_lines <- current_lines[first_line(node):last_line(node)]

  # check whether we need to add an empty line
  # assuming one at most is needed
  next_sibling <- xml2::xml_find_first(node, "following-sibling::*")

  last_node <- length(next_sibling) == 0
  if (last_node) {
    return(current_lines)
  }

  missing_empty_line <- (first_line(next_sibling) > (last_line(node) + 1))
  if (missing_empty_line) {
    current_lines <- c(current_lines, "")
  }

  if (xml2::xml_name(node) == "heading" && nzchar(tail(current_lines, n = 1))) {
    current_lines <- c(current_lines, "")
  }

  if (
    xml2::xml_name(next_sibling) == "list" && nzchar(tail(current_lines, n = 1))
  ) {
    current_lines <- c(current_lines, "")
  }

  current_lines
}

#' @rdname deepl_update
#'
#'
#' @export
deepl_branch_update <- function(repo = ".", max_commits = 100) {
  current_branch <- gert::git_branch(repo = repo)
  tip_commit <- gert::git_commit_id(current_branch, repo = repo)
  tail_commit <- gert::git_merge_find_base(
    current_branch,
    target = .git_default_branch(),
    repo = repo
  )
  excludes <- read_excludes(repo) |>
    purrr::map(\(x) fs::dir_ls(path = repo, glob = x)) |>
    unlist()

  log <- gert::git_log(ref = tip_commit, max = max_commits, repo = repo)

  # commit after the merge base
  first_branch_commit_index <- which(log[["commit"]] == tail_commit) - 1
  updated_files <- log[["commit"]][seq_len(first_branch_commit_index)] |>
    purrr::map(\(x) commit_files(x, repo = repo)) |>
    unlist() |>
    purrr::keep(\(x) fs::path_ext(x) %in% c("md", "Rmd", "qmd")) |>
    setdiff(excludes)

  if (length(updated_files) == 0) {
    return(TRUE)
  }

  purrr::walk(
    updated_files,
    update_file_translations,
    repo = repo,
    tip_commit = tip_commit,
    tail_commit = tail_commit
  )
}

update_file_translations <- function(file, repo, tip_commit, tail_commit) {
  file_targets <- file_targets(file, repo)
  purrr::walk(
    file_targets,
    guess_translate,
    repo = repo,
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
  repo,
  tip_commit,
  tail_commit
) {
  target_language <- read_extension(repo, get_extension(target))[["target"]]
  source_language <- read_extension(repo, get_extension(source))[["source"]]

  preferences <- read_preferences(
    repo = repo,
    source_lang = source_language,
    target_lang = target_language
  )

  glossary <- preferences[["glossary"]]

  deepl_part_translate(
    path = file.path(repo, source),
    out_path = file.path(repo, target),
    repo = repo,
    source_lang = source_language,
    target_lang = target_language,
    tail_commit = tail_commit,
    tip_commit = tip_commit,
    formality = preferences[["formality"]],
    yaml_fields = preferences[["yaml_fields"]],
    glossary = glossary
  )
}

file_targets <- function(file, repo) {
  setdiff(
    fs::dir_ls(path = repo, regex = fs::path_ext_remove(fs::path_file(file))),
    file
  )
}

commit_files <- function(commit, repo) {
  gert::git_diff(commit, repo = repo)[["new"]]
}


.git_default_branch <- function(repo = ".") {
  if (gert::git_branch_exists("main", repo = repo)) {
    return("main")
  }

  if (gert::git_branch_exists("master", repo = repo)) {
    return("master")
  }

  cli::cli_abort(
    "Can't guess default branch in {.path {repo}}, 
  not main, not master."
  )
}

xml_kiddos <- function(xml) {
  kiddos <- xml2::xml_children(xml)
  kiddos <- purrr::discard(kiddos, \(x) xml2::xml_name(x) == "list")
  # this avoids the items that are in nested lists.
  list_items <- xml2::xml_find_all(xml, "d1:list/d1:item")

  kiddos <- c(kiddos, list_items)
  kiddos[order(purrr::map_dbl(kiddos, first_line))]
}

xml_kiddos_length <- function(xml) length(xml_kiddos(xml))

first_line <- function(node) {
  as.numeric(sub(":.*", "", xml2::xml_attr(node, "sourcepos")))
}

last_line <- function(node) {
  as.numeric(sub(":.*", "", sub(".*-", "", xml2::xml_attr(node, "sourcepos"))))
}

increment_sourcepos <- function(node) {
  sprintf("%s:1-%s:2", last_line(node) + 1, last_line(node) + 1)
}
