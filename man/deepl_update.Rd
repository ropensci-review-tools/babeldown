% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update.R
\name{deepl_update}
\alias{deepl_update}
\title{Update a translation of a file in a Git repo}
\usage{
deepl_update(
  path,
  out_path,
  yaml_fields = c("title", "description"),
  glossary_name = NULL,
  source_lang = NULL,
  target_lang = NULL,
  formality = c("default", "more", "less", "prefer_more", "prefer_less"),
  max_commits = 100L
)
}
\arguments{
\item{path}{Path to the Markdown file to be translated (character).}

\item{out_path}{Path where the new translated file should be saved (character).}

\item{yaml_fields}{Vector of character names of YAML fields to translate.}

\item{glossary_name}{Name of the glossary to be used, if any (character).}

\item{source_lang}{Name or code of source language. See \href{https://www.deepl.com/docs-api/general/get-languages/}{DeepL docs}.}

\item{target_lang}{Name or code of source language. See \href{https://www.deepl.com/docs-api/general/get-languages/}{DeepL docs}.}

\item{formality}{Formality level to use (character), one of
\itemize{
\item "default" (default)
\item "less" -- for a more informal language
\item "prefer_more" -- for a more formal language if available, otherwise fallback to default formality
\item "prefer_less" -- for a more informal language if available, otherwise fallback to default formality
}}

\item{max_commits}{Maximal number of commits to go back to to find
when the target and source files were updated.
You might need to increase it if your project is very active.}
}
\value{
None
}
\description{
Re-use existing translation where possible
(at the node level: paragraph, heading, etc.)
}
\details{
The function looks for the latest commit that updated the source file,
and for the latest commit that updated the target file.
If the target file was updated later than the source file,
or at the same time,
nothing happens: you might need to
reorder the Git history with rebase for instance.
}
