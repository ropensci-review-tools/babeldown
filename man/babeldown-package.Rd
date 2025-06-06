% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/babeldown-package.R
\docType{package}
\name{babeldown-package}
\alias{babeldown}
\alias{babeldown-package}
\title{babeldown: Helpers for Automatic Translation of Markdown-based Content}
\description{
Provide workflows and guidance for automatic translation of Markdown-based R content using DeepL API.
}
\section{API URL}{


The DeepL API URL depends on your API plan.
babeldown uses the DeepL \emph{free} API URL by default.
If you use a Pro plan, set the API URL via

\if{html}{\out{<div class="sourceCode r">}}\preformatted{Sys.setenv("DEEPL_API_URL" = "https://api.deepl.com")
}\if{html}{\out{</div>}}
}

\section{API key}{


Set your API key via the environment variable \code{DEEPL_API_KEY}.
You could store it with the keyring package and retrieve it like so:

\if{html}{\out{<div class="sourceCode r">}}\preformatted{Sys.setenv(DEEPL_API_KEY = keyring::key_get("deepl"))
}\if{html}{\out{</div>}}
}

\seealso{
Useful links:
\itemize{
  \item \url{https://github.com/ropensci-review-tools/babeldown}
  \item Report bugs at \url{https://github.com/ropensci-review-tools/babeldown/issues}
}

}
\author{
\strong{Maintainer}: Maëlle Salmon \email{msmaellesalmon@gmail.com} (\href{https://orcid.org/0000-0002-2815-0399}{ORCID})

Other contributors:
\itemize{
  \item Xavier Timbeau [contributor]
  \item rOpenSci (\href{https://ror.org/019jywm96}{ROR}) [funder]
}

}
\keyword{internal}
