# Heavily borrowed from https://github.com/ropenscilabs/trackmd
# Might end up in worded/officedown?  Best place to use seems to
# be the knitr document hook

#' Process Critic Markup syntax for processing by pandoc
#'
#' Converts critic markup syntax in markdown to syntax appropriate for
#' processing by pandoc.  If converted to to `docx` format, will display
#' as MS Word tracked changes.
#'
#' A good place to put this function is in the `document` knitr hook of an
#' [R Markdown output format][rmarkdown::output_format()].
#'
#' @param md markdown input with critic markup, a length-1 character vector
#' @param author Name to attribute Critic Markup changes to. If NULL,
#'   defaults to system user information via [whoami::fullname()].
#' @return A character vector of markdown, split by lines
#' @noRd
#' @importFrom whoami fullname username
#' @importFrom stringi stri_replace_all_regex stri_replace_first_fixed stri_join
#' stri_extract_all_regex
criticmarkup_to_pandoc <- function(md, author = NULL) {
  if (is.null(author)) {
    author <- fullname(fallback = username(fallback = "R User"))
  }

  timestamp <- as.character(Sys.time(), format = "%Y-%m-%dT%H:%M:%SZ")

  captures <- c(
    insertion = "(?s)\\{\\+\\+(.*?)\\+\\+\\}",
    deletion = "(?s)\\{--(.*?)--\\}",
    substitution = "\\{~~(.*?)~>(.*?)~~\\}",
    highlight = "(?s)\\{==(.*?)==\\}\\{>>(.*?)<<\\}\\[\\[(\\d+)\\]\\]",
    comment = "(?s)\\{>>(.*?)<<\\}\\[\\[(\\d+)\\]\\]"
  )

  insertions <- stri_extract_all_regex(md, captures["insertion"],
    omit_no_match = TRUE
  )[[1]]
  deletions <- stri_extract_all_regex(md, captures["deletion"],
    omit_no_match = TRUE
  )[[1]]
  comments <- stri_extract_all_regex(md, "(?s)\\{>>(.*?)<<\\}",
    omit_no_match = TRUE
  )[[1]]
  # Mark paragraph breaks in insertions and deletions
  for (i in insertions) {
    md <- stri_replace_first_fixed(
      md, i,
      stri_replace_all_regex(
        i, "\n{2,}",
        "++}[]{.paragraph-insertion}\n\n{++"
      )
    )
  }
  for (i in deletions) {
    md <- stri_replace_first_fixed(
      md, i,
      stri_replace_all_regex(
        i, "\n{2,}",
        "--}[]{.paragraph-deletion}\n\n{--"
      )
    )
  }
  # Number comments
  for (i in seq_along(comments)) {
    md <- stri_replace_first_fixed(
      md, comments[i],
      stri_join(comments[i], "[[", i, "]]")
    )
  }

  replacements <- c(
    insertion = stri_join(
      "[$1]{.insertion author=\"", author, "\" date=\"",
      timestamp, "\"}"
    ),
    deletion = stri_join(
      "[$1]{.deletion author=\"", author, "\" date=\"",
      timestamp, "\"}"
    ),
    substitution = stri_join(
      "[$1]{.deletion author=\"", author, "\" date=\"",
      timestamp, "\"}",
      "[$2]{.insertion author=\"", author, "\" date=\"",
      timestamp, "\"}"
    ),
    highlight = stri_join(
      "[$2]{.comment-start id=\"$3\" author=\"", author,
      "\" date=\"", timestamp,
      "\"}$1[]{.comment-end id=\"$3\"}"
    ),
    comment = stri_join(
      "[$1]{.comment-start id=\"$2\" author=\"", author,
      "\" date=\"", timestamp,
      "\"}[]{.comment-end id=\"$2\"}"
    )
  )

  md <- stri_replace_all_regex(md, captures, replacements,
    vectorize_all = FALSE
  )
  md
}

cmwrap <- function(rmd) {
  rmd$text <- criticmarkup_to_pandoc(rmd$text)
  return(rmd)
}
