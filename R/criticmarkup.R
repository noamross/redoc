# Heavily borrowed from https://github.com/ropenscilabs/trackmd
# Might end up in worded/officedown?  Best place to use seems to
# be the knitr document hook

#' Process Critic Markup syntax for processing by pandoc
#'
#' Converts critic markup syntax in markdown to syntax appropriate for
#' processing by pandoc.  If converted to to `docx` format, will dispaly
#' as MS Word tracked changes.
#'
#' A good place to put this function is in the `document` knitr hook of an
#' [R Markdown output format][rmarkdown::output_format()].
#'
#' @param input_lines markdown input, a character vector
#' @param author Name to attribute Critic Markup changes to. If NULL,
#'   defaults to system user information.
#' @return A character vector of markdown, split by lines
#' @noRd
#' @importFrom whoami fullname username
#' @importFrom stringi stri_opts_regex stri_match_all_regex stri_replace_first_fixed
preprocess_criticmarkup <- function(input_lines, author = NULL) {
  if (is.null(author)) {
    author <- fullname(fallback = username(fallback = "R User"))
  }

  md <- paste(input_lines, collapse = "\n")
  comment_counter(reset = TRUE)

  md <- replace_all_regex_with_fun_on_matches(
    md,
    regex = "\\{\\+\\+([\\S\\s]*?)\\+\\+\\}",
    render_pandoc_add,
    author
  )

  md <- replace_all_regex_with_fun_on_matches(
    md,
    regex = "\\{--(.*?)--\\}",
    render_pandoc_delete,
    author
  )

  md <- replace_all_regex_with_fun_on_matches(
    md,
    regex = "\\{~~(.*?)~>(.*?)~~\\}",
    render_pandoc_substitution,
    author
  )

  md <- replace_all_regex_with_fun_on_matches(
    md,
    regex = "\\{==(.*?)==\\}\\{>>(.*?)<<\\}",
    render_pandoc_highlight,
    author
  )

  md <- replace_all_regex_with_fun_on_matches(
    md,
    regex = "\\{>>(.*?)<<\\}",
    render_pandoc_comment,
    author
  )

  # md <- gsub(md, pattern = "\\{\\+\\+(.*?)\\+\\+\\}",
  #              replacement = render_pandoc_add("\\1", author))
  # md <- gsub(md, pattern = "\\{--(.*?)--\\}",
  #              replacement = render_pandoc_delete("\\1", author))
  # md <- gsub(md, pattern = "\\{~~(.*?)~>(.*?)~~\\}",
  #              replacement = render_pandoc_substitution("\\1", "\\2", author))
  # md <- gsub(md, pattern = "\\{==(.*?)==\\}\\{>>(.*?)<<\\}",
  #              replacement = render_pandoc_highlight("\\1", "\\2", author))
  # md <- gsub(md, pattern = "\\{>>(.*?)<<\\}",
  #              replacement = render_pandoc_comment("\\1", author))

  comment_counter(reset = TRUE)

  strsplit(md, "\n", fixed = TRUE)[[1]]
}

replace_all_regex_with_fun_on_matches <-
  function(md, regex, render_fn, author) {
    matches <- stri_match_all_regex(
      str = md,
      pattern = regex,
      omit_no_match = TRUE,
      opts_regex = stri_opts_regex(multiline = TRUE))[[1]]

    for (i in seq_len(nrow(matches))) {
      md <- stri_replace_first_fixed(
        md,
        matches[i, 1],
        render_fn(matches[i, -1], author)
      )
    }
    return(md)
  }

.comenv <- new.env(parent = emptyenv())
comment_counter <- function(reset = FALSE, init_comment = 1) {
  if (reset) {
    return(.comenv$n <- init_comment)
  }
  .comenv$n <- .comenv$n + 1L
  .comenv$n - 1L
}

render_pandoc_add <- function(text, author) {
  texts <- strsplit(text, "\n{2,}")[[1]]
  texts <- lapply(texts, function(text) {
    paste0("[", text, "]{.insertion author=\"", author, "\"}")
  })
  if (length(texts) > 1) {
    texts[-length(texts)] <-
      paste0(
        texts[-length(texts)],
        "[]{.paragraph-insertion author=\"", author, "\"}"
      )
    texts <- paste0(texts, collapse = "\n\n")
  }
  texts
}

render_pandoc_delete <- function(text, author) {
  texts <- strsplit(text, "\n{2,}")[[1]]
  texts <- lapply(texts, function(text) {
    paste0("[", text, "]{.deletion author=\"", author, "\"}")
  })
  if (length(texts) > 1) {
    texts[-length(texts)] <-
      paste0(
        texts[-length(texts)],
        "[]{.paragraph-deletion author=\"", author, "\"}"
      )
    texts <- paste0(texts, collapse = "\n\n")
  }
  texts
}

render_pandoc_substitution <- function(text, author) {
  paste0(
    "[", text[1], "]{.deletion author=\"", author, "\"}",
    "[", text[2], "]{.insertion author=\"", author, "\"}"
  )
}

render_pandoc_comment <- function(comment, author) {
  id <- comment_counter()
  paste0(
    "[", comment, "]{.comment-start id=\"", id,
    "\" author=\"", author, "\"}[]{.comment-end id=\"", id, "\"}"
  )
}

render_pandoc_highlight <- function(text, author) {
  id <- comment_counter()
  paste0(
    "[", text[2], "]{.comment-start id=\"", id,
    "\" author=\"", author, "\"}", text[1], "[]{.comment-end id=\"", id, "\"}"
  )
}
