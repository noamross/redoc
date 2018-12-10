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
#' @importFrom stringi stri_replace_all_regex stri_replace_first_fixed stri_join
#' stri_extract_all_regex
preprocess_criticmarkup <- function(input_lines, author = NULL) {

  if (is.null(author)) {
    author <- fullname(fallback = username(fallback = "R User"))
  }

  md <- oneline(input_lines)

  captures <- c(
    insertion = "(?s)\\{\\+\\+(.*?)\\+\\+\\}",
    deletion = "(?s)\\{--(.*?)--\\}",
    substitution = "\\{~~(.*?)~>(.*?)~~\\}",
    highlight = "(?s)\\{==(.*?)==\\}\\{>>(.*?)<<\\}\\[\\[(\\d+)\\]\\]",
    comment = "(?s)\\{>>(.*?)<<\\}\\[\\[(\\d+)\\]\\]"
  )

  insertions <- stri_extract_all_regex(md, captures["insertion"])[[1]]
  deletions <- stri_extract_all_regex(md, captures["deletion"])[[1]]
  comments <- stri_extract_all_regex(md, "(?s)\\{>>(.*?)<<\\}")[[1]]
  #Mark paragraph breaks in insertions and deletions
  for (i in insertions) {
    md <- stri_replace_first_fixed(md, i,
      stri_replace_all_regex(i, "\n{2,}",
                             "++}[]{.paragraph-insertion}\n\n{++"))
  }
  for (i in deletions) {
    md <- stri_replace_first_fixed(md, i,
      stri_replace_all_regex(i, "\n{2,}",
                             "++}[]{.paragraph-insertion}\n\n{++")
    )
  }
  #Number comments
  for (i in seq_along(comments)) {
    md <- stri_replace_first_fixed(md, comments[i],
                                   stri_join(comments[i], "[[", i, "]]"))
  }

  replacements <- c(
    insertion = stri_join("[$1]{.insertion author=\"", author, "\"}"),
    deletion = stri_join("[$1]{.deletion author=\"", author, "\"}"),
    substitution = stri_join("[$1]{.deletion author=\"", author, "\"}",
                             "[$2]{.insertion author=\"", author, "\"}"),
    highlight = stri_join("[$2]{.comment-start id=\"$3\" author=\"", author,
                          "\"}$1[]{.comment-end id=\"$3\"}"),
    comment = stri_join("[$1]{.comment-start id=\"$2\" author=\"", author,
                        "\"}[]{.comment-end id=\"$2\"}")
  )

  md <- stri_replace_all_regex(md, captures, replacements,
                               vectorize_all = FALSE)
}

#' @importFrom stringi stri_replace_first_fixed
wrap_yaml <- function(lines, chunk_df) {
  md <- paste(lines, collapse = "\n")
  chunk_df <- chunk_df[chunk_df$type == "yaml" & chunk_df$label != "yaml-header",]
  for (i in seq_along(chunk_df$label)) {
    md <- stri_replace_first_fixed(md, chunk_df$code[i],
                                   paste0("::: {custom-style=\"chunk-", chunk_df$label[i], "\"}",
                                          "\n\n",
                                          chunk_df$code[i],
                                          "\n\n:::"))
  }
  stri_split_lines1(md)
}
