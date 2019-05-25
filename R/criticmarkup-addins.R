#' @importFrom stringi stri_c
cm_all_addin <- function(action) {
  if (!requireNamespace("rstudioapi") && rstudioapi::isAvailable()) {
    stop("RStudio and the 'rstudioapi' package is required for this function")
  }
  active_file <- rstudioapi::getSourceEditorContext()
  cursor_position <- active_file$selection[[1]]$range$start
  text <- stri_c(active_file[["contents"]], collapse = "\n")
  text <- switch(action,
                 accept_all = cm_accept_all(text),
                 reject_all = cm_reject_all(text),
                 delete_comments = cm_delete_comments(text))
  rstudioapi::setDocumentContents(text, active_file$id)
  rstudioapi::setCursorPosition(cursor_position, active_file$id)
}

cm_accept_all_addin <- function() cm_all_addin("accept_all")
cm_reject_all_addin <- function() cm_all_addin("reject_all")
cm_delete_comments_addin <- function() cm_all_addin("delete_comments")

#' Accept or reject changes in CriticMarkup syntax in bulk
#'
#' This function modifies a Markdown or R Markdown file by accepting or rejecting
#' changes in CriticMarkup syntax. [Lower-level functions][cm_accept_all()]
#' operate on text if the user wants to integrate these into a pipeline.
#'
#' @param file The name of the Markdown or R Markdown file
#' @param action One of "accept_all", "reject_all", or "delete_comments"
#' @param out The path to write the changed file to. The default is to
#'   overwrite the original.
#' @return The path of the modified file
#' @export
#' @examples
#'
#' outfile = tempfile(fileext = ".md")
#' cm_process_file(redoc_example_criticmarkup(), action = "accept_all", out = outfile)
#' cat(readLines(outfile), sep = "\\n")
cm_process_file <- function(file, action, out = file) {
  text <- readfile(file)
  text <- switch(action,
                 accept_all = cm_accept_all(text),
                 reject_all = cm_reject_all(text),
                 delete_comments = cm_delete_comments(text))
  cat(text, file = out)
  return(out)
}

#' Process CriticMarkup text
#'
#' These functions process CriticMarkup syntax in text and return the
#' modified text with changes accepted or rejected or comments deleted.
#' See [cm_process_file()] to modify a file on-disk.
#'
#' @param text The text to process. Takes a character vector, but multi-line
#'   text should be concatenated into a single character string with line
#'   breaks if there are edits a ross lines.
#' @return A character vector of the modified text.
#' @export
#' @rdname cm_accept_all
#' @importFrom stringi stri_replace_all_regex
#' @examples
#'
#' text <- paste(readLines(redoc_example_criticmarkup()), collapse = '\n')
#' cat(text)
#' cat(cm_accept_all(text))
#' cat(cm_reject_all(text))
#' cat(cm_delete_comments(cm_reject_all(text)))
#'
cm_accept_all <- function(text) {
  #TODO: cleanup extra spaces?
  #TODO: exclude stuff in r chunks
  text <- stri_replace_all_regex(text, "(?s)\\{\\+\\+(.*?)\\+\\+\\}", "$1")
  text <- stri_replace_all_regex(text, "(?s)\\{--(.*?)--\\}", "")
  text <- stri_replace_all_regex(text, "\\{~~(.*?)~>(.*?)~~\\}", "$2")
  return(text)
}

#' @export
#' @rdname cm_accept_all
#' @importFrom stringi stri_replace_all_regex
cm_reject_all <- function(text) {
  text <- stri_replace_all_regex(text, "(?s)\\{\\+\\+(.*?)\\+\\+\\}", "")
  text <- stri_replace_all_regex(text, "(?s)\\{--(.*?)--\\}", "$1")
  text <- stri_replace_all_regex(text, "\\{~~(.*?)~>(.*?)~~\\}", "$1")
  return(text)
}

#' @export
#' @rdname cm_accept_all
#' @importFrom stringi stri_replace_all_regex
cm_delete_comments <- function(text) {
  text <- stri_replace_all_regex(text, "(?s)\\{==(.*?)==\\}", "")
  text <- stri_replace_all_regex(text, "(?s)\\{>>(.*?)<<\\}", "")
}

# cm_accept_current <- function(text, location) {
#   active_file <- rstudioapi::getSourceEditorContext()
#   current_line <- active_file$selection[[1]]$range$start[1]
#   prev_mark <- c(NA_integer_, NA_integer_)
#   while(current_line >= 1 && is.na(prev_mark[1])) {
#     prev_mark <- stri_locate_last_regex(
#       active_file$contents[current_line],
#       "(\\{\\+\\+|\\{--|\\{~~|\\{==|\\{>>)"
#     )
#     current_line <- current_line - 1
#   }
#
#   next_mark <- c(NA_integer_, NA_integer_)
#   current_line <- active_file$selection[[1]]$range$start[1]
#   while(current_line <= length() && is.na(prev_mark[1])) {
#     prev_mark <- stri_locate_last_regex(
#       active_file$contents[current_line],
#       "(\\{\\+\\+|\\{--|\\{~~|\\{==|\\{>>)"
#     )
#     current_line <- current_line - 1
#   }
# }
#
# cm_reject_current <- function(text, location) {
#
# }
#
# cm_next <- function(text, location) {
#
# }
#

