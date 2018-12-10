#' @importFrom stringi stri_join
oneline <- function(..., collapse = "\n") {
  stri_join(c(...), collapse = collapse)
}

#' @importFrom stringi stri_split_lines1
reline <- function(...) {
  stri_split_lines1(oneline(...))
}

escape_captures <- function(str) {
  stri_replace_all_fixed(str, "$", "\\$")
}

readcsv <- function(x) {
  utils::read.csv(x, stringsAsFactors = FALSE)
}
