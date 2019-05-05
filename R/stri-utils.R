#' @importFrom stringi stri_join
oneline <- function(..., collapse = "\n") {
  stri_join(c(...), collapse = collapse)
}

#' @importFrom stringi stri_split_lines1
reline <- function(...) {
  stri_split_lines1(oneline(...))
}

#' @importFrom stringi stri_join
brkt <- function(x) {
  stri_join("[[", x, "]]")
}

#' @importFrom stringi stri_join
brktn <- function(x) {
  stri_join("\n[[", x, "]]\n")
}

#' @importFrom stringi stri_join
divwrap <- function(text, id, class = "redoc") {
  stri_join("<div class=\"", class, "\" id=\"", id, "\">\n", text, "\n</div>")
}

#' @importFrom stringi stri_join
spanwrap <- function(text, id, class = "redoc") {
#  stri_join("[\n", text, "\n]{class=\"", class, "\" id=\"", id, "\"}")
  stri_join("<span class=\"", class, "\" id=\"", id, "\">", text, "</span>")

}

#' Get the line number of the first fixed match
#' @noRd
#' @importFrom stringi stri_locate_first_fixed stri_sub stri_replace_all_fixed
#'   stri_count_fixed
stri_lineno_first_fixed <- function(text, pattern) {
  loc <- stri_locate_first_fixed(text, pattern)
  pre <- stri_sub(text, from = 1L, to = loc[,1] - 1)
  as.integer(stri_count_lines(pre))
}

stri_count_lines <- function(text) {
  text <- normalize_newlines(text)
  stri_count_fixed(text, '\n') + 1
}

#' @importFrom stringi stri_locate_all_fixed stri_replace_all_fixed
insert_at_line <- function(text, insertion, line, newline = TRUE) {
  text2 <- normalize_newlines(text)
  if (newline) insertion <- stri_join(insertion, "\n")
  line_locs <- stri_locate_all_fixed(text2, "\n")[[1]][, 1]
  stri_sub(text2, line_locs[line], to = line_locs[line] - 1) <- insertion
  return(text2)
}

get_prior_empty_line_loc <- function(text, line) {
  text <- normalize_newlines(text)
  line_locs <- c(0, stri_locate_all_fixed(text, "\n")[[1]][, 1])
  if (line > length(line_locs)) {
    return(stri_length(text) + 1)
  }
  empty_locs <- rbind(c(0,0), stri_locate_all_regex(text, "(?s)\n\\h?\n")[[1]])
  empty_loc <- max(empty_locs[empty_locs <= line_locs[line]])
  empty_loc
}

#' @importFrom stringi stri_locate_all_fixed stri_replace_all_fixed
#'   stri_locate_all_regex stri_length stri_sub<-
insert_at_prior_empty_line <- function(text, insertion, line) {
  text2 <- normalize_newlines(text)
  ll <- get_prior_empty_line_loc(text2, line)
  if (ll == stri_length(text2) + 1) insertion <- stri_join("\n", insertion)
  stri_sub(text2, ll, ll) <- stri_join(insertion, "\n")
  return(text2)
}

normalize_newlines <- function(text) {
  stri_replace_all_fixed(text, c('\r\n', '\n\r', '\r'), '\n',
                         vectorize_all = FALSE)
}

remove_extra_newlines <- function(text) {
  stri_replace_all_regex(text, '[\n\r]{2,}', '\n\n')
}

last <- function(x) {
  x[length(x)]
}

