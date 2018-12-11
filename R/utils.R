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

readfile <- function(x) {
  readChar(x, file.info(x)$size)
}

file_with_meta_ext <- function(file, meta_ext, ext = tools::file_ext(file)) {
  paste(tools::file_path_sans_ext(file),
        ".", meta_ext, ".", ext, sep = "")
}

file_with_ext <- function(file, ext) {
  paste(tools::file_path_sans_ext(file), ".", ext, sep = "")
}

get_parent_env_with <- function(var_name) {
  for (frame in rev(sys.frames())[-1]) {
    if (exists(var_name, envir = frame, inherits = FALSE))
      return(frame)
  }
  stop("No parent environment found with \"", var_name, "\"")
}
