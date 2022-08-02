`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x


subel <- function(x, name) {
  lapply(x, function(x) x[[name]])
}

sort_by <- function(list, by, null_val = NA) {
  vals <- unlist(lapply(list, function(x) x[[by]] %||% null_val))
  list[order(vals)]
}

na_rm <- function(x) {
  x[!is.na(x)]
}

readfile <- function(x) {
  brio::read_file(x)
}

file_with_meta_ext <- function(file, meta_ext, ext = tools::file_ext(file)) {
  paste(tools::file_path_sans_ext(file),
    ".", meta_ext, ".", ext,
    sep = ""
  )
}

get_parent_env_with <- function(var_names) {
  for (frame in rev(sys.frames())[-1]) {
    present <- all(vapply(
      var_names, exists, logical(1),
      envir = frame, inherits = FALSE
    ))
    if (present) return(frame)
  }
  stop(
    "No parent environment found with ",
    paste(var_names, collapse = ", ")
  )
}

add_intermediates <- function(new_intermediates) {
  render_env <- get_parent_env_with(c(
    "intermediates", "intermediates_loc",
    "knit_input"
  ))
  old_intermediates <- get("intermediates", envir = render_env)
  assign("intermediates",
    c(old_intermediates, new_intermediates),
    envir = render_env
  )
}

list_subset <- function(list, ...) {
  filters <- list(...)
  for (i in seq_along(filters)) {
    list <- Filter(list,
      f = function(x) {
        x[[names(filters)[i]]] == filters[[i]]
      }
    )
  }
  return(list)
}

#' Convert a document to Pandoc's abstract syntax tree format
#'
#' This is a convenience function for testing and development.
#' @param file the file to convert using pandoc.
#' @param from the format to convert from.  If `NULL` (default) File type will be
#'   auto-detected by extension. `.Rmd` files will be treated as `.md`.
#' @param tolist whether to return the AST as an R list.  If `FALSE`, will
#'   return length-1 character vector of raw JSON.
#' @export
#' @importFrom rmarkdown pandoc_convert
#' @importFrom jsonlite fromJSON
#' @return A list containing the structured document
#' @examples
#' ast <- pandoc_ast(redoc_example_docx())
pandoc_ast <- function(file, from = NULL, tolist = TRUE) {
  tmp <- tempfile()
  if (is.null(from) && tools::file_ext(file) == "Rmd") from <- "markdown"
  rmarkdown::pandoc_convert(
    input = normalizePath(file),
    to = "json",
    from = from,
    output = tmp
  )
  if (tolist) {
    return(jsonlite::fromJSON(tmp, simplifyVector = FALSE))
  } else {
    return(readfile(tmp))
  }
}

#' @importFrom stringi stri_subset_regex
#' @importFrom utils unzip
get_files_from_zip <- function(zipfile, regex, exdir = ".",
                               junkpaths = TRUE, overwrite = TRUE) {
  files <- unzip(zipfile, list = TRUE)$Name
  files <- stri_subset_regex(files, regex)
  unzip(zipfile, files = files, exdir = exdir, overwrite = overwrite)
  return(file.path(exdir, basename(zipfile)))
}

#' @importFrom stringi stri_subset_regex
#' @importFrom utils unzip
get_con_from_zip <- function(zipfile, regex, open = "",
                             encoding = getOption("encoding")) {
  files <- utils::unzip(zipfile, list = TRUE)$Name
  file <- stri_subset_regex(files, regex)
  if (length(file) != 1L) {
    stop("regex matches ", length(file), " files. Only 1 allowed")
  }
  unz(zipfile, files, open, encoding)
}

singlequote <- function(text) {
  if (stri_sub(text, 1) == "'" && stri_sub(text, -1) == "'") {
    return(text)
  } else {
    text <- stri_c("\"", text, "\"")
    class(text) <- "verbatim"
    return(text)
  }
}
