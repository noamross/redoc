#' Make a wrapper function
#'
#' @description Wrapper functions are passed to the `wrapper=` argument of
#' [redoc()] to specify what text in R Markdown files should be
#' captured and restored when de-rendering (in addition to code chunks, inline
#' code, and YAML blocks).  `make_wrapper()` simplifies creation of these
#' wrapper functions and returns a function that can be passed to [redoc()].
#'
#' Wrappers included in the package, all of which are used by default, are:
#'
#' \tabular{llll}{
#'  **Type of markup** \tab **Function**           \tab **Label**        \tab **Type** \cr
#'  HTML Comments      \tab `htmlcommentwrap()`    \tab `"htmlcomment"`  \tab inline   \cr
#'  Single-line LaTeX  \tab `latexwrap()`          \tab `"latex"`        \tab block    \cr
#'  Pandoc citations   \tab `citationwrap()`       \tab `"citation"`     \tab inline   \cr
#'  Raw Blocks         \tab `rawblockwrap()`       \tab `"rawblock"`     \tab block    \cr
#'  Raw Spans          \tab `rawspanwrap()`        \tab `"rawspan"`      \tab inline
#' }
#'
#' @details
#' Some captured text can not be selected by regular expressions, in which case
#' custom functions can be provided to parse out the relevant text. See the vignette "Developing
#' with redoc" for more detail.
#' @param regex A regular expression that identifies the text to be wrapped and
#' restored, including all delimiters.  It will search in a single string with
#' line breaks.  [ICU regular expressions][stringi::stringi-search-regex] are used.
#' The `(?s)` flag is recommended for multi-line expressions.
#' @param label the label to use for chunks of this type, e.g., "citation", "table".
#'    These should be unique for each type of wrapper used. [redoc()]
#'    will throw an error otherwise.
#' @param type whether the text should be treated as inline or block text. Inlines
#'    are wrapped in `span` elements and blocks are wrapped in `divs`.  These are
#'    treated differently in several ways, especially when restoring text in [dedoc()].
#' @return A function of class `redoc_wrapper`
#' @export
#' @importFrom stringi stri_extract_all_regex stri_replace_first_fixed
#' @aliases wrappers
#' @examples
#'
#' rmarkdown::render(
#'     redoc_example_rmd(),
#'     output_format = redoc(
#'                       wrappers = list(
#'                         htmlcommentwrap,
#'                         latexwrap)))
#'
#' # This is how each of these functions are defined in the redoc package
#' htmlcommentwrap <- make_wrapper(
#'     label = "htmlcomment",
#'     regex = "(?s)<!--.*?-->",
#'     type = "inline")
#'
#' latexwrap <- make_wrapper(
#'     label = "latex",
#'     regex = "(?<=[\n\r])\\\\\\w+.*?(?=[\n\r])",
#'     type = "block")
#'
#' rawblockwrap <- make_wrapper(
#'     label = "rawblock",
#'     regex = "(?s)```\\{=\\w+\\}.*?```\\h*",
#'     type = "block")
#'
#' rawspanwrap <- make_wrapper(
#'     label = "rawspan",
#'     regex = "(?s)`[^`]`\\{=\\w+?\\}",
#'     type = "inline")
#'
#' citationwrap <- make_wrapper(
#'     label = "citation",
#'     regex = "(?:@\\w+|\\[.*?-?@\\w+.*?\\](?!\\[\\(\\{))",
#'     type = "inline")

make_wrapper <- function(label, regex, type  = c("block", "inline")) {
  type = match.arg(type)
  if (type == "block")
    container_wrapper <- divwrap
  else if (type == "inline")
    container_wrapper <- spanwrap

  wrapper <- function(rmd) {

    counter <- 0
    chunks <- lapply(
      stri_extract_all_regex(rmd$text, regex)[[1]],
      function(x) {
        counter <<- counter + 1
        list(code = x,
             label = label,
             type = type,
             name = stri_join(prefix, label, "-", counter))
      })

    if (length(chunks) == 0 || (length(chunks) == 1 && any(is.na(chunks[[1]]))))
      return(rmd)

    for (i in seq_along(chunks)) {
      chunks[[i]]$lineno <- stri_lineno_first_fixed(rmd$text, chunks[[i]]$code)
      rmd$text <- stri_replace_first_fixed(rmd$text,
                                           chunks[[i]]$code,
                                           container_wrapper(
                                             chunks[[i]]$code,
                                             chunks[[i]]$name)
                                           )
    }
    rmd$code <- c(rmd$code, chunks)
    rmd
  }

  class(wrapper) <- "redoc_wrapper"
  attr(wrapper, "args") <- list(label = label, regex = regex, type = type)
  return(wrapper)

}


#' @export
#' @usage NULL
#' @rdname make_wrapper
htmlcommentwrap <- make_wrapper(
  label = "htmlcomment",
  regex = "(?s)<!--.*?-->",
  type = "inline")

#' @export
#' @usage NULL
#' @rdname make_wrapper
latexwrap <- make_wrapper(
  label = "latex",
  regex = "(?<=[\n\r])\\\\\\w+.*?(?=[\n\r])",
  type = "block")

#' @export
#' @usage NULL
#' @rdname make_wrapper
rawblockwrap <- make_wrapper(
  label = "rawblock",
  regex = "(?s)```\\{=\\w+\\}.*?```\\h*",
  type = "block")

#' @export
#' @usage NULL
#' @rdname make_wrapper
rawspanwrap <- make_wrapper(
  label = "rawspan",
  regex = "(?s)`[^`]`\\{=\\w+?\\}",
  type = "inline")

#' @export
#' @usage NULL
#' @rdname make_wrapper
citationwrap <- make_wrapper(
  label = "citation",
  regex = "(?:@\\w+|\\[.*?-?@\\w+.*?\\](?!\\[\\(\\{))",
  type = "inline")

