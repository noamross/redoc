#' Is this a reversible document?
#'
#' A function for testing is the file can be un-knit.  If not, un-knitting
#' may be attempted with the `orig_chunkfile` or `orig_docx` files in [undoc()].
#'
#' @param docx A path to a `.docx` file or an `rdocx` object produced by
#' [officer::read_docx()]
#' @return a logical value
#' @export
#' @examples
#' is_redoc(redoc_example_docx())
is_redoc <- function(docx) {
  docx <- to_docx(docx)
  chunkfile <- list.files(docx$package_dir, pattern = "\\.chunks\\.csv$")
  return(as.logical(length(chunkfile)))
}

#' @importFrom officer read_docx
to_docx <- function(docx) {
  if (inherits(docx, "rdocx")) {
    return(docx)
  } else {
    return(read_docx(docx))
  }
}

assert_redoc <- function(docx) {
  if (!is_redoc(docx)) {
    stop(deparse(substitute(docx), " is not a reversible document"))
  }
}

#' Path to an example R Markdown file
#' @export
#' @examples
#' redoc_example_rmd()
redoc_example_rmd <- function() {
  system.file("rmarkdown", "templates", "rdocx_reversible", "skeleton",
              "skeleton.Rmd", package = "redoc")
}

#' Path to an example Revserible Microsoft Word file
#' @export
#' @examples
#' redoc_example_docx()
redoc_example_docx <- function() {
  system.file("rmarkdown", "templates", "rdocx_reversible", "skeleton",
              "skeleton.docx", package = "redoc")
}
