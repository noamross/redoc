#' Is this a reversible document?
#'
#' A function for testing is the file can be de-rendered.  If not, un-knitting
#' may be attempted with the `orig_chunkfile` or `orig_docx` files in [dedoc()].
#'
#' @param docx A path to a `.docx` file or an `rdocx` object produced by
#' [officer::read_docx()]
#' @return a logical value
#' @export
#' @examples
#' is_redoc(redoc_example_docx())
is_redoc <- function(docx) {
  docx <- to_docx(docx)
  codefile <- list.files(file.path(docx$package_dir, "redoc"),
    pattern = "\\codelist\\.yml$"
  )
  return(as.logical(length(codefile)))
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
    stop(deparse(substitute(docx)), " is not a reversible document")
  }
}

#' Files for examples and testing
#'
#' @export
#' @rdname redoc_examples
#' @aliases redoc_examples
#' @examples
#' redoc_example_rmd()
#' redoc_example_docx()
#' redoc_example_edited_docx()
redoc_example_rmd <- function() {
  system.file("examples", "example.Rmd", package = "redoc")
}

#' @export
#' @rdname redoc_examples
#' @aliases redoc_examples
redoc_example_docx <- function() {
  system.file("examples", "example.docx", package = "redoc")
}

#' @export
#' @rdname redoc_examples
#' @aliases redoc_examples
redoc_example_edited_docx <- function() {
  system.file("examples", "example-edited.docx", package = "redoc")
}
