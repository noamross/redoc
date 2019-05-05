#' Prints the body of Office Open XML generated when converting markdown
#' to docx via pandoc
#'
#' Intended as a developer function for creating docx-generating code.
#' Will usually include some components unneccessary for generating open xml
#' elements, such as bookmarks and `<w:sectPr>` elements
#' @importFrom xml2 read_xml xml_find_first xml_children xml_remove xml_find_all
#' @importFrom rmarkdown pandoc_convert
#' @noRd
md_to_openxml <- function(text, simplify = TRUE,
                          remove_bookmarks = simplify,
                          remove_secs = simplify) {
  tmpf <- tempfile(fileext = ".md")
  cat(text, file = tmpf)
  tmpw <- tempfile(fileext = ".docx")
  pandoc_convert(tmpf, to = "docx", from = "markdown", output = tmpw)
  oml <- (
    xml_find_first(
      read_xml(unz(tmpw, filename = "word/document.xml")),
      "//w:body")
  )
  if (remove_bookmarks) {
    xml_remove(xml_find_all(oml, "//w:bookmarkStart | //w:bookmarkEnd"))
  }
  if (remove_secs) {
    xml_remove(xml_find_all(oml, "//w:sectPr"))
  }

  out <- paste(as.character(xml_children(oml)), collapse = "\n")
  unlink(c(tmpf, tmpw))
  class(out) <- "xmltext"
  out
}

print.xmltext <- function(x) {
  cat(x)
}
