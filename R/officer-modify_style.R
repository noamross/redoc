# Functions in this file should probably eventually be moved to the `officer`
# package once they are made more general

#' @importFrom xml2 read_xml xml_find_first xml_add_child xml_set_attrs
#'   write_xml
add_to_style <- function(docx, style_id, name, attrs = NULL) {
  docx <- to_docx(docx)
  name <- prepend_ns(name)
  if (!is.null(attrs)) names(attrs) <- prepend_ns(names(attrs))
  styles_path <- file.path(docx$package_dir, "word", "styles.xml")
  styles_xml <- read_xml(styles_path)
  style_xml <- xml_find_first(
    styles_xml,
    paste0("//w:style[@w:styleId='", style_id, "']")
  )
  rPr <- xml_add_child(style_xml, "w:rPr")
  pPr <- xml_add_child(style_xml, "w:pPr")
  style <- xml_add_child(rPr, name)
  if (!is.null(attrs)) xml_set_attrs(style, attrs)
  style <- xml_add_child(pPr, name)
  if (!is.null(attrs)) xml_set_attrs(style, attrs)
  write_xml(styles_xml, styles_path)
  return(docx)
}

#' @importFrom officer styles_info
#' @importFrom stringi stri_detect_regex
highlight_output_styles <- function(docx, name = "shd",
                                    attrs = c(
                                      val = "clear",
                                      color = "auto",
                                      fill = "FFBEBF"
                                    )) {
  docx <- to_docx(docx)
  styles <- styles_info(docx)
  styles <-
    styles$style_id[stri_detect_regex(styles$style_id, "^(inline|chunk)-")]
  lapply(styles, function(s) {
    add_to_style(docx, s, name = name, attrs = attrs)
    add_to_style(docx, s, name = "hidden")
  })
  return(docx)
}

#' @importFrom stringi stri_detect_regex
prepend_ns <- function(x, ns = "w") {
  ifelse(
    stri_detect_regex(x, paste0("^", ns, ":")),
    x,
    paste0(ns, ":", x)
  )
}
