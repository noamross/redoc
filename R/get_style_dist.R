#' @import xml2
get_style_distribution <- function(docx) {
  docx <- "custom-reference.docx"
  dxml <- read_xml(unz(docx, filename = "word/document.xml"))
  sxml <- read_xml(unz(docx, filename = "word/styles.xml"))
  txml <- read_xml(unz(docx, filename = "word/theme/theme1.xml"))
  st <- xml_find_all(sxml, "/w:styles/w:style")
  all_desc <- data.frame(
    stringsAsFactors = FALSE,
    style_type = xml_attr(st, "type"),
    style_id = xml_attr(st, "styleId"),
    style_name = xml_attr(xml_child(st, "w:name"), "val"),
    based_on = xml_attr(xml_child(st, "w:basedOn"), "val"),
    next_style = xml_attr(xml_child(st, "w:next"), "val"),
    p_space_before = xml_attr(xml_find_first(st, "w:pPr/w:spacing"), "before"),
    p_space_after = xml_attr(xml_find_first(st, "w:pPr/w:spacing"), "after"),
    p_space_line = xml_attr(xml_find_first(st, "w:pPr/w:spacing"), "line"),
    p_space_linerule = xml_attr(xml_find_first(st, "w:pPr/w:spacing"), "lineRule"),
    p_ind_left = xml_attr(xml_find_first(st, "w:pPr/w:ind"), "left"),
    p_ind_right = xml_attr(xml_find_first(st, "w:pPr/w:ind"), "right"),
    p_ind_hanging = xml_attr(xml_find_first(st, "w:pPr/w:ind"), "hanging"),
    p_ind_firstLine = xml_attr(xml_find_first(st, "w:pPr/w:ind"), "firstLine")
  )
}

#' @importFrom stringi stri_c
get_style_property <- function(docx, style_id, sub = c("", "pPr", "rPr"),
                               el, attr = "val") {
  docx <- "custom-reference.docx"
  sxml <- read_xml(unz(docx, filename = "word/styles.xml"))
  xml_find_all(sxml, stri_c("/w:styles/w:style[@w:styleId=\"", style_id, "\"]"))
}
