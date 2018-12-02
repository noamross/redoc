# This should all move to officer/worded but using this as a playground
#' @importFrom xml2 xml_find_first xml_add_child xml_set_attrs xml_child
set_section_properties <- function(x, section = NULL, properties = NULL) {
  doc_obj <- x$doc_obj$get()
  if (is.null(section)) {
    sec <- xml_find_first(doc_obj, "/w:document/w:body/w:sectPr")
    if (inherits(sec, "xml_missing")) {
      sec <- xml_add_child(xml_find_first(doc_obj, "/w:document/w:body"),
                           "w:sectPr")
    }
  } else {
    sec <- section
  }
  for (i in seq_along(properties)) {
    node_name <- prepend_ns(names(properties)[i])
    property <- xml_child(sec, node_name)
    if (inherits(property, "xml_missing")) {
      property <- xml_add_child(sec, node_name)
    }
    names(properties[[i]]) <- prepend_ns(names(properties[[i]]))
    xml_set_attrs(property, properties[[i]])
  }
  x
}

set_body_margins <- function(x, mar = c(top = 1, bottom = 1, left = 1,
                                        right = 1, gutter = 0, header = 0,
                                        footer = 0)) {
  if (length(mar) == 1 && (is.null(names(mar)) || names(mar) == "all")) {
    mar <- c(top = mar, bottom = mar, left = mar, right = mar,
            gutter = 0, header = 0, footer = 0)
  }
  mar <- structure(as.character(mar * 1440), .Names = names(mar))
  set_section_properties(x, properties = list(pgMar = mar))
  x
}

