# Functions in this file should probably be migrated to the `officer` package

#' @importFrom mime guess_type
embed_file <- function(docx, file, content_type = guess_type(file)) {
  docx <- to_docx(docx)
  file.copy(file, to = file.path(docx$package_dir, basename(file)))

  extension <- tools::file_ext(file)
  docx$content_type$add_ext(extension, content_type)
  docx$content_type$save()

  rel <- docx$doc_obj$relationship()
  new_rid <- sprintf("rId%.0f", rel$get_next_id())
  rel$add(
    id = new_rid,
    type = paste0(
      "http://schemas.openxmlformats.org/officeDocument/2006/relationships/",
      extension
      ),
    target = file.path("..", basename(file))
  )
  return(docx)
}
