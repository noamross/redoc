# Functions in this file should probably be migrated to the `officer` package

embed_files <- function(docx, files, internal_dir = NULL) {
  for (file in files) {
    if (!file.exists(file)) next
    if (file.info(file)$isdir) {
      embed_files(docx, list.files(file, full.names = TRUE),
        internal_dir = do.call(file.path,
                               as.list(c(internal_dir, basename(file)))))
    } else {
    embed_file(docx, file, internal_dir = internal_dir)
    }
  }
  return(docx)
}

#' @importFrom mime guess_type
embed_file <- function(docx, file, content_type = guess_type(file),
                       internal_dir = NULL) {
  if (!is.null(internal_dir) &&
      !dir.exists(file.path(docx$package_dir, internal_dir)))
    dir.create(file.path(docx$package_dir, internal_dir), recursive = TRUE)

  file.copy(file,
            to = do.call(file.path,
                         as.list(
                           c(docx$package_dir, internal_dir, basename(file)))
                         ))

  extension <- tools::file_ext(file)
  docx$content_type$add_ext(extension, content_type)
  #docx$content_type$save()

  rel <- docx$doc_obj$relationship()
  new_rid <- sprintf("rId%.0f", rel$get_next_id())
  rel$add(
    id = new_rid,
    type = paste0(
      "http://schemas.openxmlformats.org/officeDocument/2006/relationships/",
      extension
    ),
    target = file.path("..", file.path(internal_dir, basename(file)))
  )
}
