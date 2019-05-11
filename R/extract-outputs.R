#' @importFrom rmarkdown pandoc_convert
#' @importFrom yaml read_yaml
extract_outputs <- function(docx,
                            track_changes,
                            wrap,
                            verbose = FALSE) {
  md_tmp <- tempfile(fileext = ".md")
  pandoc_convert(normalizePath(docx),
    from = "docx+styles+empty_paragraphs",
    to = "markdown+fenced_code_blocks",
    options = c(
      paste0(
        "--lua-filter=",
        system.file("lua-filters", "extract-outputs.lua", package = "redoc")
      ),
      "--standalone"
    ),
    output = md_tmp,
    verbose = verbose
  )
  yml <- yaml::read_yaml(md_tmp)
  yml
}
