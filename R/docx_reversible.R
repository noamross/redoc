#' Convert to a Reversible Microsoft Word Document
#'
#' Format for converting from R Markdown to a Microsoft Word Document that can
#' be reversed using [undoc()] after editing in Word.
#'
#' @param highlight_outputs whether to highlight outputs from chunks and inline
#'   code in the final document
#' @param wrap when round-tripping the document, at what width to wrap the
#'   markdown output? See [undoc()].
#' @param margins page margin size.  Can be a single value or a named
#'   vector with values, `top`, `bottom`, `left`, `right`, `gutter`, `header`,
#'   and `footer`.  If NULL defaults to the reference document.
#' @param ... other parameters passed to [rmarkdown::word_document()]
#' @importFrom rmarkdown output_format word_document
#' @importFrom officer read_docx
#' @importFrom tools file_path_sans_ext
#' @importFrom rmarkdown word_document
#' @importFrom knitr knit_print knit_global opts_chunk
#' @importFrom xfun parse_only
#' @export
rdocx_reversible <- function(highlight_outputs = FALSE, wrap = 80,
                             margins = NULL, ...) {

  out <- word_document(
    md_extensions = c("+fenced_divs", "+bracketed_spans"),
    ...)

  out$knitr <- rmarkdown::knitr_options(
    # Wrap code outputs in spans and divs
    knit_hooks = list(
      # TODO: See if its better to make empty inline output raw openxml
      evaluate.inline = function(code, envir = knit_global()) {
        v = withVisible(eval(parse_only(code), envir = envir))
        if (is.null(v$value) || v$value == "") v$value <- "\uFEFF"
        if (v$visible) knit_print(v$value, inline = TRUE, options = opts_chunk$get())
      },
      inline = function(x) {
        id = paste0("inline-", inline_counter())
        paste0("[", x, "]{custom-style=\"", id, "\"}")},
      chunk = function(x, options) {
        if (isFALSE(options$redoc_include)) {
          # Special output for empty chunks
          # TODO: move empty chunk handler to a lua filter to make more general
          # TODO: test if we need special handling for other no-result chunks
          paste0("```{=openxml}\n<w:p><w:pPr><w:pStyle w:val=\"chunk-",
                 options$label,
                 "\"/><w:rPr><w:vanish/></w:rPr></w:pPr></w:p>\n```")
        } else {
          paste0("::: {custom-style=\"chunk-", options$label, "\"}\n",
                 x,
                 "\n:::")
        }
      }
    ),
    opts_hooks = list(
      include = function(options) {
        if (isFALSE(options$include)) {
          options$include <- TRUE
          options$redoc_include <- FALSE
        }
        options
      }
    )
  )

  # Pre-parse, name inline chunks and save chunk contents to lookup table
  out$pre_knit <- function(input, ...) {
    utils::write.table(parse_rmd_to_df(input),
                       file = paste0(file_path_sans_ext(input), ".chunks.csv"),
                       sep = ",", row.names = FALSE, qmethod = "double")
    inline_counter(reset = TRUE)
    chunk_counter(reset = TRUE)
  }

  out$post_processor <-
    function(metadata, input_file, output_file, clean, verbose) {
      docx <- read_docx(output_file)
      rmd_input <- get(envir = parent.frame(n = 1), "original_input")
      chunkfile <- paste0(file_path_sans_ext(rmd_input), ".chunks.csv")
      tmpd <- tempdir()

      orig_rmd <- file.path(tmpd,
                            paste0(file_path_sans_ext(basename(rmd_input)),
                                   ".original.Rmd"))
      file.copy(rmd_input, orig_rmd)

      roundtrip_rmd <- undoc(
        output_file,
        to = paste0(basename(file_path_sans_ext(rmd_input)), ".roundtrip.Rmd"),
        dir = tmpd, wrap = wrap, overwrite = TRUE,
        orig_chunkfile = chunkfile)

      docx <- embed_file(docx, chunkfile)
      docx <- embed_file(docx, orig_rmd)
      docx <- embed_file(docx, roundtrip_rmd)

      if (highlight_outputs) {
        docx <- highlight_output_styles(docx)
      }

      if (!is.null(margins)) {
        set_body_margins(docx, margins)
      }

      print(docx, output_file)
      if (clean) {
        file.remove(chunkfile)
      }
      return(output_file)
    }
  out
}
