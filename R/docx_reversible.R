#' Convert to a Reversible Microsoft Word Document
#'
#' Format for converting from R Markdown to a Microsoft Word Document that can
#' be reversed using [undoc()] after editing in Word.
#'
#' @param highlight_outputs whether to highlight outputs from chunks and inline
#'   code in the final document
#' @param wrap when round-tripping the document, at what width to wrap the
#'   markdown output? See [undoc()].
#' @param margins page margin size.  Can be a single value or a named vector
#'   with values, `top`, `bottom`, `left`, `right`, `gutter`, `header`, and
#'   `footer`.  If NULL defaults to the reference document.
#' @param line_numbers either TRUE or list with any of the arguments `start`,
#'   `by`, `restart`, and `distance`
#' @param comment_author The name to affilliate any Critic Markup tracked
#'   changes with
#' @param keep_md whether to keep the markdown document
#' @param ... other parameters passed to [rmarkdown::word_document()]
#' @importFrom rmarkdown output_format word_document
#' @importFrom officer read_docx
#' @importFrom tools file_path_sans_ext
#' @importFrom rmarkdown word_document
#' @importFrom knitr knit_print knit_global opts_chunk opts_knit
#' @importFrom xfun parse_only
#' @export
rdocx_reversible <- function(highlight_outputs = FALSE, wrap = 80,
                             margins = NULL, line_numbers = NULL,
                             comment_author = NULL, keep_md = FALSE, ...) {
  out <- word_document(...)


  # Pre-parse, name inline chunks and save chunk contents to lookup table
  out$pre_knit <- function(input, ...) {
    chunkfile <- paste0(file_path_sans_ext(input), ".chunks.csv")
    knitr::opts_knit$set(chunkfile = chunkfile)
    utils::write.table(parse_rmd_to_df(input),
                       file = chunkfile,
                       sep = ",", row.names = FALSE, qmethod = "double"
    )
    inline_counter(reset = TRUE)
    chunk_counter(reset = TRUE)
  }

  out$knitr <- rmarkdown::knitr_options(
    # Wrap code outputs in spans and divs
    knit_hooks = list(
      evaluate.inline = function(code, envir = knit_global()) {
        v <- withVisible(eval(parse_only(code), envir = envir))
        if (is.null(v$value) || v$value == "") v$value <- "\uFEFF"
        if (v$visible) {
          knit_print(v$value, inline = TRUE, options = opts_chunk$get())
        }
      },
      inline = function(x) {
        id <- paste0("inline-", inline_counter())
        paste0("[", x, "]{custom-style=\"", id, "\"}")
      },
      chunk = function(x, options) {
        if (isFALSE(options$redoc_include)) x <- ""
        paste0(
          "::: {custom-style=\"chunk-", options$label, "\"}\n",
          x, "\n:::"
        )
      },
      document = function(x) {
        chunkfile <- opts_knit$get("chunkfile")
        x <- preprocess_criticmarkup(x, author = comment_author)
        x <- wrap_yaml(x, readcsv(chunkfile))
        x
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

  md_extensions <- c("+smart", "+fenced_divs", "+bracketed_spans")

  out$pandoc <- rmarkdown::pandoc_options(
    to = "docx",
    from = rmarkdown::from_rmarkdown(extensions = md_extensions),
    args = c(
      "--lua-filter",
      system.file("protect-empty.lua", package = "redoc")
    )
  )

  out$post_processor <-
    function(metadata, input_file, output_file, clean, verbose) {
      docx <- read_docx(output_file)
      rmd_input <- get(envir = parent.frame(n = 1), "original_input")
      chunkfile <- opts_knit$get("chunkfile")
      mdfile <- paste0(basename(file_path_sans_ext(rmd_input)), ".md")

      tmpd <- tempdir()

      orig_rmd <- file.path(
        tmpd, paste0(file_path_sans_ext(basename(rmd_input)), ".original.Rmd")
      )
      orig_md <- file.path(
        tmpd, paste0(file_path_sans_ext(file_path_sans_ext(basename(input_file))), ".md")
      )
      file.copy(rmd_input, orig_rmd)
      file.copy(input_file, orig_md)

      roundtrip_rmd <- undoc(
        output_file,
        to = paste0(basename(file_path_sans_ext(rmd_input)), ".roundtrip.Rmd"),
        dir = tmpd, wrap = wrap, overwrite = TRUE,
        orig_chunkfile = chunkfile
      )

      docx <- embed_file(docx, roundtrip_rmd)
      docx <- embed_file(docx, chunkfile)
      docx <- embed_file(docx, orig_rmd)
      docx <- embed_file(docx, orig_md)

      if (highlight_outputs) {
        docx <- highlight_output_styles(docx)
      }

      # Stuff to go to worded/officedown
      if (!is.null(margins)) {
        set_body_margins(docx, margins)
      }

      if (isTRUE(line_numbers)) {
        set_body_linenumbers(docx)
      } else if (is.list(line_numbers)) {
        do.call(set_body_linenumbers, c(list(x = docx), line_numbers))
      }

      print(docx, output_file)
      if (clean) {
        file.remove(chunkfile)
      }
      return(output_file)
    }
  out$keep_md <- keep_md
  out
}
