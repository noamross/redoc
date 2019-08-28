#' R Markdown format for Reversible Reproducible Word Documents
#'
#' Format for converting from R Markdown to a Microsoft Word Document that can
#' be reversed using [dedoc()] after editing in Word.
#'
#' @param highlight_outputs whether to highlight outputs from chunks and inline
#'   code in the final document
#' @param wrap when round-tripping the document, at what width to wrap the
#'   markdown output? Set the default with `"redoc.wrap"` in `options()`. See [dedoc()].
#' @param margins page margin size.  Can be a single value or a named vector
#'   with values, `top`, `bottom`, `left`, `right`, `gutter`, `header`, and
#'   `footer`.  If NULL defaults to the reference document.
#' @param line_numbers either TRUE or list with any of the arguments `start`,
#'   `by`, `restart`, and `distance`
#' @param comment_author The name to attribute any CriticMarkup tracked
#'   changes to. Defaults to [whoami::fullname()].
#' @param keep_md whether to keep the intermediate markdown document
#' @param wrappers a list of wrapper functions to capture text to protect when
#'   rendering and de-rendering.  See [make_wrapper()].
#' @param diagnostics Whether to embed diagnostic information in the output
#'   file.  If TRUE, this will save session information and the current
#'   pandoc and (if used) RStudio versions inside the Word document for later
#'   bug-checking.
#' @param ... other parameters passed to [rmarkdown::word_document()]
#' @importFrom rmarkdown output_format word_document
#' @importFrom officer read_docx
#' @importFrom tools file_path_sans_ext
#' @importFrom rmarkdown word_document
#' @importFrom knitr knit_print knit_global opts_chunk opts_knit
#' @export
redoc <- function(highlight_outputs = FALSE, wrap = getOption("redoc.wrap", 80),
                  margins = NULL, line_numbers = NULL,
                  comment_author = NULL, keep_md = FALSE,
                  wrappers = list(
                    htmlcommentwrap, latexwrap,
                    rawblockwrap, rawspanwrap,
                    cmwrap, citationwrap
                  ),
                  diagnostics = TRUE,
                  ...) {

  # Make a function to pre-process the Rmd file

  pre_knit <- make_preknitter(wrappers = wrappers)

  md_extensions <- c("+smart", "+fenced_divs", "+bracketed_spans")

  pandoc <- rmarkdown::pandoc_options(
    to = "docx+empty_paragraphs",
    from = rmarkdown::from_rmarkdown(extensions = md_extensions),
    args = c(
      "--lua-filter",
      system.file("lua-filters", "protect-empty.lua",
        package = "redoc"
      ),
      "--eol=lf"
    )
  )

  post_processor <-
    function(metadata, input_file, output_file, clean, verbose) {
      docx <- officer::read_docx(output_file)

      render_env <- get_parent_env_with(c(
        "intermediates", "intermediates_loc",
        "knit_input", "original_input"
      ))

      original_rmd_input <- get("original_input", envir = render_env)
      renv_intermediates <- get("intermediates", envir = render_env)
      renv_intermediates_loc <- get("intermediates_loc", envir = render_env)
      renv_intermediates_dir <- get("intermediates_dir", envir = render_env)

      codefile <- renv_intermediates_loc(
        file_with_meta_ext(basename(original_rmd_input), "codelist", "yml")
      )
      codelist <- read_yaml(codefile)

      embed_files(docx, c(original_rmd_input, renv_intermediates),
        internal_dir = "redoc"
      )

      roundtrip_rmd <- dedoc(
        output_file,
        to = file_with_meta_ext(
          basename(original_rmd_input), "roundtrip", "Rmd"
        ),
        dir = renv_intermediates_dir,
        wrap = wrap,
        overwrite = TRUE,
        orig_codefile = codefile
      )

      add_intermediates(roundtrip_rmd)
      embed_files(docx, roundtrip_rmd, internal_dir = "redoc")

      if (diagnostics) {
        diag_file <- renv_intermediates_loc(
          file_with_meta_ext(basename(original_rmd_input), "diagnostics", "yml")
        )
        write_yaml(get_diagnostics(), diag_file, column.major = FALSE)
        add_intermediates(diag_file)
        embed_files(docx, diag_file, internal_dir = "redoc")
      }

      docx <- hide_output_styles(docx)
      if (highlight_outputs) docx <- highlight_output_styles(docx)


      # Stuff to go to worded/officedown
      if (!is.null(margins)) docx <- set_body_margins(docx, margins)
      if (isTRUE(line_numbers)) {
        set_body_linenumbers(docx)
      } else if (is.list(line_numbers)) {
        do.call(set_body_linenumbers, c(list(x = docx), line_numbers))
      }

      print(docx, output_file)
      return(output_file)
    }
  output_format <- rmarkdown::output_format(
    pandoc = pandoc,
    knitr = rmarkdown::knitr_options(),
    keep_md = keep_md,
    pre_knit = pre_knit,
    post_processor = post_processor,
    base_format = word_document(...)
  )
  output_format
}

get_diagnostics <- function() {
  pandoc_version <- as.character(rmarkdown::pandoc_version())
  session_info <- sessioninfo::session_info()
  if (requireNamespace("rstudioapi") &&
    rstudioapi::isAvailable()) {
    rstudio_info <- rstudioapi::versionInfo()[c("version", "mode")]
  } else {
    rstudio_info <- NULL
  }
  list(
    redoc_version = as.list(
      session_info$packages[session_info$packages$package == "redoc", ]
    ),
    pandoc_version = pandoc_version,
    rstudio_info = rstudio_info,
    session_info = session_info
  )
}
