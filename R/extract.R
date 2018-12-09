#' Convert an Reversible Document back to R Markdown
#'
#' Converts a document originally created with [rdocx_reversible()] back to R
#' Markdown, including changes made to text in MS Word.
#'
#' @details Internal data storing R chunks may be lost in the editing process.
#'   This is known to occur with documents edited with LibreOffice.
#'
#' @param docx The `.docx file to convert`
#' @param to the filename to write the resulting `.Rmd` file.  The default is to
#'   use the same basename as the docx document
#' @param dir The directory to write the `.Rmd`` to. Defaults to current working
#'   directory
#' @param track_changes How to deal with tracked changes and comments in the
#'   `.docx` file. `"accept"` accepts all changes, and `"reject"` rejects all of
#'   them. The default, `"criticmarkup"`, converts the tracked changes to
#'   [Critic Markup syntax](http://criticmarkup.com/spec.php#thebasicsyntax).
#'   `"all"` marks up tracked changes and comments in `<span>` tags.  See the
#'   [pandoc manual](http://pandoc.org/MANUAL.html#option--track-changes) for
#'   details.
#' @param wrap The width at which to wrap text. If `NA`, text is not wrapped
#' @param overwrite Whether to overwrite an existing file
#' @param orig_chunkfile,orig_docx The original chunkfile or Word document
#'   created when the document was first knit.  Useful for debugging, or in
#'   cases where the word file has been corrupted or transformed, for instance
#'   by copy-and-pasting the content into a new file.  If provided, undoc will
#'   use this chunkfile or word file to re-create the `.Rmd` file with the text
#'   of the input.
#' @param verbose whether to print pandoc progress text
#' @importFrom rmarkdown pandoc_convert
#' @importFrom  tools file_path_sans_ext
#' @export
undoc <- function(docx, to = NULL, dir = ".",
                  track_changes = c("criticmarkup", "accept", "reject", "all"),
                  wrap = 80, overwrite = FALSE,
                  orig_chunkfile = NULL, orig_docx = NULL, verbose = FALSE) {
  if (!is_redoc(docx) && is.null(orig_chunkfile) && is.null(orig_docx)) {
    stop("Document is not reversible - no internal data on R chunks found.
Alternate data may be provided via orig_chunkfile or orig_docx")
  }

  if (is.null(to)) to <- paste0(file_path_sans_ext(basename(docx)), ".Rmd")
  to <- file.path(dir, to)
  if (!overwrite && file.exists(to)) stop(to, " exists and overwrite = FALSE")

  if (!is.null(orig_chunkfile)) {
    chunk_df <- utils::read.csv(orig_chunkfile, stringsAsFactors = FALSE)
  } else if (!is.null(orig_docx)) {
    chunk_df <- redoc_extract_chunks(orig_docx)
  } else {
    chunk_df <- redoc_extract_chunks(docx)
  }

  md_lines <- convert_docx_to_md(docx, track_changes, wrap, verbose)
  md_lines <- merge_yaml_headers(md_lines, chunk_df)
  md_lines <- replace_yaml_blocks(md_lines, chunk_df)
  md_lines <- replace_inlines(md_lines, chunk_df)
  md_lines <- replace_chunks(md_lines, chunk_df)

  cat(md_lines, file = to, sep = "\n")

  return(to)
}

#' Extract the Rmd used to to produce a Reversible Word Doc
#'
#' Documents produced with [rdocx_reversible()] store an copy of the original
#' `.Rmd` files used to produce them.  This is useful for diffing against the
#' version created with [undoc()], especially if tracked changes have not been
#' used.
#' @param docx A path to a word file or a an `rdocx` object created with
#'   [officer::read_docx()].
#' @param type One of `"original"` or `"roundtrip"`. `"original"` extracts the
#'   exact document originally knit.  `"roundtrip"` (default) extracts a document
#'   that has been converted to Word and back with no edits in between.  The
#'   latter should be more useful for comparing against edits, as line-wrapping
#'   and placement of no-output chunks should match.
#' @param dir The directory to write the `.Rmd`` to. Defaults to current working
#'   directory
#' @param to the filename to write the resulting `.Rmd` file.  The default is to
#'   use the the original name with either `.orignal.Rmd` or `roundtrip.Rmd`
#'   extensions.
#' @param overwrite whether to overwite existing files
#' @export
#' @return The path to the extracted `.Rmd`
#' @examples
#' redoc_extract_rmd(redoc_example_docx(), dir = tempdir())
redoc_extract_rmd <- function(docx, type = c("original", "roundtrip"),
                              dir = ".", to = NULL, overwrite = FALSE) {
  docx <- to_docx(docx)
  assert_redoc(docx)
  type <- match.arg(type)
  rmdfile <- list.files(docx$package_dir,
    pattern = paste0("\\.", type, "\\.Rmd$"),
    full.names = TRUE
  )
  if (is.null(to)) to <- basename(rmdfile)
  out <- file.path(dir, to)
  if (file.exists(out) && !overwrite) stop(out, " exists and overwrite=FALSE")
  file.copy(rmdfile, out, overwrite = overwrite)
  return(file.path(dir, to))
}

#' @importFrom officer read_docx
redoc_extract_chunks <- function(docx) {
  docx <- to_docx(docx)
  assert_redoc(docx)
  chunkfile <- list.files(docx$package_dir,
    pattern = "\\.chunks\\.csv$",
    full.names = TRUE
  )
  chunk_df <- utils::read.csv(chunkfile, stringsAsFactors = FALSE)
  chunk_df
}

#' @importFrom stringi stri_replace_all_fixed
replace_inlines <- function(md_lines, chunk_df) {
  chunk_df <- chunk_df[chunk_df$type == "inline", ]
  if (nrow(chunk_df)) {
    patterns <- paste0("[[", chunk_df$label, "]]")
    replacements <- paste0("`r ", chunk_df$code, "`")
    md_lines <- stri_replace_all_fixed(md_lines, patterns, replacements,
      vectorize_all = FALSE
    )
  }
  md_lines
}

#' @importFrom stringi stri_replace_all_fixed stri_replace_first_fixed
#'   stri_replace_all_regex stri_split_lines1 stri_detect_fixed
replace_chunks <- function(md_lines, chunk_df) {
  chunk_df <- chunk_df[chunk_df$type == "block", ]
  md_lines <- oneline(md_lines)
  if (nrow(chunk_df)) {
    patterns <- paste0("[[", chunk_df$label, "]]")
    replacements <- paste(chunk_df$header, chunk_df$code, "```", sep = "\n")
    detected <- logical(1)
    append <- ""
    start_append <- ""
    last_detected <- NA_integer_
    for (i in seq_along(patterns)) {
      detected <- stri_detect_fixed(md_lines, patterns[i])
      if (!detected) {
        append <- oneline(append, replacements[i], collapse = "\n\n")
      } else {
        if (is.na(last_detected)) {
          start_append <- append
        } else {
          replacements[last_detected] <-
            oneline(replacements[last_detected], append, collapse = "\n\n")
        }
        last_detected <- i
        append <- ""
      }
    }
    if (start_append != "") {
      has_yaml <- stri_detect_regex(md_lines, "(?s)^---.*---\n")
      if (has_yaml) {
        md_lines <- stri_replace_first_regex(
          md_lines,
          "(?s)(?<!^)(\n+)---\n",
          oneline("$1---\n", escape_captures(start_append), "\n")
        )
      } else {
        md_lines <- oneline(start_append, md_lines)
      }
    }
    for (i in seq_along(patterns)) {
      md_lines <- stri_replace_first_fixed(
        md_lines, patterns[i],
        replacements[i]
      )
      md_lines <- stri_replace_all_fixed(md_lines, patterns[i], "")
    }
    md_lines <- stri_replace_all_regex(md_lines, "\n{3,}", "\n\n")
  }
  reline(md_lines)
}


#' @importFrom stringi stri_extract_first_regex stri_replace_first_regex
#' @importFrom yaml yaml.load as.yaml
merge_yaml_headers <- function(md_lines, chunk_df) {
  chunk_df <- chunk_df[chunk_df$label == "yaml-header", ]
  if (nrow(chunk_df)) {
    md_lines <- paste0(md_lines, collapse = "\n")
    new_yaml <- stri_extract_first_regex(md_lines, "(?s)^---\\n.*\\n---\\n")
    new_metadata <- yaml.load(new_yaml)
    old_metadata <- yaml.load(chunk_df$code)
    for (name in names(new_metadata)) {
      old_metadata[[name]] <- new_metadata[[name]]
    }
    md_lines <- stri_replace_first_regex(md_lines, "(?s)^---\\n.*\\n---\\n", "")
    md_lines <- paste0(
      paste0("---\n", as.yaml(old_metadata), "---\n"),
      md_lines,
      collapse = "\n"
    )
    md_lines <- stri_split_lines1(md_lines)
  }
  md_lines
}

replace_yaml_blocks <- function(md_lines, chunk_df) {
  chunk_df <- chunk_df[chunk_df$type == "yaml" & chunk_df$label != "yaml-header", ]
  if (nrow(chunk_df)) {
    md_lines <- paste(md_lines, collapse = "\n")
    patterns <- paste0("[[chunk-", chunk_df$label, "]]")
    replacements <- paste(chunk_df$code)
    detected <- logical(1)
    prepend <- ""
    for (i in seq_along(patterns)) {
      detected <- stri_detect_fixed(md_lines, patterns[i])
      if (!detected) {
        prepend <- paste0(c(prepend, replacements[i]), collapse = "\n\n")
      } else {
        replacements[i] <-
          paste0(c(prepend, replacements[i]), collapse = "\n\n")
        prepend <- ""
      }
    }
    for (i in seq_along(patterns)) {
      md_lines <- stri_replace_first_fixed(
        md_lines, patterns[i],
        replacements[i]
      )
      md_lines <- stri_replace_all_fixed(md_lines, patterns[i], "")
    }
    if (prepend != "") {
      md_lines <- paste0(c(md_lines, prepend), collapse = "")
    }
    md_lines <- stri_replace_all_regex(md_lines, "\n{3,}", "\n\n")
    md_lines <- stri_split_lines1(md_lines)
  }
  md_lines
}


convert_docx_to_md <- function(docx, track_changes, wrap, verbose) {
  docx <- normalizePath(docx)
  track_changes <- match.arg(track_changes, track_changes)
  if (track_changes == "criticmarkup") {
    track_opts <- c(
      "--track-changes=all",
      paste0(
        "--lua-filter=",
        system.file("criticmarkup.lua", package = "redoc")
      )
    )
  } else {
    track_opts <- paste0("--track-changes=", track_changes)
  }

  if (is.na(wrap)) {
    wrap_opts <- "--wrap=none"
  } else {
    wrap_opts <- c("--wrap=auto", paste0("--columns=", wrap))
  }
  filter_opts <- c(paste0(
    "--lua-filter=",
    system.file("revchunks.lua", package = "redoc")
  ))
  other_opts <- c("--standalone") # note adding metadata args for additional title block elements here might work (possibly as title block variable), though can't control order?
  opts <- c(track_opts, filter_opts, wrap_opts, other_opts)
  md_tmp <- tempfile(fileext = ".md")
  pandoc_convert(docx,
    from = "docx+styles+empty_paragraphs",
    to = "markdown",
    output = md_tmp,
    options = opts,
    verbose = verbose
  )
  return(readLines(md_tmp))
}
