#' Convert an Reversible Document back to R Markdown
#'
#' Converts a document originally created with [redoc()] back to R
#' Markdown, including changes made to text in MS Word.
#'
#' @details R chunks may be lost in the editing process if using non-Microsoft
#'   word processors (e.g. LibreOffice or in copy-and-pasting text into a new document.
#'
#' @param docx The `.docx`` file to convert
#' @param to the filename to write the resulting `.Rmd` file.  The default is to
#'   use the same basename as the docx document
#' @param dir The directory to write the `.Rmd`` to. Defaults to current working
#'   directory
#' @param track_changes How to deal with tracked changes and comments in the
#'   `.docx` file. `"accept"` accepts all changes, and `"reject"` rejects all of
#'   them. `"criticmarkup"`, converts the tracked changes to
#'   [Critic Markup syntax](http://criticmarkup.com/spec.php#thebasicsyntax).
#'   The default, `"comments_only"` will only convert comments, as other changes can be
#'   viewed with [redoc_diff()].
#'   `"all"` marks up tracked changes and comments in `<span>` tags and is
#'   useful for debugging.  See the
#'   [pandoc manual](http://pandoc.org/MANUAL.html#option--track-changes) for
#'   details.
#' @param block_missing,inline_missing What to do about code blocks or inline code
#'  whose output has been removed in the editing of the Word document. "restore"
#'  attempts to restore the code as close to its original location in the document
#'  as possible. "comment" will do so but wrap it in HTML comments. "omit" will
#'  not restore the code at all.
#' @param wrap The width at which to wrap text. If `NA`, text is not wrapped.
#'   Set the default with `"redoc.wrap"` in `options()`.
#' @param overwrite Whether to overwrite an existing file
#' @param orig_codefile,orig_docx The original `.codelist.yml` or Word document
#'   created when the document was first knit.  Useful for debugging, or in
#'   cases where the word file has been corrupted or transformed, for instance,
#'   by copy-and-pasting the content into a new file.  If provided, `dedoc` will
#'   use this codefile or word file to re-create the `.Rmd` file with the text
#'   of the input.
#' @param verbose whether to print pandoc progress text
#' @importFrom rmarkdown pandoc_convert
#' @importFrom yaml read_yaml
#' @importFrom  tools file_path_sans_ext
#' @export
dedoc <- function(docx, to = NULL, dir = ".",
                  track_changes = "comments_only",
                  block_missing = "comment",
                  inline_missing = "omit",
                  wrap = getOption("redoc.wrap", 80), overwrite = FALSE,
                  orig_docx = NULL, orig_codefile = NULL,
                  verbose = FALSE) {
  if (!is_redoc(docx) && is.null(orig_codefile) && is.null(orig_docx)) {
    md_only <- TRUE
    if (verbose) {
      message("Document is not reversible - no internal data on R chunks found.
Returning markdown only. Alternate data may be provided via `orig_codefile or `orig_docx`")
    }
  } else {
    md_only <- FALSE
  }

  stopifnot(track_changes %in%
              c("comments_only", "criticmarkup", "accept", "reject", "all"))
  stopifnot(block_missing %in% c("comment", "omit", "restore"))
  stopifnot(inline_missing %in% c("comment", "omit", "restore"))

  if (is.null(to)) to <- paste0(file_path_sans_ext(basename(docx)), ".Rmd")
  if (is.null(dir)) dir <- "."
  to <- file.path(dir, to)
  if (!overwrite && file.exists(to)) stop(to, " exists and overwrite = FALSE")

  if (!is.null(orig_codefile)) {
    codelist <- read_yaml(orig_codefile)
  } else if (!is.null(orig_docx)) {
    codelist <- redoc_extract_code(orig_docx)
  } else {
    codelist <- redoc_extract_code(docx)
  }

  md <- convert_docx_to_md(docx, track_changes, wrap, verbose, md_only)
  if (!md_only) {
    codelist <- sort_by(codelist, "lineno")
    md <- merge_yaml_headers(md, codelist)
    md <- restore_code(md,
      codelist = list_subset(codelist, type = "block"),
      missing = block_missing
    )
    md <- restore_code(md,
      codelist = list_subset(codelist, type = "inline"),
      missing = inline_missing
    )
    md <- remove_extra_newlines(md)
  }

  cat(md, file = to, sep = "")
  return(to)
}


#' Extract the Rmd used to to produce a Reversible Word Doc
#'
#' Documents produced with [redoc()] store an copy of the original
#' `.Rmd` files used to produce them.  This is useful for diffing against the
#' version created with [dedoc()], especially if tracked changes have not been
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
#' @param overwrite whether to overwrite existing files
#' @export
#' @importFrom stringi stri_subset_fixed stri_subset_regex
#' @return The path to the extracted `.Rmd`
#' @examples
#' redoc_extract_rmd(redoc_example_docx(), dir = tempdir())
redoc_extract_rmd <- function(docx, type = c("original", "roundtrip"),
                              dir = ".", to = NULL, overwrite = FALSE) {
  docx <- to_docx(docx)
  assert_redoc(docx)
  type <- match.arg(type)
  rmdfiles <- list.files(file.path(docx$package_dir, "redoc"),
    pattern = "\\.(r|R)md$",
    full.names = TRUE
  )
  if (type == "original") {
    rmdfile <- stri_subset_regex(rmdfiles,
                                 "(?:\\.preprocessed\\.|\\.roundtrip\\.)",
                                 negate = TRUE)[1]
  } else if (type == "roundtrip") {
    rmdfile <- stri_subset_fixed(rmdfiles, ".roundtrip.")
  }


  if (is.null(to)) to <- basename(rmdfile)
  out <- file.path(dir, to)
  if (file.exists(out) && !overwrite) stop(out, " exists and overwrite=FALSE")
  file.copy(rmdfile, out, overwrite = overwrite)
  return(file.path(dir, to))
}

redoc_extract_code <- function(docx) {
  docx <- to_docx(docx)
  assert_redoc(docx)
  codefile <- list.files(file.path(docx$package_dir, "redoc"),
    pattern = "\\.codelist\\.yml$",
    full.names = TRUE
  )
  codelist <- read_yaml(codefile)
  codelist
}

#' @importFrom stringi stri_replace_first_fixed stri_detect_fixed stri_join
restore_code <- function(md, codelist, missing) {
  if (!length(codelist)) return(md)
  codelist <- sort_by(codelist, "lineno")
  offset <- attr(md, "yaml_offset") %||% 0

  last_detected_end <- offset

  for (item in codelist) {
    marker <- stri_join("<<<", item$name, ">>>")
    marker_line <- stri_lineno_first_fixed(md, marker)
    if (!is.na(marker_line)) {
      last_detected_end <- marker_line + stri_count_lines(item$code)
      md <- stri_replace_first_fixed(md, marker, item$code,
        vectorize_all = FALSE
      )
      md <- stri_replace_all_fixed(md, marker, "")
    } else if (missing != "omit") {
      if (missing == "comment") {
        if (item$type == "inline") {
          restorecode <- stri_join(
            "<!--", item$code, ", originally line ",
            item$lineno, " -->\n"
          )
        } else {
          restorecode <- stri_join(
            "\n<!-- originally line ", item$lineno, "\n",
            item$code, "\n-->\n"
          )
        }
      } else {
        restorecode <- stri_join(item$code, "\n")
      }
      md <- insert_at_prior_empty_line(
        md, restorecode,
        max(last_detected_end, item$lineno + offset)
      )
      offset <- offset + 3
    }
  }
  return(md)
}

#' @importFrom stringi stri_extract_first_regex stri_replace_first_regex
#'   stri_replace_last_fixed
#' @importFrom yaml yaml.load as.yaml
merge_yaml_headers <- function(md, codelist) {
  old_header <- list_subset(codelist, label = "yamlheader")[[1]]
  new_yaml <- stri_extract_first_regex(md, "(?s)\\A\\s*---\\n.*?\\n---\\n")
  if (is.null(old_header) ||
    all(is.na(old_header)) || length(old_header) == 0) {
    attr(md, "yaml_offset") <- stri_count_fixed(new_yaml, "\n") - 1
    return(md)
  }

  old_metadata <- yaml.load(old_header$code)

  if (!is.na(new_yaml)) {
    new_metadata <- yaml.load(new_yaml)
    for (name in names(new_metadata)) {
      old_metadata[[name]] <- new_metadata[[name]]
    }
  }
  merged_yaml <- oneline(
    "---",
    stri_replace_last_fixed(
      as.yaml(old_metadata,
        handlers = NULL
      ),
      "\n", ""
    ),
    "---"
  )
  yaml_offset <- stri_count_lines(merged_yaml) -
    stri_count_lines(old_header$code)

  md <- stri_replace_first_regex(md, "(?s)^\\n*?---\\n.*?\\n---\\n", "")
  md <- oneline(merged_yaml, md)
  attr(md, "yaml_offset") <- yaml_offset
  return(md)
}

# replace_yaml_blocks <- function(md, codelist) {
#   yaml_blocks <- codelist[stri_detect_regex(names(codlist)), "^redoc-yaml-\\d+"]
#   if (!length(yaml_blocks)) return(md)
#   md <- paste(md, collapse = "\n")
#   patterns <- paste0("[[chunk-", codelist$label, "]]")
#   replacements <- paste(codelist$code)
#   detected <- logical(1)
#   prepend <- ""
#   for (i in seq_along(patterns)) {
#     detected <- stri_detect_fixed(md, patterns[i])
#     if (!detected) {
#       prepend <- paste0(c(prepend, replacements[i]), collapse = "\n\n")
#     } else {
#       replacements[i] <-
#         paste0(c(prepend, replacements[i]), collapse = "\n\n")
#       prepend <- ""
#     }
#   }
#   for (i in seq_along(patterns)) {
#     md <- stri_replace_first_fixed(
#       md, patterns[i],
#       replacements[i]
#     )
#     md <- stri_replace_all_fixed(md, patterns[i], "")
#   }
#   if (prepend != "") {
#     md <- paste0(c(md, prepend), collapse = "")
#   }
#   md <- stri_replace_all_regex(md, "\n{3,}", "\n\n")
#   md <- stri_split_lines1(md)
# }


convert_docx_to_md <- function(docx,
                               track_changes,
                               wrap = getOption("redoc.wrap", 80),
                               verbose, md_only) {
  docx <- normalizePath(docx)
  track_changes <- match.arg(track_changes, track_changes)
  if (track_changes == "criticmarkup") {
    track_opts <- c(
      "--track-changes=all",
      paste0(
        "--lua-filter=",
        system.file("lua-filters", "criticmarkup.lua", package = "redoc")
      )
    )
  } else if (track_changes == "comments_only") {
    track_opts <- c(
      "--track-changes=all",
      paste0(
        "--lua-filter=",
        system.file("lua-filters", "criticmarkup-commentsonly.lua",
                    package = "redoc")
      )
    )
  } else {
    track_opts <- paste0("--track-changes=", track_changes)
  }

  if (is.null(wrap)) {
    wrap_opts <- "--wrap=none"
  } else {
    wrap_opts <- c("--wrap=auto", paste0("--columns=", wrap))
  }
  if (!md_only) {
    filter_opts <- c(paste0(
      "--lua-filter=",
      system.file("lua-filters", "revchunks.lua", package = "redoc")
    ))
    from_format <- "docx+styles+empty_paragraphs"
  } else {
    filter_opts <- character(0)
    from_format <- "docx"
  }
  other_opts <- c("--standalone", "--eol=lf")
  opts <- c(filter_opts, track_opts, wrap_opts, other_opts)
  md_tmp <- tempfile(fileext = ".md")
  pandoc_convert(docx,
    from = from_format,
    to = "markdown",
    output = md_tmp,
    options = opts,
    verbose = verbose
  )
  return(readfile(md_tmp))
}
