## Parsers largely lifted from knitr and rmarkdown packages

#' @importFrom knitr all_patterns
#' @importFrom stringi stri_trim_both
parse_rmd_to_df <- function(input_file) {
  lines <- readLines(input_file)

  patterns <- all_patterns$md
  chunk.begin <- patterns$chunk.begin
  chunk.end <- patterns$chunk.end

  yaml <- parse_yaml(lines)

  blks <- grepl(chunk.begin, lines)
  txts <- filter_chunk_end(blks, grepl(chunk.end, lines))
  tmp <- blks | utils::head(c(TRUE, txts), -1)
  groups <- unname(split(lines, cumsum(tmp)))

  chunk_counter(reset = TRUE)
  inline_counter(reset = TRUE)

  chunks <- lapply(groups, function(g) {
    block <- grepl(chunk.begin, g[1])
    if (block) {
      n <- length(g)
      if (n >= 2 && grepl(chunk.end, g[n])) {
        g <- g[-n]
      }
      g <- strip_block(g, patterns$chunk.code)
      params.src <- if (group_pattern(chunk.begin)) {
        stri_trim_both(gsub(chunk.begin, "\\1", g[1]))
      } else {
        ""
      }
      parse_block(g[-1], g[1], params.src)
    }
    else {
      parse_inline(g, patterns)
    }
  })

  chunks <- Filter(Negate(is.null), chunks)

  chunk_df <- do.call(rbind, chunks)
  chunk_df <- rbind(yaml, chunk_df)
  chunk_df$label <- ifelse(chunk_df$type == "block",
    paste0("chunk-", chunk_df$label),
    chunk_df$label
  )
  rownames(chunk_df) <- NULL
  chunk_df
}

filter_chunk_end <- function(chunk.begin, chunk.end) {
  in.chunk <- FALSE
  fun <- function(is.begin, is.end) {
    if (in.chunk && is.end) {
      in.chunk <<- FALSE
      return(TRUE)
    }
    if (!in.chunk && is.begin) {
      in.chunk <<- TRUE
    }
    FALSE
  }
  mapply(fun, chunk.begin, chunk.end)
}

# Possibly extraneous - removes code prefix for latex and other non-md formats
strip_block <- function(x, prefix = NULL) {
  if (!is.null(prefix) && (length(x) > 1)) {
    x[-1L] <- sub(prefix, "", x[-1L])
    spaces <- min(attr(regexpr("^ *", x[-1L]), "match.length"))
    if (spaces > 0) {
      x[-1L] <- substring(x[-1L], spaces + 1)
    }
  }
  x
}

group_pattern <- function(pattern) {
  !is.null(pattern) && grepl("\\(.+\\)", pattern)
}

parse_block <- function(code, header, params.src) {
  params <- params.src
  engine <- "r"
  # if (out_format("markdown")) {
  engine <- sub("^([a-zA-Z0-9_]+).*$", "\\1", params)
  params <- sub("^([a-zA-Z0-9_]+)", "", params)
  #  }
  params <- gsub("^\\s*,*|,*\\s*$", "", params)
  if (tolower(engine) != "r") {
    params <- sprintf("%s, engine=\"%s\"", params, engine)
    params <- gsub("^\\s*,\\s*", "", params)
  }
  params.src <- params
  params <- parse_params(params.src)

  if (nzchar(spaces <- gsub("^([\t >]*).*", "\\1", header))) {
    params$indent <- spaces
    code <- gsub(sprintf("^%s", spaces), "", code)
    code <- gsub(
      sprintf("^%s", gsub("\\s+$", "", spaces)),
      "", code
    )
  }
  code <- paste(code, collapse = "\n")

  label <- params$label

  data.frame(label = label, type = "block", header = header, code = code)
}

#' @importFrom knitr opts_knit
out_format <- function(x) {
  fmt <- opts_knit$get("out.format")
  if (missing(x)) {
    fmt
  } else {
    !is.null(fmt) && (fmt %in% x)
  }
}

parse_params <- function(params) {
  if (params == "") {
    return(list(label = unnamed_chunk()))
  }
  res <- withCallingHandlers(eval(parse_only(paste(
    "alist(",
    quote_label(params), ")"
  ))), error = function(e) {
    message(
      "(*) NOTE: I saw chunk options \"", params,
      "\"\n please go to https://yihui.name/knitr/options",
      "\n (it is likely that you forgot to quote \"character\" options)"
    )
  })
  idx <- which(names(res) == "")
  for (i in idx) if (identical(res[[i]], alist(, )[[1]])) {
      res[[i]] <- NULL
    }
  idx <- if (is.null(names(res)) && length(res) == 1L) {
    1L
  } else {
    which(names(res) == "")
  }
  if ((n <- length(idx)) > 1L || (length(res) > 1L && is.null(names(res)))) {
    stop("invalid chunk options: ", params, "\n(all options must be of the form 'tag=value' except the chunk label)")
  }
  if (is.null(res$label)) {
    if (n == 0L) {
      res$label <- unnamed_chunk()
    } else {
      names(res)[idx] <- "label"
    }
  }
  if (!is.character(res$label)) {
    res$label <- gsub(" ", "", as.character(as.expression(res$label)))
  }
  if (identical(res$label, "")) {
    res$label <- unnamed_chunk()
  }
  res
}

parse_only <- function(code) {
  if (length(code) == 0) {
    return(expression())
  }
  parse(text = code, keep.source = FALSE)
}

quote_label <- function(x) {
  x <- gsub("^\\s*,?", "", x)
  if (grepl("^\\s*[^'\"](,|\\s*$)", x)) {
    x <- gsub("^\\s*([^'\"])(,|\\s*$)", "'\\1'\\2", x)
  }
  else if (grepl("^\\s*[^'\"](,|[^=]*(,|\\s*$))", x)) {
    x <- gsub(
      "^\\s*([^'\"][^=]*)(,|\\s*$)", "'\\1'\\2",
      x
    )
  }
  x
}

#' @importFrom knitr opts_knit
unnamed_chunk <- function(prefix = NULL, i = chunk_counter()) {
  if (is.null(prefix)) {
    prefix <- opts_knit$get("unnamed.chunk.label")
  }
  paste(prefix, i, sep = "-")
}

.counters <- new.env(parent = emptyenv())

chunk_counter <- function(reset = FALSE, init_chunk = 1) {
  if (reset) {
    return(.counters$nc <- init_chunk)
  }
  .counters$nc <- .counters$nc + 1L
  .counters$nc - 1L
}

inline_counter <- function(reset = FALSE, init_inline = 1) {
  if (reset) {
    return(.counters$ni <- init_inline)
  }
  .counters$ni <- .counters$ni + 1L
  .counters$ni - 1L
}

unnamed_inline <- function(prefix = NULL, i = inline_counter()) {
  if (is.null(prefix)) {
    prefix <- "inline"
  }
  paste(prefix, i, sep = "-")
}

#' @importFrom stringi stri_locate_all_regex stri_match_all_regex
parse_inline <- function(input, patterns) {
  inline.code <- patterns$inline.code
  input <- paste(input, collapse = "\n")
  loc <- cbind(start = numeric(0), end = numeric(0))
  if (group_pattern(inline.code)) {
    loc <- stri_locate_all_regex(input, inline.code)[[1]]
  }
  if (nrow(loc) && !all(is.na(loc))) {
    code <- stri_match_all_regex(input, inline.code)[[1L]]
    code <- if (NCOL(code) >= 2L) {
      code[is.na(code)] <- ""
      apply(code[, -1L, drop = FALSE], 1, paste, collapse = "")
    }
  } else {
    return(NULL)
  }
  labels <- character(0)
  for (i in seq_along(code)) {
    labels[i] <- unnamed_inline()
  }
  data.frame(
    label = labels, type = rep("inline", length(code)),
    header = rep(NA_character_, length(code)), code = code,
    stringsAsFactors = FALSE
  )
}

#' @importFrom stringi stri_detect_regex
parse_yaml <- function(lines) {
  yaml.begin = "^---\\s*$"
  yaml.end = "^(---|\\.\\.\\.)\\s*$"
  i <- 1
  n <- length(lines)
  yaml_state <- FALSE
  block_start <- NA_integer_
  block_counter <- 1
  blocks <- list()
  is_start <- TRUE
  while (i <= n) {
    if (!yaml_state) {
      if (stri_detect_regex(lines[i], yaml.begin) &&
          (i != n) &&
          stri_detect_regex(lines[i + 1], "^\\s*$", negate = TRUE)) {
        yaml_state <- TRUE
        block_start <- i
        i <- i + 1
        next
      } else if (is_start) {
        is_start <- stri_detect_regex(lines[i], "^\\s*$")
        i <- i + 1
        next
      } else {
        i <- i + 1
        next
      }
    } else {
      if (stri_detect_regex(lines[i], yaml.end)) {
        blocks <- c(blocks, paste0(lines[block_start:i], collapse = "\n"))
        if (is_start) {
          names(blocks)[block_counter] <- "yaml-header"
          is_start <- FALSE
        } else {
          names(blocks)[block_counter] <- paste0("yaml-", block_counter)
        }
        yaml_state <- FALSE
        block_counter <- block_counter + 1
        i <- i + 1
        next
      } else {
        i <- i + 1
        next
      }
    }
  }
  data.frame(label = names(blocks),
             type = rep("yaml", length(blocks)),
             header = rep(NA_character_, length(blocks)),
             code = unlist(blocks), stringsAsFactors = FALSE)
}
