#' Wrap and store code for un-knitting
#'
#' @return list with elements `text` and `code`.  `text` the text provided with has code elements
#'   wrapped in names spans and divs with unique ids. `code` is a list of those code chunks, with ids
#'   as names.
#'   with names of the form `redoc-type-number`
#' @param rmd_text R Markdown text as a length-1 character vector
#' @param wrappers a list of functions to further process the text.  They will
#' be passed a list of `text` and `code` and should return the same with
#' text processed and additional elements appended to `code`. R code and YAML are temporarily replaced with
#' `[[redoc-type-number]]` so additional wrappers don't mangle them. Chunks in text should generally we wrapped in
#' Pandoc spans and divs with attribute `custom-style="redoc-TYPE-NUMBER`.
#' @noRd
wrap_code <- function(text, wrappers = list()) {
  rmd <- list(text = text, code = list())

  rmd <- hide_chunks(rmd)
  rmd <- hide_inlines(rmd)
  rmd <- hide_yaml(rmd)

  for (wrapper in wrappers) {
    rmd <- wrapper(rmd)
  }

  rmd <- unhide_yaml(rmd)
  rmd <- unhide_inlines(rmd)
  rmd <- unhide_chunks(rmd)

  rmd$code <- sort_by(rmd$code, "lineno")
  names(rmd$code) <- unlist(subel(rmd$code, "name"))

  class(rmd$code) <- "codelist"
  return(rmd)
}

prefix <- "redoc-"

#' @importFrom stringi stri_extract_all_regex
hide_chunks <- function(rmd) {
  chunk_regex <- "(?sx)
  (?<=(^|\n))
    (?:
      [\t >]*```+\\h*\\{.*?\\}.*?[\t >]*```+\\h* |
      (^|\n)\\h*<<[^\\}](.+)[^\\{]>>\\h*(\n|$)
    )
  (?=(\n|$))"

  label = "codechunk"

  counter <- 0
  chunks <- lapply(
    stri_extract_all_regex(rmd$text, chunk_regex)[[1]],
    function(x) {
      counter <<- counter + 1
      list(code = x,
           label = label,
           type = "block",
           name = stri_join(prefix, label, "-", counter))
    })
  for (i in seq_along(chunks)) {
    chunks[[i]]$lineno <- stri_lineno_first_fixed(rmd$text, chunks[[i]]$code)
    rmd$text <- stri_replace_first_fixed(rmd$text,
                                         chunks[[i]]$code,
                                         brkt(chunks[[i]]$name))
  }
  rmd$code <- c(rmd$code, chunks)
  rmd
}

hide_inlines <- function(rmd) {
  inline_regex <- "(?<!(^|\n)``)`r[ #](?:[^`]+)\\s*`"
  label = "inlinecode"

  counter <- 0
  inlines <- lapply(
    stri_extract_all_regex(rmd$text, inline_regex)[[1]],
    function(x) {
      counter <<- counter + 1
      list(code = x,
           label = label,
           type = "inline",
           name = stri_join(prefix, label, "-", counter))
    })

  for (i in seq_along(inlines)) {
    inlines[[i]]$lineno <- stri_lineno_first_fixed(rmd$text, inlines[[i]]$code)
    rmd$text <- stri_replace_first_fixed(rmd$text,
                                         inlines[[i]]$code,
                                         brkt(inlines[[i]]$name))
  }
  rmd$code <- c(rmd$code, inlines)
  rmd
}


#' @importFrom stringi stri_detect_regex
hide_yaml <- function(rmd) {
  yaml.begin = "^---\\h*$"
  yaml.end = "^(---|\\.\\.\\.)\\h*$"
  lines <- reline(rmd$text)

  yamls <- list()
  yaml_header <- NULL
  current_yaml <- NULL
  in_yaml <- FALSE
  at_start <- TRUE
  for (i in seq_along(lines)) {
    if (!in_yaml) {
      if (stri_detect_regex(lines[i], yaml.begin)) {
        in_yaml <- TRUE
        current_yaml <- c(current_yaml, lines[i])
      }
      if (at_start & !in_yaml) {
        if (stri_detect_regex(lines[i], "^\\h*$", negate = TRUE)) {
          at_start <- FALSE
        }
      }
    } else if (in_yaml) {
      if (stri_detect_regex(lines[i], yaml.end)) {
        in_yaml <- FALSE
        current_yaml <- c(current_yaml, lines[i])
        yaml_block <- stri_join(unlist(current_yaml), collapse = "\n")
        if (at_start) {
          yaml_header <- yaml_block
          at_start <- FALSE
        } else {
          yamls <- c(yamls, list(yaml_block))
        }
        current_yaml <- list()
        next
      } else {
        current_yaml <- c(current_yaml, lines[i])
      }
    }
  }
  label = "yaml"

  counter <- 0
  yamls <- lapply(yamls, function(x) {
    counter <<- counter + 1
    list(code = x,
         label = label,
         type = "block",
         name = stri_join(prefix, label, "-", counter))
  })
  if (!is.null(yaml_header)) {
    yaml_header <- list(list(code = yaml_header,
                             label = "yamlheader",
                             type = "header",
                             name = stri_join(prefix, "yamlheader")
    ))
    yamls <- c(yamls, yaml_header)
  }


  for (i in seq_along(yamls)) {
    yamls[[i]]$lineno <- stri_lineno_first_fixed(rmd$text, yamls[[i]]$code)
    rmd$text <- stri_replace_first_fixed(rmd$text,
                                         yamls[[i]]$code,
                                         brkt(yamls[[i]]$name))
  }
  rmd$code <- c(rmd$code, yamls)
  rmd
}



#' @importFrom stringi stri_detect_fixed stri_replace_all_fixed
unhide_yaml <- function(rmd) {

  yamls <- list_subset(rmd$code, label = "yaml")
  if (length(yamls)) {
    rmd$text <- stri_replace_all_fixed(rmd$text,
                                       brkt(subel(yamls, "name")),
                                       divwrap(subel(yamls, "code"), subel(yamls, "name")),
                                       vectorize_all = FALSE)
  }

  yaml_header <- list_subset(rmd$code, label = "yamlheader")
  rmd$text <- stri_replace_first_fixed(rmd$text,
                                       brkt(subel(yaml_header, "name")),
                                       subel(yaml_header, "code"))
  rmd
}

#' @importFrom stringi stri_detect_fixed stri_replace_all_fixed
unhide_inlines <- function(rmd) {
  inlines <- list_subset(rmd$code,  label = "inlinecode")
  rmd$text <- stri_replace_all_fixed(rmd$text,
                                     brkt(subel(inlines, "name")),
                                     spanwrap(subel(inlines, "code"),
                                              subel(inlines, "name")),
                                     vectorize_all = FALSE)
  rmd
}

#' @importFrom stringi stri_detect_fixed stri_replace_all_fixed
unhide_chunks <- function(rmd) {
  chunks <- list_subset(rmd$code,  label = "codechunk")
  rmd$text <- stri_replace_all_fixed(rmd$text,
                                     brkt(subel(chunks, "name")),
                                     divwrap(subel(chunks, "code"),
                                             subel(chunks, "name")),
                                     vectorize_all = FALSE)
  rmd
}
