make_rmd_preprocessor <- function(text_processor,
                                  meta_ext= "preprocessed") {

  preprocessor <- function(input, encoding, intermediates_dir) {

    render_env <- get_parent_env_with("knit_input")
    knit_input <- get("knit_input", envir = render_env)
    intermediates <- get("intermediates", envir = render_env)
    intermediates_loc <- get("intermediates_loc", envir = render_env)

    rmd_text <- readfile(input)

    rmd_text_preprocessed <- text_processor(rmd_text)

    preprocessed_rmd_file <- intermediates_loc(
      file_with_meta_ext(knit_input, meta_ext)
    )
    cat(rmd_text_preprocessed, file = preprocessed_rmd_file)

    assign("knit_input", preprocessed_rmd_file, envir = render_env)
    assign("intermediates", c(intermediates, preprocessed_rmd_file),
           envir = render_env)

  }

  return(preprocessor)
}

# add_date <- function(text) {
#   oneline(text, "`r Sys.Date()`")
# }
# of <- rmarkdown::output_format(
#   knit = rmarkdown::knitr_options(),
#   pandoc = rmarkdown::pandoc_options(to = "html"),
#   pre_knit = make_rmd_preprocessor(add_date, "dateadded"),
#   base_format = rmarkdown::html_document()
#
# )
#
# #undebug(preprocess_rmd)
# #debug(get_render_env)
# rmarkdown::render("x x.Rmd", output_format = of, clean = TRUE,
#                   intermediates_dir = "i_d")
