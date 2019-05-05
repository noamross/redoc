rdocx_document2 <- function(mapstyles, ...) {

  redoc_format <- redoc::redoc(...)

  if( missing(mapstyles) )
    mapstyles <- list()

  pre_processor = function(metadata, input_file, runtime, knit_meta, files_dir, output_dir){
    md <- readLines(input_file)
    md <- officedown:::chunk_macro(md)
    md <- officedown:::block_macro(md)
    writeLines(md, input_file)
  }

  post_processor <- function(metadata, input_file, output_file, clean, verbose) {
    x <- officer::read_docx(output_file)
    x <- officedown:::process_images(x)
    x <- officedown:::process_links(x)
    x <- officedown:::process_embedded_docx(x)
    x <- officedown:::process_chunk_style(x)
    x <- officedown:::process_sections(x)
    x <- officedown:::process_par_settings(x)
    x <- officer::change_styles(x, mapstyles = mapstyles)

    print(x, target = output_file)
    output_file
  }


  output_format <- rmarkdown::output_format(
    knitr = redoc_format$knitr,
    pandoc = redoc_format$pandoc,
    keep_md = redoc_format$keep_md,
    clean_supporting = redoc_format$clean_supporting,
    pre_processor = pre_processor,
    post_processor = post_processor,
    base_format = redoc_format
  )

  output_format
}
