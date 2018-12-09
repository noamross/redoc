context("redoc round-trips")

test_that("Document round-tripping works", {
  rmarkdown::render(redoc_example_rmd(),
    output_dir = getwd(),
    output_file = "skel.docx", quiet = TRUE,
    output_options = list(keep_md = FALSE),
    clean = TRUE
  )
  rdoc <- undoc("skel.docx", overwrite = TRUE)
  odoc <- redoc_extract_rmd("skel.docx", type = "roundtrip", overwrite = TRUE)
  expect_equal(readLines(rdoc), readLines(odoc))
})

unlink(list.files(pattern = ("\\.(docx|Rmd|md)")))
