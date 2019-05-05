context("redoc round-trips")
library(diffobj)
# library(redoc)

if (dir.exists("artifacts")) unlink("artifacts", recursive = TRUE)
dir.create("artifacts")
file.copy(redoc_example_rmd(),
  file.path("artifacts", "testdoc.Rmd"),
  overwrite = TRUE
)

test_that("Knitting is successful", {
  rmarkdown::render(
    input = file.path("artifacts", "testdoc.Rmd"),
    output_format = redoc(),
    output_dir = "artifacts",
    intermediates_dir = "artifacts",
    output_file = "testdoc.docx",
    quiet = TRUE,
    clean = FALSE,
  )
  expect_true(file.exists(file.path("artifacts", "testdoc.docx")))
})

test_that("All files are embedded and match", {
  unzip(file.path("artifacts", "testdoc.docx"),
    exdir = file.path("artifacts", "docx_unzipped")
  )
  to_embed <-
    sort(list.files("artifacts", "(md|yml)$",
      include.dirs = FALSE,
      full.names = TRUE
    ))
  embedded <- sort(list.files(
    file.path("artifacts", "docx_unzipped", "redoc"),
    "(md|yml)$",
    include.dirs = FALSE, full.names = TRUE
  ))
  expect_identical(basename(embedded), basename(to_embed))
  for (i in seq_along(embedded)) {
    expect_identical(
      readLines(embedded[i], warn = FALSE),
      readLines(to_embed[i], warn = FALSE)
    )
  }
})

test_that("R Markdown is preserved in the roundtrip", {
  diffpath <- file.path("artifacts", "diffs")
  if (!dir.exists(diffpath)) dir.create(diffpath)
  orig <- file.path("artifacts", "testdoc.Rmd")
  roundtrip <- file.path("artifacts", "testdoc.roundtrip.Rmd")
  drmd <- diffobj::diffFile(orig, roundtrip,
    mode = "sidebyside", context = "auto", format = "html",
    tar.banner = "Original", cur.banner = "Current",
    pager = list(file.path = tempfile(fileext = ".html"))
  )
  cat(as.character(drmd), file = file.path(diffpath, "roundtrip-rmd.html"))
  expect_equal(readLines(orig), readLines(roundtrip))

  orig_ast <- pandoc_ast(orig)
  roundtrip_ast <- pandoc_ast(roundtrip)
  dast <- diffObj(orig_ast, roundtrip_ast,
    mode = "sidebyside", pager = "on", format = "html",
    tar.banner = "Original", cur.banner = "Current"
  )
  cat(as.character(dast),
    file = file.path("artifacts", "diffs", "roundtrip-ast.html")
  )
  expect_equal(orig_ast, roundtrip_ast)
})
