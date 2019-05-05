
<!-- README.md is generated from README.Rmd. Please edit that file -->

# redoc - Reversible Reproducible Documents

<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![Project Status: WIP - Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Build
Status](https://travis-ci.org/noamross/redoc.svg?branch=master)](https://travis-ci.org/noamross/redoc)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/noamross/redoc?branch=master&svg=true)](https://ci.appveyor.com/project/noamross/redoc)
[![codecov](https://codecov.io/gh/noamross/redoc/branch/master/graph/badge.svg)](https://codecov.io/gh/noamross/redoc)
[![CRAN
status](https://www.r-pkg.org/badges/version/redoc)](https://cran.r-project.org/package=redoc)
<!-- badges: end -->

**redoc** is a package to enable a two-way R Markdown Microsoft Word
workflow. It generates Word documents that can be de-rendered back into
R Markdown, retaining edits on the Word document, including tracked
changes.

## Installation

Install the **redoc** package with this command:

``` r
source("https://install-github.me/noamross/redoc")
```

Note that **redoc** requires a recent version of Pandoc (\>= 2.1.2). If
you have RStudio version 1.2 or higher, you should have this by default.

## Basic Usage

**redoc** provides an R Markdown [output
format](https://bookdown.org/yihui/rmarkdown/output-formats.html),
`redoc()`, built on top of `rmarkdown::word_document()`. You will
typically call it via the YAML header in your R Markdown document. You
have the option of highlighting the outputs (both chunk and inline) in
the Word Document.

``` yaml
---
output:
  redoc::redoc
---
```

`redoc()` output resembles typical R Markdown Word output, but has some
key differences:

  - [Critic Markup](http://criticmarkup.com/spec.php#thebasicsyntax)
    edits will be converted to Word tracked changes.
  - By default, parts of the documented generated programmatically will
    be highlighted. (Change this with `highlight_outputs = TRUE`)
  - The original `.Rmd` and all code is stored internally in Word
    document for later retrieval.

Word files that have been created by `redoc()` can be reverted to `.Rmd`
with the `dedoc()` function, *even after they are edited*. `dedoc()`
will return the path of the de-rendered document.

``` r
library(redoc)
print(basename(redoc_example_docx()))
#> [1] "example.docx"
dedoc(redoc_example_docx())
#> [1] "./example.Rmd"
```

If the Word document has tracked changes, `dedoc()` will, by default,
convert these to back to Critic Markup syntax. However, tracked changes
are not necessary. You can view the changes between the original R
Markdown file and the de-rendered one using the `redoc_diff()` function.

``` r
redoc_diff(redoc_example_edited_docx())
```

![](man/figures/readme-diff.png)

More details and features can be found in the vignettes for
[users](https://noamross.github.io/redoc/articles/mixed-workflows-with-redoc.html)
and
[developers](https://noamross.github.io/redoc/articles/redoc-package-design.html)

## RStudio Integration

**redoc** has three RStudio Addins to simplify workflow when working
with R Markdown documents:

  - “Render and Update” renders an R Markdown Document and the updates
    the text after round-tripping in to Word format and back. This helps
    with cleaning up small syntax differences (e.g. white space).
  - “Dedoc to active file” and “Dedoc to new file” de-render a file and
    place the contents in RStudio editor tabs, and also display a diff
    changes in the RStudio viewer.

The package also contains a `dedoc` R Markdown template.

## Contributing

Want have feedback or want to contribute? Great\! Please take a look at
the [contributing
guidelines](https://github.com/noamross/redoc/blob/master/.github/CONTRIBUTING.md)
before filing an issue or pull request.

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/noamross/redoc/blob/master/.github/CODE_OF_CONDUCT.md).
By participating in this project you agree to abide by its terms.
