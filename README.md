
<!-- README.md is generated from README.Rmd. Please edit that file -->

# redoc - reversible R Markdown/MS Word documents.

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

**redoc** is an experimental package to enable a two-way R-Markdown ⟷
Microsoft Word workflow. It is in early design phase; *I will certainly
break things, intentionally and accidentally.* Testing and feedback is
welcome\! Please look at
[CONTRIBUTING.md](https://github.com/noamross/redoc/blob/master/.github/CONTRIBUTING.md),
(especially the
[roadmap](https://github.com/noamross/redoc/blob/master/.github/CONTRIBUTING.md#roadmap))
and the [design
vignette](https://noamross.github.io/redoc/articles/redoc-package-design.html)
if you are interested in development.

## Installation

Install the **redoc** package with this command:

``` r
source("https://install-github.me/noamross/redoc")
```

## Usage

**redoc** provides an R Markdown [output
format](https://bookdown.org/yihui/rmarkdown/output-formats.html) of
`rdocx_reversible()`, built on top of `rmarkdown::word_document()`. You
will typically call it via the YAML header in your R Markdown document.
You have the option of highlighting the outputs (both chunk and inline)
in the Word Document.

``` yaml
---
output:
  redoc::rdocx_reversible:
    keep_md: TRUE
    highlight_outputs: TRUE
---
```

[Critic Markup](http://criticmarkup.com/spec.php#thebasicsyntax) edits
will be converted to Word tracked changes. There are a few other
formatting options available, too.

Word files that have been created by `rdocx_reversible()` (“redocs”) can
be reverted to `.Rmd` with `undoc()`, *even after they are edited*.

``` r
library(redoc)
undoc(redoc_example_docx())
#> [1] "./skeleton.Rmd"
```

If the Word document has tracked changes, `undoc()` will, by default,
convert these to back to Critic Markup syntax.

Undoc’ing a redoc where chunk outputs have been deleted will restore the
original code chunks to the document, usually immediately after the
previous chunk. If chunk outputs are moved, code chunks move with them.
Inline code outputs that are deleted are not restored - it behooves one
to only use inline outputs to print outputs, rather than change the
state for upcoming chunks.

Redocs also store the original `.Rmd` used to make them internally,
which can be extracted and used to diff against the original.

``` r
redoc_extract_rmd(redoc_example_docx())
#> [1] "./skeleton.original.Rmd"
```

## Contributing

Want have feedback or want to contribute? Great\! Please take a look at
the [contributing
guidelines](https://github.com/noamross/redoc/blob/master/.github/CONTRIBUTING.md)
before filing an issue or pull request.

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/noamross/redoc/blob/master/.github/CODE_OF_CONDUCT.md).
By participating in this project you agree to abide by its terms.
