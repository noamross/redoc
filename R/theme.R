#' Add CriticMarkup highlighting themes to RStudio
#'
#' @param theme Name of the CriticMarkup-enabled theme to apply.  Currently only
#'     "textmate" is available
#' @param apply,force,globally passed to [rstudioapi::addTheme()]
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' \donttest{
#' add_criticmarkup_theme("textmate")
#' }}
add_criticmarkup_theme <- function(theme = "textmate", apply = FALSE,
                                   force = FALSE, globally = FALSE) {
  if (!requireNamespace("rstudioapi")) {
    stop("The 'rstudioapi' package is required for this function")
  }
  theme <- tolower(theme)
  rstudioapi::addTheme(
    system.file("themes", paste0("cm-", theme, ".rstheme"),
                package = "redoc"),
    apply = apply, force = force, globally = globally
  )
}
