
#' Package Load Hook
#'
#' Automatically loads and registers Google Fonts specified in the brand YAML
#' when the package is loaded. This ensures fonts are available for ggplot2
#' themes without requiring manual setup.
#'
#' @param libname Character string giving the library directory where the
#'   package was installed.
#' @param pkgname Character string giving the name of the package.
#'
#' @return NULL (invisibly). Called for side effects (font registration).

.onLoad <- function(libname, pkgname) {
  # Load fonts when package loads
  if (requireNamespace("showtext", quietly = TRUE)) {
    fonts <- get_theme_fonts()
    for (font_family in fonts$all_families) {
      sysfonts::font_add_google(font_family, font_family)
    }
    showtext::showtext_auto()
  }
}
