#' Extract Fonts from Brand Configuration
#'
#' Retrieves font families defined in your _brand.yml file. Returns a structured
#' list with primary and secondary fonts for easy programmatic access in plots
#' and custom themes.
#'
#' @param theme A bslib theme object (defaults to my_theme()).
#'
#' @return Named list with three elements:
#'   \describe{
#'     \item{`primary`}{Main font family (used for body text, labels, etc.)}
#'     \item{`secondary`}{Secondary font family if defined (often used for headings), or `NULL`}
#'     \item{`all_families`}{Character vector of all font families defined in brand file}
#'   }
#'   If _brand.yml is not found, returns fallback values with "sans" as primary font.
#'
#' @details
#' This function reads the `typography.fonts` section from your package's
#' _brand.yml file directly. The first font listed becomes `primary`, the
#' second becomes `secondary`.
#'
#' **Expected _brand.yml structure:**
#' ```yaml
#' typography:
#'   fonts:
#'     - family: "Open Sans"
#'       source: google
#'     - family: "Merriweather"
#'       source: google
#' ```
#'
#' @family theme-utilities
#' @seealso
#'   [get_theme_colors()] for extracting theme colors,
#'   [theme_luwi()] to use these fonts in ggplot2 automatically
#'
#' @examples
#' # Get font configuration
#' fonts <- get_theme_fonts()
#' fonts$primary    # "Open Sans"
#' fonts$secondary  # "Merriweather" or NULL
#'
#' # Use in ggplot2
#' library(ggplot2)
#' fonts <- get_theme_fonts()
#'
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point() +
#'   labs(title = "My Plot") +
#'   theme_minimal(base_family = fonts$primary) +
#'   theme(
#'     plot.title = element_text(family = fonts$secondary %||% fonts$primary)
#'   )
#'
#' # Check what fonts are available
#' fonts <- get_theme_fonts()
#' cat("Available fonts:", paste(fonts$all_families, collapse = ", "))
#'
#' @export
get_theme_fonts <- function(theme = my_theme()) {
  # For bslib brand themes, we need to read the brand file directly
  brand_file <- system.file("_brand.yml", package = "luwitemplatedm")
  if (brand_file != "" && file.exists(brand_file)) {
    brand_config <- yaml::read_yaml(brand_file)
    if (!is.null(brand_config$typography$fonts)) {
      fonts <- brand_config$typography$fonts
      families <- sapply(fonts, function(f) f$family)
      return(list(
        primary = families[1],
        secondary = if (length(families) > 1) families[2] else NULL,
        all_families = families
      ))
    }
  }
  # Fallback
  return(list(
    primary = "sans",
    secondary = NULL,
    all_families = "sans"
  ))
}
