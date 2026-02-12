#' Create Luwi Branded bslib Theme
#'
#' Generates a Bootstrap 5 theme from your _brand.yml file with optional custom
#' CSS. This is the central theming function that powers all other styling in
#' the package - use it in your Shiny UI to ensure consistent branding across
#' your application.
#'
#' @param mode Character. Either \code{"light"} (default) or \code{"dark"}.
#'   Controls which color palette is used from the \code{color} and
#'   \code{color-dark} sections of \code{_brand.yml}.
#'
#' @return A \code{bslib::bs_theme()} object ready to use in Shiny applications
#'
#' @details
#' This function:
#' 1. Reads your package's \code{_brand.yml} file (colors, fonts, spacing)
#' 2. Creates a Bootstrap 5 theme using \code{\link[bslib]{bs_theme}}
#' 3. If \code{mode = "dark"}, overrides color variables with the
#'    \code{color-dark} palette from the same YAML
#' 4. Applies custom CSS from \code{inst/css/custom.css} if available
#'
#' The returned theme object is used as the default in other package functions
#' like \code{\link{theme_luwi}}, \code{\link{get_theme_colors}}, and
#' \code{\link{luwi_ggplotly}}, ensuring consistent styling across Shiny UI,
#' static plots, and interactive visualizations.
#'
#' @section Dark Mode:
#' Dark mode uses the \code{color-dark} section of \code{_brand.yml} to
#' override Bootstrap Sass variables. This ensures the full cascade of
#' derived variables (RGB decompositions, emphasis colors, subtle backgrounds)
#' is recomputed by the Sass compiler. Use \code{\link{use_dark_mode}} in
#' your Shiny server to enable reactive toggling.
#'
#' @family theme-generators
#' @seealso
#'   \code{\link[bslib]{bs_theme}} for the underlying theme constructor,
#'   \code{\link{get_theme_colors}} to extract colors from the theme,
#'   \code{\link{get_theme_fonts}} to extract fonts from the theme,
#'   \code{\link{theme_luwi}} for ggplot2 theme using these colors/fonts,
#'   \code{\link{use_dark_mode}} for Shiny dark mode integration
#'
#' @examples
#' # Light theme (default)
#' theme <- my_theme()
#'
#' # Dark theme
#' dark_theme <- my_theme("dark")
#'
#' # Use in Shiny app
#' library(shiny)
#' library(bslib)
#'
#' ui <- page_fluid(
#'   theme = my_theme(),
#'   h1("My Branded App"),
#'   p("All Bootstrap components use your brand colors automatically")
#' )
#'
#' \dontrun{
#' # App with dark mode toggle
#' ui <- page_fluid(
#'   theme = my_theme(),
#'   input_dark_mode(id = "dark_mode"),
#'   plotOutput("my_plot")
#' )
#'
#' server <- function(input, output, session) {
#'   dm <- use_dark_mode(input, session)
#'
#'   output$my_plot <- renderPlot({
#'     ggplot(mtcars, aes(mpg, wt)) +
#'       geom_point() +
#'       theme_luwi(theme = dm$theme())
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#'
#' @export
my_theme <- function(mode = c("light", "dark")) {
  mode <- match.arg(mode)

  brand_file <- system.file("_brand.yml", package = "luwitemplatedm")
  if (brand_file == "") {
    stop("_brand.yml not found in package.")
  }

  # Always start from the light brand theme — bslib parses the standard

  # color/typography/theme sections from the YAML
  theme <- bslib::bs_theme(
    version = 5,
    brand = brand_file
  )

  # Read dark palette from YAML once — used for both Sass overrides and CSS generation
  brand_config <- yaml::read_yaml(brand_file)
  dk <- brand_config[["color-dark"]]

  # For dark mode, override Sass variables with the color-dark palette.
  # This triggers a full Sass recompile so all derived Bootstrap variables
  # (--bs-primary-rgb, --bs-*-text-emphasis, etc.) update correctly.
  if (mode == "dark") {
    if (is.null(dk)) {
      warning("No 'color-dark' section in _brand.yml. Returning light theme.")
    } else {
      theme <- bslib::bs_add_variables(
        theme,
        "primary"    = dk$primary,
        "secondary"  = dk$secondary,
        "success"    = dk$success,
        "danger"     = dk$danger,
        "warning"    = dk$warning,
        "info"       = dk$info,
        "light"      = dk$light,
        "dark"       = dk$dark,
        "body-bg"    = dk$background,
        "body-color" = dk$foreground,
        .where = "declarations"
      )
    }
  }

  # Add custom CSS
  css_file <- system.file("css/custom.css", package = "luwitemplatedm")
  if (css_file != "" && file.exists(css_file)) {
    theme <- bslib::bs_add_rules(theme, readLines(css_file))
  } else {
    warning("custom.css not found at: ", css_file)
  }

  # Generate dark mode CSS custom property overrides from YAML so that
  # every var(--bs-primary) etc. in custom.css resolves correctly in dark mode.
  # This keeps _brand.yml as the single source of truth.
  if (!is.null(dk)) {
    # Helper: hex to "r, g, b" string for Bootstrap RGB variables
    hex_to_rgb <- function(hex) {
      rgb <- grDevices::col2rgb(hex)
      paste(rgb[1], rgb[2], rgb[3], sep = ", ")
    }

    dark_css <- paste0(
      '[data-bs-theme="dark"] {\n',
      '  --bs-primary: ', dk$primary, ';\n',
      '  --bs-secondary: ', dk$secondary, ';\n',
      '  --bs-success: ', dk$success, ';\n',
      '  --bs-danger: ', dk$danger, ';\n',
      '  --bs-warning: ', dk$warning, ';\n',
      '  --bs-info: ', dk$info, ';\n',
      '  --bs-light: ', dk$light, ';\n',
      '  --bs-dark: ', dk$dark, ';\n',
      '  --bs-body-bg: ', dk$background, ';\n',
      '  --bs-body-color: ', dk$foreground, ';\n',
      '  --bs-primary-rgb: ', hex_to_rgb(dk$primary), ';\n',
      '  --bs-secondary-rgb: ', hex_to_rgb(dk$secondary), ';\n',
      '  --bs-success-rgb: ', hex_to_rgb(dk$success), ';\n',
      '  --bs-danger-rgb: ', hex_to_rgb(dk$danger), ';\n',
      '  --bs-warning-rgb: ', hex_to_rgb(dk$warning), ';\n',
      '  --bs-info-rgb: ', hex_to_rgb(dk$info), ';\n',
      '  --bs-light-rgb: ', hex_to_rgb(dk$light), ';\n',
      '  --bs-dark-rgb: ', hex_to_rgb(dk$dark), ';\n',
      '  --bs-body-bg-rgb: ', hex_to_rgb(dk$background), ';\n',
      '  --bs-body-color-rgb: ', hex_to_rgb(dk$foreground), ';\n',
      '}\n',
      '[data-bs-theme="dark"] .btn-primary {\n',
      '  --bs-btn-bg: ', dk$primary, ';\n',
      '  --bs-btn-border-color: ', dk$primary, ';\n',
      '  --bs-btn-hover-bg: color-mix(in srgb, ', dk$primary, ' 85%, black);\n',
      '  --bs-btn-hover-border-color: color-mix(in srgb, ', dk$primary, ' 80%, black);\n',
      '  --bs-btn-active-bg: color-mix(in srgb, ', dk$primary, ' 75%, black);\n',
      '  --bs-btn-active-border-color: color-mix(in srgb, ', dk$primary, ' 70%, black);\n',
      '}\n',
      '[data-bs-theme="dark"] .btn-secondary {\n',
      '  --bs-btn-bg: ', dk$secondary, ';\n',
      '  --bs-btn-border-color: ', dk$secondary, ';\n',
      '  --bs-btn-hover-bg: color-mix(in srgb, ', dk$secondary, ' 85%, black);\n',
      '  --bs-btn-hover-border-color: color-mix(in srgb, ', dk$secondary, ' 80%, black);\n',
      '  --bs-btn-active-bg: color-mix(in srgb, ', dk$secondary, ' 75%, black);\n',
      '  --bs-btn-active-border-color: color-mix(in srgb, ', dk$secondary, ' 70%, black);\n',
      '}\n',
      '[data-bs-theme="dark"] .btn-success {\n',
      '  --bs-btn-bg: ', dk$success, ';\n',
      '  --bs-btn-border-color: ', dk$success, ';\n',
      '}\n',
      '[data-bs-theme="dark"] .btn-danger {\n',
      '  --bs-btn-bg: ', dk$danger, ';\n',
      '  --bs-btn-border-color: ', dk$danger, ';\n',
      '}\n',
      '[data-bs-theme="dark"] .btn-warning {\n',
      '  --bs-btn-bg: ', dk$warning, ';\n',
      '  --bs-btn-border-color: ', dk$warning, ';\n',
      '}\n',
      '[data-bs-theme="dark"] .btn-info {\n',
      '  --bs-btn-bg: ', dk$info, ';\n',
      '  --bs-btn-border-color: ', dk$info, ';\n',
      '}'
    )
    theme <- bslib::bs_add_rules(theme, dark_css)
  }

  # Tag with mode so downstream functions (get_theme_colors) can detect it
  attr(theme, "luwi_mode") <- mode

  theme
}
