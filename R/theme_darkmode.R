#' Enable Dark Mode Toggling in Shiny
#'
#' Wires up reactive theme switching for dark mode. Call this once in your
#' Shiny server to get a reactive theme that tracks the toggle state.
#' Bootstrap components are styled via CSS custom property overrides injected
#' by \code{\link{my_theme}} — no full theme swap occurs on toggle.
#'
#' @param input The Shiny \code{input} object
#' @param session The Shiny \code{session} object (kept for API compatibility,
#'   not actively used)
#' @param input_id Character. The ID of the \code{bslib::input_dark_mode()}
#'   widget in your UI. Default is \code{"dark_mode"}.
#'
#' @return A list with two reactive elements:
#'   \describe{
#'     \item{\code{mode}}{Reactive returning \code{"dark"} or \code{"light"}}
#'     \item{\code{theme}}{Reactive returning the corresponding
#'       \code{bslib::bs_theme()} object from \code{\link{my_theme}}}
#'   }
#'
#' @details
#' The light theme is compiled at initialization. The dark theme is compiled
#' lazily on first toggle to avoid unnecessary startup cost. Bootstrap
#' components switch instantly via CSS custom property overrides (no
#' \code{session$setCurrentTheme()} call). Pass \code{dm$theme()} to plot
#' functions to make them reactive to the toggle.
#'
#' @section UI Setup:
#' Your UI must include \code{bslib::input_dark_mode(id = "dark_mode")}
#' (or a matching \code{input_id}). Place it in your navbar, sidebar,
#' or wherever makes sense for your layout.
#'
#' @family theme-generators
#' @seealso
#'   \code{\link{my_theme}} for the underlying theme constructor,
#'   \code{\link[bslib]{input_dark_mode}} for the toggle widget
#'
#' @examples
#' \dontrun{
#' library(shiny)
#' library(bslib)
#' library(ggplot2)
#'
#' ui <- page_sidebar(
#'   theme = my_theme(),
#'   title = "Dark Mode Demo",
#'   sidebar = sidebar(
#'     input_dark_mode(id = "dark_mode")
#'   ),
#'   card(
#'     card_header("Branded Plot"),
#'     plotOutput("plot")
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   dm <- use_dark_mode(input, session)
#'
#'   output$plot <- renderPlot({
#'     ggplot(mtcars, aes(mpg, wt, color = factor(cyl))) +
#'       geom_point(size = 3) +
#'       scale_color_luwi_d() +
#'       theme_luwi(theme = dm$theme())
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#'
#' @export
use_dark_mode <- function(input, session, input_id = "dark_mode") {

  # Pre-compute light theme; dark theme is compiled lazily on first toggle
  light_theme <- my_theme("light")
  dark_theme_cache <- NULL

  current_mode <- shiny::reactive({
    if (identical(input[[input_id]], "dark")) "dark" else "light"
  })

  current_theme <- shiny::reactive({
    if (current_mode() == "dark") {
      if (is.null(dark_theme_cache)) {
        dark_theme_cache <<- my_theme("dark")
      }
      dark_theme_cache
    } else {
      light_theme
    }
  })

  # No session$setCurrentTheme() needed — the [data-bs-theme="dark"] CSS block
  # injected by my_theme() handles all Bootstrap components via custom properties.
  # Plot reactivity comes from dm$theme() invalidating render functions.

  list(
    mode  = current_mode,
    theme = current_theme
  )
}

#' Dark Mode CSS for Third-Party Widgets
#'
#' Generates a \code{tags$style()} block that overrides compiled colors in
#' third-party widget stylesheets (bootstrap-datepicker, Shiny input
#' containers) that load separately from the bslib theme. Place this in
#' your UI to ensure these widgets respect dark mode.
#'
#' @return A \code{shiny::tags$head()} element to include in your UI
#'
#' @details
#' Some Shiny widgets (datepicker, checkboxes, radios) load their own CSS
#' files independently of bslib. These files contain compiled hex values
#' that cannot be overridden from within the bslib theme. This function
#' injects a \code{<style>} tag into the page head that loads after all
#' widget CSS, ensuring the dark palette takes effect.
#'
#' @examples
#' \dontrun{
#' ui <- page_sidebar(
#'   theme = my_theme(),
#'   dark_mode_css(),
#'   input_dark_mode(id = "dark_mode"),
#'   sidebar = sidebar(
#'     dateRangeInput("dates", "Date Range:"),
#'     checkboxInput("flag", "Show details")
#'   )
#' )
#' }
#'
#' @export
dark_mode_css <- function() {
  brand_file <- system.file("_brand.yml", package = "luwitemplatedm")
  brand_config <- yaml::read_yaml(brand_file)
  dk <- brand_config[["color-dark"]]

  if (is.null(dk)) return(shiny::tags$head())

  # Helper: hex to "r, g, b"
  hex_to_rgb <- function(hex) {
    rgb <- grDevices::col2rgb(hex)
    paste(rgb[1], rgb[2], rgb[3], sep = ", ")
  }

  css <- paste0('
/* Third-party widget dark mode overrides — injected last to win specificity */

/* Datepicker */
html[data-bs-theme="dark"] .datepicker table tr td.active,
html[data-bs-theme="dark"] .datepicker table tr td.active:hover,
html[data-bs-theme="dark"] .datepicker table tr td.active:focus,
html[data-bs-theme="dark"] .datepicker table tr td.active:active,
html[data-bs-theme="dark"] .datepicker table tr td.active.active,
html[data-bs-theme="dark"] .datepicker table tr td.active.day,
html[data-bs-theme="dark"] .datepicker table tr td.active.highlighted,
html[data-bs-theme="dark"] .datepicker table tr td.active.highlighted:active,
html[data-bs-theme="dark"] .datepicker table tr td.active.highlighted.active,
html[data-bs-theme="dark"] .datepicker table tr td.active.highlighted:hover,
html[data-bs-theme="dark"] .datepicker table tr td.active.highlighted:focus,
html[data-bs-theme="dark"] .datepicker table tr td span.active,
html[data-bs-theme="dark"] .datepicker table tr td span.active:hover,
html[data-bs-theme="dark"] .datepicker table tr td span.active:active,
html[data-bs-theme="dark"] .datepicker table tr td span.active.active {
  background-color: ', dk$primary, ' !important;
  border-color: ', dk$primary, ' !important;
  background-image: none !important;
  color: ', dk$background, ' !important;
}
html[data-bs-theme="dark"] .datepicker table tr td.today,
html[data-bs-theme="dark"] .datepicker table tr td.today:hover {
  background-color: color-mix(in srgb, ', dk$primary, ' 30%, ', dk$background, ' 70%) !important;
  background-image: none !important;
}

/* Shiny checkbox & radio containers */
html[data-bs-theme="dark"] .shiny-input-container .checkbox input:checked,
html[data-bs-theme="dark"] .shiny-input-container .checkbox-inline input:checked,
html[data-bs-theme="dark"] .shiny-input-container .radio input:checked,
html[data-bs-theme="dark"] .shiny-input-container .radio-inline input:checked {
  background-color: ', dk$primary, ' !important;
  border-color: ', dk$primary, ' !important;
  accent-color: ', dk$primary, ' !important;
}
html[data-bs-theme="dark"] .shiny-input-container .checkbox input:focus,
html[data-bs-theme="dark"] .shiny-input-container .radio input:focus {
  border-color: ', dk$primary, ' !important;
  box-shadow: 0 0 0 0.25rem rgba(', hex_to_rgb(dk$primary), ', 0.25) !important;
}
')

  shiny::tags$head(shiny::tags$style(shiny::HTML(css)))
}
