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
