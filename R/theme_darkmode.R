#' Enable Dark Mode Toggling in Shiny
#'
#' Wires up reactive theme switching for dark mode. Call this once in your
#' Shiny server to get a reactive theme that tracks the toggle state and
#' automatically updates the page styling.
#'
#' @param input The Shiny \code{input} object
#' @param session The Shiny \code{session} object
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
#' This function pre-computes both the light and dark themes at
#' initialization so that toggling is instant — no Sass recompilation
#' happens on each switch. It also calls
#' \code{session$setCurrentTheme()} to update Bootstrap styling
#' whenever the toggle changes.
#'
#' Pass \code{dm$theme()} to any plot function that accepts a
#' \code{theme} argument (\code{\link{theme_luwi}},
#' \code{\link{luwi_ggplotly}}, \code{\link{get_theme_colors}}, etc.)
#' to ensure plots re-render with the correct palette.
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

  # Pre-compute both themes once — avoids Sass recompilation on every toggle
  light_theme <- my_theme("light")
  dark_theme  <- my_theme("dark")

  current_mode <- shiny::reactive({
    if (identical(input[[input_id]], "dark")) "dark" else "light"
  })

  current_theme <- shiny::reactive({
    if (current_mode() == "dark") dark_theme else light_theme
  })

  # Update the page-level Bootstrap theme when mode changes
  shiny::observe({
    session$setCurrentTheme(current_theme())
  })

  list(
    mode  = current_mode,
    theme = current_theme
  )
}
