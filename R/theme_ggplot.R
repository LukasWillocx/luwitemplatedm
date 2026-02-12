#' Luwi ggplot2 Theme with Brand Styling
#'
#' A clean, publication-ready ggplot2 theme that automatically matches your
#' bslib/Shiny application styling. Built on `theme_minimal()` with transparent
#' backgrounds for seamless integration into dashboards.
#'
#' @param theme A bslib theme object (defaults to my_theme())
#' @param base_size Base font size in points (default: 14). All text elements
#'   scale proportionally from this value
#'
#' @return A ggplot2 theme object that can be added to plots with `+`
#'
#' @family ggplot-themes
#' @seealso
#'   [luwi_ggplotly()] for interactive plotly version,
#'   [scale_color_luwi_d()], [scale_fill_luwi_c()] for matching color scales,
#'   [theme_set()] to apply globally to all plots
#'
#' @examples
#' library(ggplot2)
#'
#' # Basic usage
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point() +
#'   theme_luwi()
#'
#' # With larger text for presentations
#' ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
#'   geom_point(size = 3) +
#'   labs(title = "Iris Measurements") +
#'   theme_luwi(base_size = 16)
#'
#' # Set as default theme for all plots
#' theme_set(theme_luwi())
#'
#' # Customize further
#' ggplot(economics, aes(date, unemploy)) +
#'   geom_line() +
#'   theme_luwi() +
#'   theme(panel.grid.major.x = element_blank())  # Remove vertical grid
#'
#' @export
theme_luwi <- function(theme = my_theme(), base_size = 14) {
  colors <- get_theme_colors(theme)
  fonts <- get_theme_fonts()

  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
      legend.box.background = ggplot2::element_rect(fill = "transparent", color = NA),
      panel.grid.major = ggplot2::element_line(color = ggplot2::alpha(colors$primary, 0.4), linewidth = 0.4,linetype='dashed'),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(
        family = fonts$primary,
        face = "bold",
        size = base_size * 1.3,
        color = colors$body_color
      ),
      plot.subtitle = ggplot2::element_text(
        color = colors$body_color,
        margin = ggplot2::margin(b = 10)
      ),
      axis.text = ggplot2::element_text(color = colors$body_color, family = fonts$primary),
      axis.title = ggplot2::element_text(color = colors$body_color, family = fonts$primary),
      legend.text = ggplot2::element_text(color = colors$body_color, family = fonts$primary),
      legend.title = ggplot2::element_text(color = colors$body_color, family = fonts$primary),
      legend.background = ggplot2::element_rect(fill = "transparent", color = NA),
      legend.key = ggplot2::element_rect(fill = "transparent", color = NA),
      strip.background = ggplot2::element_rect(fill = colors$light, color = NA),
      strip.text = ggplot2::element_text(
        family = fonts$primary,
        face = "bold",
        color = colors$body_color
      )
    )
}
###############################################################################
#' Interactive Plotly with Brand Styling for Shiny
#'
#' Converts a ggplot object to an interactive plotly visualization with brand
#' styling automatically applied. Designed specifically for Shiny dashboards with
#' transparent backgrounds, custom fonts, and branded grid lines.
#'
#' @param p A ggplot2 object to convert
#' @param theme A bslib theme object (defaults to my_theme())
#' @param base_size Base font size in points (default: 14)
#' @param tooltip Which aesthetics to display on hover. Can be a character vector
#'   of aesthetic names (e.g., `c("x", "y")`) or "all" for all aesthetics.
#'   Default is `"y"` to show only y-values
#'
#' @return A plotly htmlwidget ready for Shiny rendering
#'
#' @family ggplot-themes
#' @seealso
#'   [theme_luwi()] for static ggplot version,
#'   [plotly::ggplotly()] for underlying conversion function,
#'   [plotly::config()] for additional plotly configuration
#'
#' @examples
#' library(ggplot2)
#' library(plotly)
#'
#' # Basic interactive plot
#' p <- ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point() +
#'   labs(title = "Fuel Economy vs Weight")
#'
#' luwi_ggplotly(p)
#'
#' # Custom tooltips showing multiple values
#' p <- ggplot(mtcars, aes(mpg, wt, color = factor(cyl))) +
#'   geom_point(size = 3)
#'
#' luwi_ggplotly(p, tooltip = c("x", "y", "color"))
#'
#' # In a Shiny app
#' \dontrun{
#' library(shiny)
#'
#' ui <- page_fluid(
#'   theme = my_theme(),
#'   plotlyOutput("plot")
#' )
#'
#' server <- function(input, output) {
#'   output$plot <- renderPlotly({
#'     p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
#'       geom_point()
#'     luwi_ggplotly(p, tooltip = c("x", "y", "color"))
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#'
#' # Custom hover text
#' p <- ggplot(mtcars, aes(mpg, wt, text = paste("Car:", rownames(mtcars)))) +
#'   geom_point()
#'
#' luwi_ggplotly(p, tooltip = "text")
#'
#' @export
luwi_ggplotly <- function(p, theme = my_theme(), base_size = 14, tooltip = "y") {
  colors <- get_theme_colors(theme)
  fonts <- get_theme_fonts()

  # Apply theme_luwi to the ggplot BEFORE conversion — ggplotly inherits
  # ggplot theme settings and they can override plotly layout() calls
  p <- p + theme_luwi(theme = theme, base_size = base_size)

  # Convert primary color with alpha to rgba format for plotly
  primary_alpha <- grDevices::col2rgb(colors$primary, alpha = TRUE)
  grid_color <- sprintf("rgba(%s, %s, %s, 0.4)",
                        primary_alpha[1],
                        primary_alpha[2],
                        primary_alpha[3])

  plotly::ggplotly(p, tooltip = tooltip) %>%
    plotly::config(displayModeBar = FALSE) %>%
    plotly::layout(
      paper_bgcolor = "transparent",
      plot_bgcolor = "transparent",
      # Global font
      font = list(
        family = fonts$primary,
        color = colors$body_color
      ),
      # Title
      title = list(
        font = list(
          family = fonts$primary,
          size = base_size*1.3,
          color = colors$body_color
        )
      ),
      # X-axis
      xaxis = list(
        titlefont = list(family = fonts$primary, color = colors$body_color),
        tickfont = list(family = fonts$primary, color = colors$body_color),
        gridcolor = grid_color,
        gridwidth = 0.4,
        griddash = "dash",
        showgrid = TRUE,
        zeroline = FALSE
      ),
      # Y-axis
      yaxis = list(
        titlefont = list(family = fonts$primary, color = colors$body_color),
        tickfont = list(family = fonts$primary, color = colors$body_color),
        gridcolor = grid_color,
        gridwidth = 0.4,
        griddash = "dash",
        showgrid = TRUE,
        zeroline = FALSE
      ),
      # Legend
      legend = list(
        font = list(family = fonts$primary, color = colors$body_color)
      ),
      # Hover tooltips
      hoverlabel = list(
        font = list(family = fonts$primary, size = base_size)
      )
    )
}

###############################################################################
#' Continuous Color Scale (Luwi Brand)
#'
#' Apply a sequential gradient color scale for continuous data with brand colors.
#' Convenience wrapper around `scale_color_gradientn()`.
#'
#' @param type `"warm"` , `"cool"` or `"green"`. Default is `"warm"`
#' @param ... Additional arguments passed to [ggplot2::scale_color_gradientn()]
#'   (e.g., `limits`, `breaks`, `na.value`)
#'
#' @return A ggplot2 scale object
#'
#' @family ggplot-scales
#' @seealso
#'   [scale_fill_luwi_c()] for fill aesthetic,
#'   [scale_color_luwi_sequential()] for the underlying palette,
#'   [scale_color_luwi_div()] for diverging data
#'
#' @examples
#' library(ggplot2)
#'
#' # Continuous color scale
#' ggplot(faithfuld, aes(waiting, eruptions, color = density)) +
#'   geom_point() +
#'   scale_color_luwi_c(type = "warm") +
#'   theme_luwi()
#'
#' # Cool palette with custom limits
#' ggplot(diamonds, aes(carat, price, color = depth)) +
#'   geom_point(alpha = 0.3) +
#'   scale_color_luwi_c(type = "cool", limits = c(55, 70)) +
#'   theme_luwi()
#'
#' @export
scale_color_luwi_c <- function(type = "warm", ...) {
  ggplot2::scale_color_gradientn(
    colors = scale_color_luwi_sequential(type = type),
    ...
  )
}

###############################################################################
#' Continuous Fill Scale (Luwi Brand)
#'
#' Apply a sequential gradient fill scale for continuous data with brand colors.
#' Convenience wrapper around `scale_fill_gradientn()`.
#'
#' @param type Palette type: `"warm"` (coral to red), `"cool"` (teal to blue),
#'   or `"green"` (olive to dark). Default is `"warm"`
#' @param ... Additional arguments passed to [ggplot2::scale_fill_gradientn()]
#'   (e.g., `limits`, `breaks`, `na.value`)
#'
#' @return A ggplot2 scale object
#'
#' @family ggplot-scales
#' @seealso
#'   [scale_color_luwi_c()] for color aesthetic,
#'   [scale_color_luwi_sequential()] for the underlying palette,
#'   [scale_fill_luwi_div()] for diverging data
#'
#' @examples
#' library(ggplot2)
#'
#' # Heatmap with warm colors
#' ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
#'   geom_tile() +
#'   scale_fill_luwi_c(type = "warm") +
#'   theme_luwi()
#'
#' # Area chart with green gradient
#' ggplot(economics, aes(date, unemploy)) +
#'   geom_area(fill = scale_color_luwi_sequential(type = "green", n = 1)) +
#'   theme_luwi()
#'
#' @export
scale_fill_luwi_c <- function(type = "warm", ...) {
  ggplot2::scale_fill_gradientn(
    colors = scale_color_luwi_sequential(type = type),
    ...
  )
}

###############################################################################
#' Diverging Color Scale (Luwi Brand)
#'
#' Apply a two-directional gradient color scale for data with a meaningful
#' midpoint. Convenience wrapper around `scale_color_gradientn()` using the
#' cool-to-warm diverging palette.
#'
#' @param ... Additional arguments passed to [ggplot2::scale_color_gradientn()]
#'   (e.g., `limits`, `midpoint`, `breaks`)
#'
#' @return A ggplot2 scale object
#'
#' @details
#' Use this for data where deviations from a central value matter:
#' - Positive/negative change
#' - Above/below average
#' - Correlation coefficients
#' - Temperature anomalies
#'
#' Consider setting `midpoint` to center the gradient on your neutral value.
#'
#' @family ggplot-scales
#' @seealso
#'   [scale_fill_luwi_div()] for fill aesthetic,
#'   [scale_color_luwi_diverging()] for the underlying palette,
#'   [scale_color_gradient2()] for manual midpoint control
#'
#' @examples
#' library(ggplot2)
#'
#' # Correlation matrix
#' cor_data <- reshape2::melt(cor(mtcars))
#' ggplot(cor_data, aes(Var1, Var2, color = value)) +
#'   geom_point(size = 5) +
#'   scale_color_luwi_div(limits = c(-1, 1)) +
#'   theme_luwi()
#'
#' # With custom midpoint
#' ggplot(economics, aes(date, unemploy - mean(unemploy))) +
#'   geom_line() +
#'   scale_color_luwi_div(midpoint = 0) +
#'   theme_luwi()
#'
#' @export
scale_color_luwi_div <- function(...) {
  ggplot2::scale_color_gradientn(
    colors = scale_color_luwi_diverging(),
    ...
  )
}

###############################################################################
#' Diverging Fill Scale (Luwi Brand)
#'
#' Apply a two-directional gradient fill scale for data with a meaningful
#' midpoint. Convenience wrapper around `scale_fill_gradientn()` using the
#' cool-to-warm diverging palette.
#'
#' @param ... Additional arguments passed to [ggplot2::scale_fill_gradientn()]
#'   (e.g., `limits`, `midpoint`, `breaks`)
#'
#' @return A ggplot2 scale object
#'
#' @details
#' Ideal for heatmaps and tiles where deviations from center are meaningful.
#'
#' @family ggplot-scales
#' @seealso
#'   [scale_color_luwi_div()] for color aesthetic,
#'   [scale_color_luwi_diverging()] for the underlying palette
#'
#' @examples
#' library(ggplot2)
#'
#' # Correlation heatmap
#' cor_data <- reshape2::melt(cor(mtcars))
#' ggplot(cor_data, aes(Var1, Var2, fill = value)) +
#'   geom_tile() +
#'   scale_fill_luwi_div(limits = c(-1, 1)) +
#'   theme_luwi()
#'
#' @export
scale_fill_luwi_div <- function(...) {
  ggplot2::scale_fill_gradientn(
    colors = scale_color_luwi_diverging(),
    ...
  )
}

###############################################################################
#' Discrete Color Scale (Luwi Brand)
#'
#' Apply brand colors for categorical/discrete data. Convenience wrapper around
#' `scale_color_manual()` with up to 15 distinct brand colors.
#'
#' @param ... Additional arguments passed to [ggplot2::scale_color_manual()]
#'   (e.g., `breaks`, `labels`, `na.value`)
#'
#' @return A ggplot2 scale object
#'
#' @details
#' Supports up to 15 categories with carefully selected high-contrast colors.
#' For more than 8 categories, consider grouping into "Other" or using a
#' continuous scale instead.
#'
#' @family ggplot-scales
#' @seealso
#'   [scale_fill_luwi_d()] for fill aesthetic,
#'   [scale_color_luwi_discrete()] for the underlying palette
#'
#' @examples
#' library(ggplot2)
#'
#' # Categorical scatter plot
#' ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
#'   geom_point(size = 3) +
#'   scale_color_luwi_d() +
#'   theme_luwi()
#'
#' # Line plot with multiple groups
#' ggplot(economics_long, aes(date, value01, color = variable)) +
#'   geom_line() +
#'   scale_color_luwi_d() +
#'   theme_luwi()
#'
#' @export
scale_color_luwi_d <- function(...) {
  ggplot2::scale_color_manual(
    values = scale_color_luwi_discrete(),
    ...
  )
}

###############################################################################
#' Discrete Fill Scale (Luwi Brand)
#'
#' Apply brand colors for categorical/discrete fill aesthetic. Convenience wrapper
#' around `scale_fill_manual()` with up to 15 distinct brand colors.
#'
#' @param ... Additional arguments passed to [ggplot2::scale_fill_manual()]
#'   (e.g., `breaks`, `labels`, `na.value`)
#'
#' @return A ggplot2 scale object
#'
#' @details
#' Perfect for bar charts, boxplots, and violin plots with categorical groups.
#' Colors are ordered by brand importance: primary, secondary, success, warning, danger.
#'
#' @family ggplot-scales
#' @seealso
#'   [scale_color_luwi_d()] for color aesthetic,
#'   [scale_color_luwi_discrete()] for the underlying palette
#'
#' @examples
#' library(ggplot2)
#'
#' # Bar chart
#' ggplot(mpg, aes(class, fill = class)) +
#'   geom_bar() +
#'   scale_fill_luwi_d() +
#'   theme_luwi()
#'
#' # Boxplot by category
#' ggplot(mpg, aes(class, hwy, fill = class)) +
#'   geom_boxplot() +
#'   scale_fill_luwi_d() +
#'   theme_luwi() +
#'   theme(legend.position = "none")  # Class already on x-axis
#'
#' @export
scale_fill_luwi_d <- function(...) {
  ggplot2::scale_fill_manual(
    values = scale_color_luwi_discrete(),
    ...
  )
}
