#' Extract Colors from bslib Theme
#'
#' Retrieves all theme color variables as an R-friendly named list. Useful for
#' programmatically accessing your brand colors or building custom palettes.
#'
#' @param theme A bslib theme object (defaults to my_theme())
#'
#' @return Named list of hex color values with underscore-separated names:
#'   `primary`, `secondary`, `success`, `danger`, `warning`, `info`, `light`,
#'   `dark`, `body_bg`, `body_color`, `input_border_color`
#'
#' @details
#' Color names are converted from CSS variable format (hyphenated) to R-friendly
#' format (underscored). For example, "body-bg" becomes "body_bg".
#'
#' @family color-utilities
#' @seealso [scale_color_luwi_discrete()], [scale_color_luwi_sequential()],
#'   [scale_color_luwi_diverging()] for ready-to-use palettes
#'
#' @examples
#' # Get all theme colors
#' colors <- get_theme_colors()
#' colors$primary  # Your brand primary color
#'
#' # Use in custom visualizations
#' plot(1:10, col = colors$primary, pch = 19)
#'
#' # Build a custom gradient
#' my_gradient <- colorRampPalette(c(colors$light, colors$primary))
#'
#' @export
get_theme_colors <- function(theme = my_theme()) {

  mode <- attr(theme, "luwi_mode") %||% "light"

  if (mode == "dark") {
    # Read dark colors directly from YAML — bs_get_variables doesn't reliably
    # resolve overrides set via bs_add_variables
    brand_file <- system.file("_brand.yml", package = "luwitemplatedm")
    brand_config <- yaml::read_yaml(brand_file)
    dk <- brand_config[["color-dark"]]

    return(list(
      primary            = dk$primary,
      secondary          = dk$secondary,
      success            = dk$success,
      danger             = dk$danger,
      warning            = dk$warning,
      info               = dk$info,
      light              = dk$light,
      dark               = dk$dark,
      body_bg            = dk$background,
      body_color         = dk$foreground,
      input_border_color = dk$foreground
    ))
  }

  # Light mode — original bs_get_variables path
  color_vars <- c(
    "primary", "secondary", "success", "danger", "warning", "info", "light", "dark",
    "body-bg", "body-color",
    "input-border-color"
  )

  colors <- bslib::bs_get_variables(theme, color_vars)
  names(colors) <- gsub("-", "_", names(colors))
  as.list(colors)
}
###############################################################################

#' Sequential Color Palette for Continuous Data
#'
#' Generates a gradient palette suitable for continuous/ordered data. Choose from
#' warm (coral-to-red), cool (teal-to-blue), or green (olive-to-dark) gradients.
#'
#' @param theme A bslib theme object (defaults to my_theme())
#' @param type Palette type:
#'   - `"warm"`
#'   - `"cool"`
#'   - `"green"`
#' @param n Number of colors to generate (default: 9). More colors = smoother gradient
#' @param reverse Logical. Reverse the palette direction (dark to light instead of light to dark)
#'
#' @return Character vector of `n` hex color codes
#'
#' @details
#' These palettes are perceptually uniform and designed to work well in both digital
#' and print contexts. They automatically use your theme's semantic colors as anchor
#' points to ensure brand consistency.
#'
#' @family color-palettes
#' @seealso
#'   [scale_fill_gradient()] and [scale_color_gradient()] to use with ggplot2,
#'   [scale_color_luwi_diverging()] for data with a meaningful midpoint
#'
#' @examples
#' library(ggplot2)
#'
#' # Warm palette for a heatmap
#' ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
#'   geom_tile() +
#'   scale_fill_gradientn(colors = scale_color_luwi_sequential(type = "warm"))
#'
#' # Cool palette reversed (dark = high values)
#' ggplot(economics, aes(date, unemploy)) +
#'   geom_area(fill = scale_color_luwi_sequential(type = "cool", n = 1))
#'
#' # Preview the palette
#' scales::show_col(scale_color_luwi_sequential(type = "green", n = 9))
#'
#' @export
scale_color_luwi_sequential <- function(theme = my_theme(), type = "warm", n = 9, reverse = FALSE) {

  colors <- get_theme_colors(theme)

  palette <- switch(type,
                    "warm" = colorRampPalette(c(colors$light, colors$primary, colors$danger))(n),
                    "cool" = colorRampPalette(c(colors$light, colors$info, colors$secondary))(n),
                    "green" = colorRampPalette(c(colors$light, colors$success, colors$dark))(n),
                    stop("Type must be 'warm', 'cool', or 'green'")
  )

  if (reverse) palette <- rev(palette)
  palette
}
###############################################################################

#' Diverging Color Palette for Data with Midpoint
#'
#' Generates a two-directional gradient palette for data where deviations from a
#' central value are meaningful (e.g., positive/negative change, hot/cold, above/below average).
#'
#' @param theme A bslib theme object (defaults to my_theme())
#' @param n Number of colors to generate (default: 11). Use odd numbers to ensure
#'   a neutral midpoint color
#' @param reverse Logical. Flip the palette direction (warm becomes cool and vice versa)
#'
#' @return Character vector of `n` hex color codes transitioning from cool (teal)
#'   through neutral (light) to warm (coral/red)
#'
#' @details
#' The palette transitions: Teal (cold) → Light blue → Neutral warm → Coral → Red-orange (hot).
#' The neutral midpoint uses your theme's `light` color to maintain brand consistency.
#'
#' **When to use diverging palettes:**
#' - Temperature data (cold to hot)
#' - Financial change (loss to gain)
#' - Survey responses (disagree to agree)
#' - Correlation coefficients (-1 to +1)
#'
#' @family color-palettes
#' @seealso
#'   [scale_fill_gradient2()] to use with ggplot2,
#'   [scale_color_luwi_sequential()] for one-directional data
#'
#' @examples
#' library(ggplot2)
#'
#' # Correlation matrix
#' ggplot(reshape2::melt(cor(mtcars)), aes(Var1, Var2, fill = value)) +
#'   geom_tile() +
#'   scale_fill_gradientn(
#'     colors = scale_color_luwi_diverging(n = 11),
#'     limits = c(-1, 1)
#'   )
#'
#' # Temperature anomalies (reversed: red = hot)
#' temp_palette <- scale_color_luwi_diverging(n = 9, reverse = TRUE)
#'
#' # Preview the palette
#' scales::show_col(scale_color_luwi_diverging())
#'
#' @export
scale_color_luwi_diverging <- function(theme = my_theme(), n = 11, reverse = FALSE) {

  colors <- get_theme_colors(theme)

  # Cool (teal) -> Light -> Warm (coral)
  palette <- colorRampPalette(c(
    colors$secondary,  # Teal (cold)
    colors$info,       # Lighter teal
    colors$light,      # Neutral warm
    colors$primary,    # Coral
    colors$danger      # Red-orange (hot)
  ))(n)

  if (reverse) palette <- rev(palette)
  palette
}

###############################################################################
#' Discrete Color Palette for Categorical Data
#'
#' Provides up to 15 distinct, high-contrast colors for categorical variables like
#' groups, categories, or factors. Colors are carefully selected to be visually
#' distinguishable while maintaining your brand aesthetic.
#'
#' @param theme A bslib theme object (defaults to my_theme())
#' @param n Number of colors needed. If `NULL`, returns all 15 colors. If `n > 15`,
#'   returns 15 colors with a warning
#'
#' @return Character vector of hex color codes (length = `n` or 15 if `n` is NULL)
#'
#' @details
#' The palette prioritizes your brand colors (primary, secondary, success, etc.) for
#' the first few categories, then adds complementary colors for additional categories.
#'
#'
#' @family color-palettes
#' @seealso
#'   [scale_color_manual()] and [scale_fill_manual()] to use with ggplot2,
#'   [scale_color_luwi_sequential()] for many categories (converts to gradient)
#'
#' @examples
#' library(ggplot2)
#'
#' # Basic categorical plot
#' ggplot(mpg, aes(class, hwy, fill = class)) +
#'   geom_boxplot() +
#'   scale_fill_manual(values = scale_color_luwi_discrete())
#'
#' # Specify exact number needed
#' ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
#'   geom_point(size = 3) +
#'   scale_color_manual(values = scale_color_luwi_discrete(n = 3))
#'
#' # Preview all 15 colors
#' scales::show_col(scale_color_luwi_discrete())
#'
#' # Warning example: too many categories
#' scale_color_luwi_discrete(n = 20)  # Returns 15 with warning
#'
#' @export
scale_color_luwi_discrete <- function(theme = my_theme(), n = NULL) {

  colors <- get_theme_colors(theme)

  # Carefully selected palette with good contrast
  # Based on your theme colors but ensuring distinctiveness
  palette <- c(
    colors$primary,
    colors$secondary,
    colors$success,
    colors$warning,
    colors$danger,
    "#a855f7",
    "#ec4899",
    "#06b6d4",
    "#10b981",
    "#f59e0b",
    "#8b5cf6",
    "#14b8a6",
    "#f97316",
    "#84cc16",
    colors$dark
  )

  if (is.null(n)) {
    return(palette)
  }

  if (n > 15) {
    warning("Only 15 distinct colors available. Consider using a gradient for more categories.")
    n <- 15
  }

  palette[1:n]
}
