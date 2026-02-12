# Vertical Sidebar Navigation App Template
library(luwitemplatedm)
library(bslib)
library(ggplot2)
library(plotly)


# UI
ui <- bslib::page_sidebar(
  title = tags$div(
    class = "d-flex w-100 justify-content-between align-items-center",
    span("My Report Application", class = "navbar-brand"),
    input_dark_mode(id = "dark_mode")
  ),
  theme = my_theme(),
  # Vertical navigation sidebar
  sidebar = sidebar(
    title = "test",
    open = "always",
    navset_pill_list(
      id = "sidebar_nav",
      nav_panel("Step 1: Overview", value = "step1"),
      nav_panel("Step 2: Analysis", value = "step2")
    )
  ),
  # Main content area
  navset_hidden(
    id = "main_content",
    # Tab 1
    nav_panel_hidden(
      value = "step1",
      layout_columns(
        col_widths = c(6, 6, 6, 6),
        row_heights = c("calc(50vh - 75px)", "calc(50vh - 75px)"),
        card(
          card_header("Scatter Plot (ggplot2)"),
          card_body(plotOutput("scatter_plot"))
        ),
        card(
          card_header("Bar Chart (ggplot2)"),
          card_body(plotOutput("bar_plot"))
        ),
        card(
          card_header("Interactive Scatter (plotly)"),
          card_body(plotly::plotlyOutput("plotly_scatter"))
        ),
        card(
          card_header("Card 4"),
          card_body(),
          card_footer("footer")
        )
      )
    ),
    # Tab 2
    nav_panel_hidden(
      value = "step2",
      layout_columns(
        col_widths = c(6, 6, 6, 6),
        row_heights = c("calc(50vh - 75px)", "calc(50vh - 75px)"),
        card(
          card_header("Card 5"),
          card_body()
        ),
        card(
          card_header("Card 6"),
          card_body()
        ),
        card(
          card_header("Card 7"),
          card_body()
        ),
        card(
          card_header("Card 8"),
          card_body()
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Dark mode reactive — pre-computes both themes, syncs Bootstrap on toggle
  dm <- use_dark_mode(input, session)

  # Sync sidebar navigation with main content
  observeEvent(input$sidebar_nav, {
    nav_select("main_content", selected = input$sidebar_nav)
  })

  # ggplot2 scatter — reacts to dark mode toggle
  output$scatter_plot <- renderPlot({
    ggplot(mtcars, aes(mpg, wt, color = factor(cyl))) +
      geom_point(size = 3) +
      labs(title = "Fuel Economy vs Weight", color = "Cylinders") +
      scale_color_luwi_d() +
      theme_luwi(theme = dm$theme())
  },bg='transparent')

  # ggplot2 bar chart — reacts to dark mode toggle
  output$bar_plot <- renderPlot({
    ggplot(mpg, aes(class, fill = class)) +
      geom_bar() +
      labs(title = "Vehicle Classes") +
      scale_fill_luwi_d() +
      theme_luwi(theme = dm$theme()) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      )
  },bg='transparent')

  # Plotly interactive — reacts to dark mode toggle
  output$plotly_scatter <- plotly::renderPlotly({
    p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
      geom_point(size = 2) +
      labs(title = "Iris Measurements") +
      scale_color_luwi_d()
    luwi_ggplotly(p, theme = dm$theme(), tooltip = c("x", "y", "color"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
