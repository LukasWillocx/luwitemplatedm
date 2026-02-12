# Horizontal Navbar App Template

library(luwitemplatedm)
library(bslib)

# UI
ui <- bslib::page_navbar(
  title = "Application",
  theme = my_theme(),

  # Tab 1
  nav_panel(
    "tab one",
    layout_columns(
      col_widths = c(8, 4, 4, 8),
      row_heights = c('43vh' , '43vh'),
      fill=F,
      card(
        full_screen = T,
        card_header("Card 1"),
        card_body(),
        card_footer('A footer with text')
      ),
      card(
        full_screen = T,
        card_header("Card 2"),
        card_body(
        )
      ),
      card(
        full_screen = T,
        card_header("Card 3"),
        card_body(
        )
      ),
      card(
        full_screen = T,
        card_header("Card 4"),
        card_body(
        )
      )
    )
  ),

  # Tab 2
  nav_panel(
    "tab two",
    layout_columns(
      col_widths = c(4, 8, 8, 4),
      row_heights = c('43vh' , '43vh'),
      fill=F,
      card(
        full_screen = T,
        card_header("Card 5"),
        card_body(
        )
      ),
      card(
        full_screen = T,
        card_header("Card 6"),
        card_body(
        )
      ),
      card(
        full_screen = T,
        card_header("Card 7"),
        card_body(
        )
      ),
      card(
        full_screen = T,
        card_header("Card 8"),
        card_body(
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {

}

# Run the application
shinyApp(ui = ui, server = server)
