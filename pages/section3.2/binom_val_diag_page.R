
################################################################
# DESCRIPTION                                                  #
################################################################

binom_val_diag_description = div(
  titlePanel("Page Description"),
  p("Currently this has not been implemented. Come back later!"),
)

################################################################
# PAGE LOGIC                                                   #
################################################################

page_binom_val_diag = div(
  titlePanel("Binomial-valued Diagnostic"),
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(width = 3,
      # Input: Slider for the number of bins ----
      numericInput(inputId = "binom_val_diag_placeholder",
                   tags$p('Placeholder input that does absolutely nothing.', 
                          style = "font-size: 90%;"),value = 100000, min = 1),
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # OUTPUTTING THE VALUES
      tabsetPanel(type = "tabs",
                  tabPanel("Description", binom_val_diag_description),
      )
    )
  )
)

################################################################
# MAIN FUNCTIONS                                               #
################################################################

#TODO ONCE ERRORS ARE FIXED