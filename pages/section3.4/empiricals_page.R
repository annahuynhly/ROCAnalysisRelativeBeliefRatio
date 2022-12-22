
################################################################
# DESCRIPTION                                                  #
################################################################

empiricals_description = div( # CHANGE THIS
  titlePanel("Page Description"),
  p("Need to work on this section. Come back later!"),
)

################################################################
# PAGE LOGIC                                                   #
################################################################

page_empiricals = div( # CHANGE THIS
  titlePanel("Section 3.4: empiricals"), # CHANGE THIS
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Slider for the number of bins ----
      numericInput(inputId = "empiricals_placeholder", # CHANGE THIS
                   tags$p('Placeholder input that does absolutely nothing.', 
                          style = "font-size: 90%;"),value = 100000, min = 1),
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # OUTPUTTING THE VALUES
      tabsetPanel(type = "tabs",
                  tabPanel("Description", empiricals_description), # CHANGE THIS
      )
    )
  )
)
################################################################
# MAIN FUNCTIONS                                               #
################################################################

#TODO ONCE ERRORS ARE FIXED