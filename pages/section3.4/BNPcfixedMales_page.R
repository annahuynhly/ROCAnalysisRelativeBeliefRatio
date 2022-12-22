
################################################################
# DESCRIPTION                                                  #
################################################################

BNPcfixedMales_description = div( # CHANGE THIS
  titlePanel("Page Description"),
  p("Need to work on this section. Come back later!"),
)

################################################################
# PAGE LOGIC                                                   #
################################################################

page_BNPcfixedMales = div( # CHANGE THIS
  titlePanel("Section 3.4: BNPcfixedMales"), # CHANGE THIS
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Slider for the number of bins ----
      numericInput(inputId = "BNPcfixedMales_placeholder", # CHANGE THIS
                   tags$p('Placeholder input that does absolutely nothing.', 
                          style = "font-size: 90%;"),value = 100000, min = 1),
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # OUTPUTTING THE VALUES
      tabsetPanel(type = "tabs",
                  tabPanel("Description", BNPcfixedMales_description), # CHANGE THIS
      )
    )
  )
)
################################################################
# MAIN FUNCTIONS                                               #
################################################################

#TODO ONCE ERRORS ARE FIXED