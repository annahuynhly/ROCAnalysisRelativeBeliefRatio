
################################################################
# DESCRIPTION                                                  #
################################################################

readdata_description = div(
  titlePanel("Page Description"),
  p("Currently there are some bugs in the code. Come back later!"),
)

################################################################
# PAGE LOGIC                                                   #
################################################################

page_readdata = div(
  titlePanel("Section 3.2: readdata"),
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Slider for the number of bins ----
      numericInput(inputId = "readdata_placeholder",
                   tags$p('Placeholder input that does absolutely nothing.', 
                          style = "font-size: 90%;"),value = 100000, min = 1),
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # OUTPUTTING THE VALUES
      tabsetPanel(type = "tabs",
                  tabPanel("Description", readdata_description),
      )
    )
  )
)

################################################################
# MAIN FUNCTIONS                                               #
################################################################

#TODO ONCE ERRORS ARE FIXED