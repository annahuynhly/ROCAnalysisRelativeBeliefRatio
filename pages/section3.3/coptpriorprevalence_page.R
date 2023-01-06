
################################################################
# DESCRIPTION                                                  #
################################################################

coptpriorprevalence_description = div( # CHANGE THIS
  titlePanel("Page Description"),
  p("Need to work on this section. Come back later!"),
)

################################################################
# PAGE LOGIC                                                   #
################################################################

page_coptpriorprevalence = div( # CHANGE THIS
  titlePanel("Section 3.3: coptpriorprevalence"), # CHANGE THIS
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Slider for the number of bins ----
      numericInput(inputId = "coptpriorprevalence_mu0", # CHANGE THIS
                   tags$p('mu0', style = "font-size: 90%;"),value = 0),
      numericInput(inputId = "coptpriorprevalence_tau0", # CHANGE THIS
                   tags$p('tau0', style = "font-size: 90%;"),value = 0.5),
      numericInput(inputId = "coptpriorprevalence_alpha0", # CHANGE THIS
                   tags$p('alpha0', style = "font-size: 90%;"),value = 1.787),
      numericInput(inputId = "coptpriorprevalence_beta0", # CHANGE THIS
                   tags$p('beta0', style = "font-size: 90%;"),value = 1.056),
      numericInput(inputId = "coptpriorprevalence_a1", # CHANGE THIS
                   tags$p('a1', style = "font-size: 90%;"),value = 15.3589),
      numericInput(inputId = "coptpriorprevalence_a2", # CHANGE THIS
                   tags$p('a2', style = "font-size: 90%;"),value = 22.53835),
      numericInput(inputId = "coptpriorprevalence_nND", # CHANGE THIS
                   tags$p('nND', style = "font-size: 90%;"),value = 25),
      numericInput(inputId = "coptpriorprevalence_xND", # CHANGE THIS
                   tags$p('xND', style = "font-size: 90%;"),value = -0.072),
      numericInput(inputId = "coptpriorprevalence_sND2", # CHANGE THIS
                   tags$p('sND2', style = "font-size: 90%;"),value = 19.38),
      numericInput(inputId = "coptpriorprevalence_nD", # CHANGE THIS
                   tags$p('nD', style = "font-size: 90%;"),value = 20),
      numericInput(inputId = "coptpriorprevalence_xD", # CHANGE THIS
                   tags$p('xD', style = "font-size: 90%;"),value = 0.976),
      numericInput(inputId = "coptpriorprevalence_sD2", # CHANGE THIS
                   tags$p('sD2', style = "font-size: 90%;"),value = 16.778),
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # OUTPUTTING THE VALUES
      tabsetPanel(type = "tabs",
                  tabPanel("Description", coptpriorprevalence_description), # CHANGE THIS
      )
    )
  )
)
################################################################
# MAIN FUNCTIONS                                               #
################################################################

#TODO ONCE ERRORS ARE FIXED