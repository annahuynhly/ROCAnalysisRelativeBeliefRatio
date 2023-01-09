
################################################################
# DESCRIPTION                                                  #
################################################################

variables3.3_description = div( # CHANGE THIS
  titlePanel("Page Description"),
  p("General idea: if you need to change values for this section, please change them here."),
  p("The reason for this section is because we have a lot of variables that seem to be consistent
    between the codes."),
  p("Need to work on this section. Come back later!"),
)

################################################################
# DETERMINING THE INPUTS                                       #
################################################################

variables3.3_control_panel = fluidPage( # CHANGE THIS
  
  fluidRow(
    column(3, 
           numericInput(inputId = "section3.3_mu0", # CHANGE THIS
                        tags$p('mu0', style = "font-size: 90%;"),value = 0)),
    
    column(3,
           numericInput(inputId = "section3.3_tau0", # CHANGE THIS
                        tags$p('tau0', style = "font-size: 90%;"),value = 0.5)),
    column(3, 
           numericInput(inputId = "section3.3_alpha0", # CHANGE THIS
                        tags$p('alpha0', style = "font-size: 90%;"),value = 1.787)),
  ),
  
  fluidRow(
    column(3, 
           numericInput(inputId = "section3.3_beta0", # CHANGE THIS
                        tags$p('beta0', style = "font-size: 90%;"),value = 1.056)),
    
    column(3,
           numericInput(inputId = "section3.3_a1", # CHANGE THIS
                        tags$p('a1', style = "font-size: 90%;"),value = 15.3589)),
    column(3, 
           numericInput(inputId = "section3.3_a2", # CHANGE THIS
                        tags$p('a2', style = "font-size: 90%;"),value = 22.53835)),
  ),
  
  fluidRow(
    column(3, 
           numericInput(inputId = "section3.3_nND", # CHANGE THIS
                        tags$p('nND', style = "font-size: 90%;"),value = 25)),
    
    column(3,
           numericInput(inputId = "section3.3_xND", # CHANGE THIS
                        tags$p('xND', style = "font-size: 90%;"),value = -0.072)),
    column(3, 
           numericInput(inputId = "section3.3_sND2", # CHANGE THIS
                        tags$p('sND2', style = "font-size: 90%;"),value = 19.38)),
  ),
  
  fluidRow(
    column(3, 
           numericInput(inputId = "section3.3_nD", # CHANGE THIS
                        tags$p('nD', style = "font-size: 90%;"),value = 20)),
    
    column(3,
           numericInput(inputId = "section3.3_xD", # CHANGE THIS
                        tags$p('xD', style = "font-size: 90%;"),value = 0.976)),
    column(3, 
           numericInput(inputId = "section3.3_sD2", # CHANGE THIS
                        tags$p('sD2', style = "font-size: 90%;"),value = 16.778)),
  ),
)


################################################################
# PAGE LOGIC                                                   #
################################################################

page_variables3.3 = div( # CHANGE THIS
  titlePanel("Control Variables"), # CHANGE THIS
  mainPanel(
      # OUTPUTTING THE VALUES
      tabsetPanel(type = "tabs",
                  tabPanel("Description", variables3.3_description),
                  tabPanel("Control Panel", variables3.3_control_panel), # CHANGE THIS
    )
  )
)


################################################################
# MAIN FUNCTIONS                                               #
################################################################

#TODO ONCE ERRORS ARE FIXED