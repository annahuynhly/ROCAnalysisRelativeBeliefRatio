
################################################################
# DESCRIPTION                                                  #
################################################################

variables3.4_description = div( # CHANGE THIS
  titlePanel("Page Description"),
  p("General idea: if you need to change values for this section, please change them here."),
  p("This also includes the .csv file used for some of the computations."),
  p("The reason for this section is because we have a lot of variables that seem to be consistent
    between the codes."),
  p("Need to work on this section. Come back later!"),
)

################################################################
# DETERMINING THE INPUTS                                       #
################################################################

variables3.4_control_panel = fluidPage( # CHANGE THIS
  
  fluidRow(
    column(3, 
           numericInput(inputId = "section3.4_mu0", # CHANGE THIS
                        tags$p('mu0', style = "font-size: 90%;"),value = 45)),
    column(3,
           numericInput(inputId = "section3.4_tau0", # CHANGE THIS
                        tags$p('tau0', style = "font-size: 90%;"),value = 0.5)),
    column(3, 
           numericInput(inputId = "section3.4_lambda1", # CHANGE THIS
                        tags$p('lambda1', style = "font-size: 90%;"),value = 8.545)),
    column(3, 
           numericInput(inputId = "section3.4_lambda2", # CHANGE THIS
                        tags$p('lambda2', style = "font-size: 90%;"),value = 1080.596)),
  ),
  
  fluidRow(
    column(3, 
           numericInput(inputId = "section3.4_a1", # CHANGE THIS
                        tags$p('a1', style = "font-size: 90%;"),value = 9.81)),
    column(3,
           numericInput(inputId = "section3.4_a2", # CHANGE THIS
                        tags$p('a2', style = "font-size: 90%;"),value = 109.66)),
    column(3, 
           numericInput(inputId = "section3.4_a", # CHANGE THIS
                        tags$p('a', style = "font-size: 90%;"),value = 9.8)),
    column(3, 
           numericInput(inputId = "section3.4_L", # CHANGE THIS
                        tags$p('L', style = "font-size: 90%;"),value = 200)),
  ),
  
  fluidRow(
    column(3, 
           numericInput(inputId = "section3.4_nMonteprior", # CHANGE THIS
                        tags$p('nMonteprior', style = "font-size: 90%;"),value = 200000)),
    
    column(3,
           numericInput(inputId = "section3.4_nMontepost", # CHANGE THIS
                        tags$p('nMontepost', style = "font-size: 90%;"),value = 200000)),
    column(3, 
           numericInput(inputId = "section3.4_nstar", # CHANGE THIS
                        tags$p('nstar', style = "font-size: 90%;"),value = 200)),
  ),
)

variables3.4_gender_covid_data = fluidPage(
  title = "Displaying the Gender Covid Dataset Used in the Code",
  DT::dataTableOutput("gender_covid")
)


################################################################
# PAGE LOGIC                                                   #
################################################################

page_variables3.4 = div( # CHANGE THIS
  titlePanel("Control Variables"), # CHANGE THIS
  mainPanel(
      # OUTPUTTING THE VALUES
      tabsetPanel(type = "tabs",
                  tabPanel("Description", variables3.4_description),
                  tabPanel("Control Panel", variables3.4_control_panel), # CHANGE THIS
                  tabPanel("Gender Covid Data", variables3.4_gender_covid_data)
    )
  )
)


################################################################
# MAIN FUNCTIONS                                               #
################################################################

#TODO ONCE ERRORS ARE FIXED