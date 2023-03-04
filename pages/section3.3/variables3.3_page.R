################################################################
# DESCRIPTION                                                  #
################################################################

sect3.3_def_description = div(
  titlePanel("Page Description"),
  mainPanel(
    p("This tab lists the meaning behind the inputs and the outputs listed
        in the AUC page under section 3.3."),
    p("Currently is in progress. Please check back later!")
  )
)

################################################################
# DESCRIPTION OF INPUTS                                        #
################################################################

sect3.3_def_inputs = div(
  titlePanel("Inputs and their Meanings"),
  mainPanel(
    p("Inputs refer to the users inputs found on the AUC page from section 3.2."),
    p("Currently is in progress. Please check back later!")
    
    #tableHTML_output("input_table_description"),
  )
)

################################################################
# DESCRIPTION OF OUTPUTS                                       #
################################################################

sect3.3_def_outputs = div(
  titlePanel("Inputs and their Meanings"),
  mainPanel(
    p("Outputs refer to the outputs (results) found on the binormal diagnostic 
      page from section 3.3."),
    p("Currently is in progress. Please check back later!")
    
    #tableHTML_output("output_table_description"),
  )
)

################################################################
# ELABORATION OF STATISTICAL TERMS                             #
################################################################

sect3.3_def_tables = div(
  titlePanel("Elaboration of Statistical Terms"),
  mainPanel(
    p("In this section, we refer to some uncommonly known statistical definitions that were used in
      this section."),
    p("Currently is in progress. Please check back later!")
    
    #imageOutput("formulas"),
  ),
)


################################################################
# PAGE LOGIC                                                   #
################################################################

page_sect3.3_def = div(
  titlePanel("Section 3.3: Definitions"),
  # OUTPUTTING THE VALUES
  tabsetPanel(type = "tabs",
              tabPanel("Definitions", sect3.3_def_description),
              tabPanel("Inputs", sect3.3_def_inputs),
              tabPanel("Outputs", sect3.3_def_outputs),
              tabPanel("Elaboration of Statistical Terms", sect3.3_def_tables)
  )
)


################################################################
# DETERMINING THE INPUTS                                       #
################################################################

variables3.3_control_panel = fluidPage(
  # Currently unused -> plan to move for the new section later.
  
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


