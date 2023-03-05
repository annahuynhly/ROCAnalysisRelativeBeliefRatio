################################################################
# DESCRIPTION                                                  #
################################################################

sect3.4_def_description = div(
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

sect3.4_def_inputs = div(
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

sect3.4_def_outputs = div(
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

sect3.4_def_tables = div(
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

page_sect3.4_def = div(
  titlePanel("Section 3.4: Definitions"),
  # OUTPUTTING THE VALUES
  tabsetPanel(type = "tabs",
              tabPanel("Definitions", sect3.4_def_description),
              tabPanel("Inputs", sect3.4_def_inputs),
              tabPanel("Outputs", sect3.4_def_outputs),
              tabPanel("Elaboration of Statistical Terms", sect3.4_def_tables)
  )
)








################################################################
# DETERMINING THE INPUTS                                       #
################################################################


variables3.4_gender_covid_data = fluidPage(
  title = "Displaying the Gender Covid Dataset Used in the Code",
  DT::dataTableOutput("gender_covid")
)

