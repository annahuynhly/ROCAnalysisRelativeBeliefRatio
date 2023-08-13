# TODO: add feature for someone to change the location of the legend or not have it at all.
################################################################
# LIBRARIES                                                    #
################################################################

# Libraries for website creation
library(shiny)
library(DT) # for tables
library(varhandle)
library(tableHTML)
library(shinyanimate) # image animations
library(shinycssloaders) # for loading screens

# Other libraries used for the code
library(rBeta2009)
library(LaplacesDemon)
library(tidyverse)

# Globally setting the spinner colour and type
options(spinner.type = 6, spinner.color = "#18bc9b")

# Removing scientific notation
options(scipen = 999)

# Accessing other R-codes
source("routes.R")

################################################################
# FRONTEND                                                     #
################################################################

ui = navbarPage(
  title = "Relative Belief Ratio for ROC Analysis",
  tabPanel("Home", home_page),
  tabPanel("Definitions", def_page),
  navbarMenu("Finite Valued Diagnostic",
    #tabPanel("Definitions", page_sect3.2_def),
    tabPanel("Inferences for the AUC", page_finite_val_inference1),
    tabPanel("Input for the Prevalence", page_finite_val_start),
    tabPanel("Inferences for the Prevalence", page_finite_val_prevalence),
    tabPanel("Inferences for the Optimal Cutoff", page_finite_val_inference2)
  ),
  navbarMenu("Binormal Diagnostic",
    #tabPanel("Definitions", page_sect3.3_def),
    tabPanel("Inferences for the AUC", page_binormal_diag_inference1),
    tabPanel("Input for the Prevalence", page_binormal_diag_start),
    tabPanel("Inferences for the Prevalence", page_binormal_diag_prevalence),
    tabPanel("Inferences for the Optimal Cutoff", page_binormal_diag_inference2)
  ),
  navbarMenu("Nonparametric Bayes Model",
    #tabPanel("Definitions", page_sect3.4_def),
    tabPanel("Inferences for the AUC", page_nonpara_bayes1),
    tabPanel("Input for the Prevalence", page_nonpara_bayes_start),
    tabPanel("Inferences for the Prevalence", page_nonpara_bayes_prevalence),
    tabPanel("Inferences for the Optimal Cutoff", page_nonpara_bayes2),
  ),
  tabPanel("Contact & Credits", contact_page),
  id = "navbarID",
  theme = shinythemes::shinytheme("flatly"),
  #theme = "main.css"
  footer = div(
    br(style = "line-height:10;"),
    hr(),
    class = "footer",
    includeHTML("footer.html")
  )
)

################################################################
# BACKEND                                                      #
################################################################

server = function(input, output, session) {

  # SECTION 3.2 ################################################## 
  
  source(file.path("server/section3.2", "section3.2_setup_variables.R"),  local = TRUE)$value
  source(file.path("server/section3.2", "section3.2_variables.R"),  local = TRUE)$value
  source(file.path("server/section3.2", "section3.2_prevalence.R"),  local = TRUE)$value
  source(file.path("server/section3.2", "section3.2_outputs.R"),  local = TRUE)$value
  
  # SECTION 3.3 ################################################## 
  
  source(file.path("server/section3.3", "section3.3_setup_variables.R"),  local = TRUE)$value
  source(file.path("server/section3.3", "section3.3_variables_equal.R"),  local = TRUE)$value
  source(file.path("server/section3.3", "section3.3_variables_unequal.R"),  local = TRUE)$value
  source(file.path("server/section3.3", "section3.3_prevalence.R"),  local = TRUE)$value
  source(file.path("server/section3.3", "section3.3_outputs.R"),  local = TRUE)$value
  
  # SECTION 3.3 ################################################## 
  
  source(file.path("server/section3.4", "section3.4_setup_variables.R"),  local = TRUE)$value
  source(file.path("server/section3.4", "section3.4_variables.R"),  local = TRUE)$value
  source(file.path("server/section3.4", "section3.4_prevalence.R"),  local = TRUE)$value
  source(file.path("server/section3.4", "section3.4_outputs.R"),  local = TRUE)$value
  
  # ANIMATIONS ###################################################
  
  source(file.path("server", "animations.R"),  local = TRUE)$value
}

shinyApp(ui, server)