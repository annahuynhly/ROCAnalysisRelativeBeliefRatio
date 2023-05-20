################################################################
# POTENTIAL OTHER FEATURES                                     #
################################################################
# Add a github link to the repository
# Align the footer margins for the rest of the sections. -> LOW PRIORITY
# Make sure the seed is consistent
# Make the code look nicer lmao
# make outputs look nicer ??? -> probably not, looks decent enough anyways...
# make sure there is seed support for the inferences for the optimal cutoff
# maybe add more resources for understanding?
# add more colour themes and make the code more efficient for selecting a colour

# ISSUES:

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
  title = " ROC Analysis & Relative Belief",
  tabPanel("Home", home_page),
  tabPanel("Definitions", def_page),
  navbarMenu("Finite Valued Diagnostic",
    #tabPanel("Definitions", page_sect3.2_def),
    tabPanel("Getting Started", page_finite_val_start),
    tabPanel("The Prevalence", page_finite_val_prevalence),
    tabPanel("Inferences for the AUC", page_finite_val_inference1),
    tabPanel("Inferences of the Optimal Cutoff", page_finite_val_inference2)
  ),
  navbarMenu("Binormal Diagnostic",
    #tabPanel("Definitions", page_sect3.3_def),
    tabPanel("Getting Started", page_binormal_diag_start),
    tabPanel("The Prevalence", page_binormal_diag_prevalence),
    tabPanel("Inferences for the AUC", page_binormal_diag_inference1),
    tabPanel("Inferences of the Optimal Cutoff", page_binormal_diag_inference2)
  ),
  navbarMenu("Nonparametric Bayes Model",
    #tabPanel("Definitions", page_sect3.4_def),
    tabPanel("Getting Started", page_nonpara_bayes_start),
    tabPanel("The Prevalence", page_nonpara_bayes_prevalence),
    tabPanel("Inferences for the AUC", page_nonpara_bayes1),
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
  
  source(file.path("server/section3.2", "section3.2_definitions.R"),  local = TRUE)$value
  source(file.path("server/section3.2", "section3.2_prevalence.R"),  local = TRUE)$value
  source(file.path("server/section3.2", "section3.2_setup_variables.R"),  local = TRUE)$value
  source(file.path("server/section3.2", "section3.2_variables.R"),  local = TRUE)$value
  source(file.path("server/section3.2", "section3.2_outputs.R"),  local = TRUE)$value
  
  # SECTION 3.3 ################################################## 
  
  source(file.path("server/section3.3", "section3.3_definitions.R"),  local = TRUE)$value
  source(file.path("server/section3.3", "section3.3_prevalence.R"),  local = TRUE)$value
  source(file.path("server/section3.3", "section3.3_setup_variables.R"),  local = TRUE)$value
  source(file.path("server/section3.3", "section3.3_variables.R"),  local = TRUE)$value
  source(file.path("server/section3.3", "section3.3_outputs.R"),  local = TRUE)$value
  
  # SECTION 3.3 ################################################## 
  
  source(file.path("server/section3.4", "section3.4.R"),  local = TRUE)$value
  source(file.path("server/section3.4", "section3.4_prevalence.R"),  local = TRUE)$value
  source(file.path("server/section3.4", "section3.4_setup_variables.R"),  local = TRUE)$value
  source(file.path("server/section3.4", "section3.4_variables.R"),  local = TRUE)$value
  source(file.path("server/section3.4", "section3.4_outputs.R"),  local = TRUE)$value
  
  # ANIMATIONS ###################################################
  # Note: may make a separate .R file based on the number of animations
  
  observe(addHoverAnim(session, 'finite_val_diag_prevalence_arrow', 'wobble'))
  observe(addHoverAnim(session, 'binormal_diag_prevalence_arrow', 'wobble'))
  observe(addHoverAnim(session, 'nonpara_bayes_prevalence_arrow', 'wobble'))
  observe(addHoverAnim(session, 'AnnaImg', 'rubberBand'))
  observe(addHoverAnim(session, 'MikeImg', 'tada'))
  observe(addHoverAnim(session, 'LuaiImg', 'flip'))
  observe(addHoverAnim(session, 'QiaoyuImg', 'fadeOutDown'))
  observe(addHoverAnim(session, 'calculator', 'pulse'))
  observe(addHoverAnim(session, 'diseased_group', 'pulse'))
  observe(addHoverAnim(session, 'diseased_group2', 'pulse'))
  observe(addHoverAnim(session, 'diseased_group3', 'pulse'))
  observe(addHoverAnim(session, 'groups', 'pulse'))
  observe(addHoverAnim(session, 'chart', 'pulse'))
  observe(addHoverAnim(session, 'ROC_AUC_graph', 'pulse'))
}

shinyApp(ui, server)