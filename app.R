################################################################
# POTENTIAL OTHER FEATURES                                     #
################################################################
# Add proper link to the resources in the footer
# Add a github link to the repository
# Align the headers
# Align the footer margins for the rest of the sections.
# Make the code look nicer lmao

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
library(tidyverse)

# Globally setting the spinner colour and type
options(spinner.type = 6, spinner.color = "#18bc9b")

# Accessing other R-codes
source("routes.R")

################################################################
# FRONTEND                                                     #
################################################################

ui = navbarPage(title = " ROC Analysis & Relative Belief",
                tabPanel("Getting Started", home_page),
                tabPanel("The Prevalence", page_prevalence_setup),
                navbarMenu("Finite Valued Diagnostic",
                            tabPanel("Definitions", page_sect3.2_def),
                            tabPanel("Computations", page_finite_val),
                ),
                navbarMenu("Binormal Diagnostic",
                            tabPanel("Definitions", page_sect3.3_def),
                            tabPanel("Computations", page_binormal_val)
                ),
                navbarMenu("Nonparametric Bayes Model",
                            tabPanel("Definitions", page_sect3.4_def),
                            tabPanel("Computations", page_nonpara_bayes)
                ),
                tabPanel("Contact & Credits", contact_page),
                id = "navbarID",
                theme = shinythemes::shinytheme("flatly"),
                #theme = "main.css"
                footer = div(
                  br(style = "line-height:10;"),
                  hr(),
                  class = "footer",
                  includeHTML("footer.html"))
)

################################################################
# BACKEND                                                      #
################################################################

server = function(input, output, session) {
  # Setting the seed
  global_seed = reactive(input$chosen_seed)
  
  # SECTION 3.1 ##################################################   

  source(file.path("server", "section3.1.R"),  local = TRUE)$value
  
  # SECTION 3.2 ################################################## 
  
  source(file.path("server", "section3.2.definitions.R"),  local = TRUE)$value
  source(file.path("server", "section3.2.variables.R"),  local = TRUE)$value
  source(file.path("server", "section3.2.outputs.R"),  local = TRUE)$value
  
  # SECTION 3.3 ################################################## 
  
  source(file.path("server", "section3.3.definitions.R"),  local = TRUE)$value
  source(file.path("server", "section3.3.variables.R"),  local = TRUE)$value
  source(file.path("server", "section3.3.outputs.R"),  local = TRUE)$value
  
  # SECTION 3.3 ################################################## 
  
  source(file.path("server", "section3.4.R"),  local = TRUE)$value
  
  # ANIMATIONS ###################################################
  # Note: may make a separate .R file based on the number of animations
  observe(addHoverAnim(session, 'prevalence_arrow', 'wobble'))
  observe(addHoverAnim(session, 'AnnaImg', 'rubberBand'))
  observe(addHoverAnim(session, 'MikeImg', 'tada'))
  observe(addHoverAnim(session, 'LuaiImg', 'flip'))
  observe(addHoverAnim(session, 'QiaoyuImg', 'fadeOutDown'))
  
}

shinyApp(ui, server)