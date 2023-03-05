################################################################
# TODO LIST                                                    #
################################################################

# 3.2: need to add some sections from the text

################################################################
# LIBRARIES                                                    #
################################################################

# Libraries for website creation
library(shiny)
library(DT) # for tables
library(varhandle)
library(tableHTML)

# Other libraries used for the code
library(rBeta2009)
library(tidyverse)

# Accessing other R-codes
source("routes.R")

################################################################
# FRONTEND                                                     #
################################################################

ui = navbarPage(title = " ROC Analysis & Relative Belief",
                 tabPanel("Getting Started", home_page),
                 tabPanel("The Prevalence", page_prevalence_setup),
                 navbarMenu("Section 3.2",
                            tabPanel("Definitions", page_sect3.2_def),
                            tabPanel("Finite-valued Diagnostic", page_finite_val),
                            #tabPanel("Binomial-valued Diagnostic", page_binom_val_diag)
                 ),
                 navbarMenu("Section 3.3",
                            tabPanel("Definitions", page_sect3.3_def),
                            tabPanel("Binormal Diagnostic", page_binormal_val)
                            #tabPanel("BinormalAUCEqualVariance", page_binormalAUCequalvariance),
                            #tabPanel("BinormalAUCUnequalVariance", page_binormalAUCunequalvariance),
                            #tabPanel("BinormalCoptEqualVariance", page_binormalcoptequalvariance),
                 ),
                 navbarMenu("Section 3.4",
                            tabPanel("Definitions", page_sect3.4_def),
                            tabPanel("Nonparametric Bayes Model", page_nonpara_bayes)
                            #tabPanel("BetaPrior", page_betaprior),
                            #tabPanel("BNPAUC", page_BNPAUC),
                            #tabPanel("BNPcFixedMales", page_BNPcfixedMales),
                 ),
                 tabPanel("Contact", contact_page),
                 id = "navbarID",
                 theme = shinythemes::shinytheme("flatly"),
                 #theme = "main.css"
)

################################################################
# BACKEND                                                      #
################################################################

server = function(input, output, session) {

  # SECTION 3.1 ##################################################   

  source(file.path("server", "section3.1.R"),  local = TRUE)$value
  
  # SECTION 3.2 ################################################## 
  
  source(file.path("server", "section3.2.definitions.R"),  local = TRUE)$value
  source(file.path("server", "section3.2.variables.R"),  local = TRUE)$value
  source(file.path("server", "section3.2.outputs.R"),  local = TRUE)$value
  
  # SECTION 3.3 ################################################## 
  
  source(file.path("server", "section3.3.R"),  local = TRUE)$value
  
  # SECTION 3.3 ################################################## 
  
  source(file.path("server", "section3.4.R"),  local = TRUE)$value
}

shinyApp(ui, server)