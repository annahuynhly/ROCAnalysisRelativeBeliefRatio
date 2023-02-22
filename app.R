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

ui <- navbarPage(title = " ROC Analysis & Relative Belief",
                 tabPanel("Home", home_page),
                 tabPanel("The Prevalence", page_RB_setup),
                 navbarMenu("Section 3.2",
                            # To avoid the need to parse encoded URLs via utils::URLdecode use e.g.:
                            # tabPanel(title = "Section 3.2.1", "3.2.1 content", value = "section_3.2.1"),
                            tabPanel("Definitions", page_sect3.2_def),
                            tabPanel("Finite-valued Diagnostic", page_theAUC),
                 ),
                 navbarMenu("Section 3.3",
                            tabPanel("3.3 Variables", page_variables3.3),
                            tabPanel("BinormalAUCEqualVariance", page_binormalAUCequalvariance),
                            tabPanel("BinormalAUCUnequalVariance", page_binormalAUCunequalvariance),
                            #tabPanel("BinormalCoptEqualVariance", page_binormalcoptequalvariance),
                            #tabPanel("BinormalCoptUnequalVariance", page_binormalcoptunequalvariance),
                            #tabPanel("CoptPriorPrevalence", page_coptpriorprevalence),
                            #tabPanel("plotROC", page_plotROC)
                 ),
                 navbarMenu("Section 3.4",
                            tabPanel("3.4 Variables", page_variables3.4),
                            tabPanel("BetaPrior", page_betaprior),
                            tabPanel("BNPAUC", page_BNPAUC),
                            #tabPanel("BNPcFixedMales", page_BNPcfixedMales),
                            #tabPanel("BNPCoptFemales", page_BNPcoptFemales),
                            #tabPanel("BNPCoptMales", page_BNPcoptMales),
                            #tabPanel("BNPData", page_BNPdata),
                            #tabPanel("Empiricals", page_empiricals),
                            #tabPanel("ForGammaPrior", page_itsforgammaprior),
                            #tabPanel("Smoother", page_smoother),
                            #tabPanel("StoreBNPCoptFemales", page_storeBNPcoptFemales)
                 ),
                 tabPanel("Contact", contact_page),
                 id = "navbarID",
                 theme = shinythemes::shinytheme("flatly"),
                 #theme = "main.css"
)

################################################################
# BACKEND                                                      #
################################################################

server <- function(input, output, session) {
  # Making it so the tab changes when the user clicks to another tab
  observeEvent(session$clientData$url_hash, {
    currentHash <- utils::URLdecode(sub("#", "", session$clientData$url_hash))
    if(is.null(input$navbarID) || !is.null(currentHash) && currentHash != input$navbarID){
      freezeReactiveValue(input, "navbarID")
      updateNavbarPage(session, "navbarID", selected = currentHash)
    }
  }, priority = 1)
  
  observeEvent(input$navbarID, {
    currentHash <- sub("#", "", session$clientData$url_hash)
    pushQueryString <- paste0("#", input$navbarID)
    if(is.null(currentHash) || currentHash != input$navbarID){
      freezeReactiveValue(input, "navbarID")
      updateQueryString(pushQueryString, mode = "push", session)
    }
  }, priority = 0)
  

  # SECTION 3.1 ##################################################   

  source(file.path("server", "section3.1.R"),  local = TRUE)$value
  
  # SECTION 3.2 ################################################## 
  
  source(file.path("server", "section3.2.R"),  local = TRUE)$value
  
  # SECTION 3.3 ################################################## 
  
  source(file.path("server", "section3.3.R"),  local = TRUE)$value
  
  # SECTION 3.3 ################################################## 
  
  source(file.path("server", "section3.4.R"),  local = TRUE)$value
}

shinyApp(ui, server)