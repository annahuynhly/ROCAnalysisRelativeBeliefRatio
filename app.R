# TODO LIST:
# conditionalROC.txt -> CANNOT RUN; waiting for fixed bugs
# conditiononAUCbig.txt -> Need review for plot titles & sanity checks
# ex1prog.txt -> Need review for plot titles & sanity checks
# readdata.txt -> CANNOT RUN; waiting for fixed bugs
# realdataROC.txt -> need to sanity check math & waiting for fixed bugs
# ROC.txt -> some error here?

################################################################
# LIBRARIES                                                    #
################################################################

# Libraries for website creation
library(shiny)

# Other libraries used for the code
library(rBeta2009)

# Accessing other R-codes
source("routes.R")

################################################################
# FRONTEND                                                     #
################################################################

ui <- navbarPage(title = " ROC Analysis & Relative Belief",
                 tabPanel("Home", home_page),
                 navbarMenu("Section 3.2",
                            # To avoid the need to parse encoded URLs via utils::URLdecode use e.g.:
                            # tabPanel(title = "Section 3.2.1", "3.2.1 content", value = "section_3.2.1"),
                            tabPanel("ConditionalROC", page_conditionalROC),
                            tabPanel("ConditionalAUCbig", page_conditionalAUCbig),
                            tabPanel("Example1Program", page_ex1prog),
                            tabPanel("ReadData", page_readdata),
                            tabPanel("RealDataROC", page_realdataROC),
                            tabPanel("ROC", page_ROC)
                 ),
                 navbarMenu("Section 3.3",
                            tabPanel("BinormalAUCEqualVariance", page_binormalAUCequalvariance),
                            tabPanel("BinormalAUCUnequalVariance", page_binormalAUCunequalvariance),
                            tabPanel("BinormalCoptEqualVariance", page_binormalcoptequalvariance),
                            tabPanel("BinormalCoptUnequalVariance", page_binormalcoptunequalvariance),
                            tabPanel("CoptPriorPrevalence", page_coptpriorprevalence),
                            tabPanel("plotROC", page_plotROC)
                 ),
                 navbarMenu("Section 3.4",
                            tabPanel("BetaPrior", page_betaprior),
                            tabPanel("BNPAUCFemales", page_BNPAUCFemales),
                            tabPanel("BNPAUCMales", page_BNPAUCMales),
                            tabPanel("BNPcFixedMales", page_BNPcfixedMales),
                            tabPanel("BNPCoptFemales", page_BNPcoptFemales),
                            tabPanel("BNPCoptMales", page_BNPcoptMales),
                            tabPanel("BNPData", page_BNPdata),
                            tabPanel("Empiricals", page_empiricals),
                            tabPanel("ForGammaPrior", page_itsforgammaprior),
                            tabPanel("Smoother", page_smoother),
                            tabPanel("StoreBNPCoptFemales", page_storeBNPcoptFemales)
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
  ################################################################
  # SECTION 3.2                                                  #
  ################################################################
  # OUTPUTS FROM conditionalAUCbig
  output$conditionalAUCbig_values = renderPrint({
    # NOTE: shows all crit values; might need to change issue since code is quite long
    conditionalAUCbig(input$conditionalAUCbig_nMonte, input$conditionalAUCbig_fND, 
                      input$conditionalAUCbig_fD)
  })
  output$conditionalAUCbig_hist = renderPlot({
    conditionalAUCbig_results = conditionalAUCbig(input$conditionalAUCbig_nMonte, 
                                                  input$conditionalAUCbig_fND, 
                                                  input$conditionalAUCbig_fD)
    hist(conditionalAUCbig_results$crit, freq=F, ylab = "y-axis label", xlab = "x-axis label",
         main = "Placeholder Histogram Title", col = "#9fcbec", border = F)
  })
  # OUTPUTS FROM conditionalROC
  # ...
  # OUTPUTS FROM ex_1prog
  output$ex1_prog_values = renderPrint({
    ex1prog(input$w, input$q)
  })
  output$Test_Plot = renderPlot({
    ex1prog_data = ex1prog_graph(input$n_size)
    plot(ex1prog_data$p, ex1prog_data$ROC1, type = "l", lty = 1, ylab = "ROC", xlab = "p",
         main = "template title")
    lines(ex1prog_data$p, ex1prog_data$ROC2, lty=2)
  })
  # OUTPUTS FROM readdata
  # ...
  # OUTPUTS FROM realdataROC
  output$realdataROC_value_1 = renderPrint({
    prior_distribution_c_opt(input$nMonteprior, input$fND, input$fD, input$realdataROC_p)
  })
  output$realdataROC_value_2 = renderPrint({
    post_distribution_c_opt(input$nMontepost, input$fND, input$fD, input$realdataROC_p)
  })
  output$realdataROC_value_3 = renderPrint({
    realdataROC_placeholder_1(input$nMonteprior, input$nMontepost, input$fND, 
                              input$fD, input$realdataROC_p)
  })
  output$realdataROC_value_4 = renderPrint({
    realdataROC_placeholder_2(input$realdataROC_ngrid, input$nMonteprior, input$nMontepost, 
                              input$fND, input$fD, input$realdataROC_p)
  })
  # OUTPUTS FROM ROC
  output$ROC_value_1 = renderPrint({
    simulate_data_ROC(input$ROC_pND, input$ROC_pD, input$ROC_nND, input$ROC_nD)
  })
  output$ROC_value_2 = renderPrint({
    ROC_compute_some_outputs_1(input$ROC_w, input$ROC_pND, input$ROC_pD)
  })
  ################################################################
  # SECTION 3.3                                                  #
  ################################################################
  # OUTPUTS FROM binormalAUCequalvariance
  # ...
  # OUTPUTS FROM binormalAUCunequalvariance
  # ...
  # OUTPUTS FROM binormalcoptequalvariance
  # ...
  # OUTPUTS FROM binormalcoptunequalvariance
  # ...
  # OUTPUTS FROM coptpriorprevalence
  # ...
  # OUTPUTS FROM plotROC
  # ...
  ################################################################
  # SECTION 3.4                                                  #
  ################################################################
  # OUTPUTS FROM betaprior
  # ...
  # OUTPUTS FROM BNPAUCFemales
  # ...
  # OUTPUTS FROM BNPAUCMales
  # ...
  # OUTPUTS FROM BNPcfixedMales
  # ...
  # OUTPUTS FROM BNPcoptFemales
  # ...
  # OUTPUTS FROM BNPcoptMales
  # ...
  # OUTPUTS FROM BNPdata
  # ...
  # OUTPUTS FROM empiricals
  # ...
  # OUTPUTS FROM itsforgammaprior
  # ...
  # OUTPUTS FROM smoother
  # ...
  # OUTPUTS FROM storeBNPcoptFemales
  # ...
}

shinyApp(ui, server)