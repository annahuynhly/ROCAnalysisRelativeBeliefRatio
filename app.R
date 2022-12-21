# TODO LIST:
# conditionalROC.txt -> CANNOT RUN; waiting for fixed bugs
# conditiononAUCbig.txt -> Need review for plot titles & sanity checks
# ex1prog.txt -> Need review for plot titles & sanity checks
# readdata.txt -> CANNOT RUN; waiting for fixed bugs
# realdataROC.txt -> need to sanity check math & waiting for fixed bugs
# ROC.txt -> some error here?

# Working directory for now; subject to change!
#setwd("C:\\Users\\AnnaH\\OneDrive\\Desktop\\Stats RA\\ShinyWebpage")

# Libraries for website creation
library(shiny)

# Other libraries used for the code
library(rBeta2009)

# Accessing other R-codes (for proper coding structure)
source("routes.R")
#source("ShinyHelperFunctions.R")
#source("./pages/section3.2/home_page.R")
#source("./pages/section3.2/conditionalROC_page.R")
#source("./pages/section3.2/conditionalAUCbig_page.R")
#source("./pages/section3.2/ex1prog_page.R")
#source("./pages/section3.2/readdata_page.R")
#source("./pages/section3.2/realdataROC_page.R")
#source("./pages/section3.2/ROC_page.R")
#source("./pages/section3.2/contact_page.R")

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
                            tabPanel("BinormalAUCEqualVariance", "placeholder page"),
                            tabPanel("BinormalAUCUnequalVariance", "placeholder page"),
                            tabPanel("BinormalCoptEqualVariance", "placeholder page"),
                            tabPanel("BinormalCoptUnequalVariance", "placeholder page"),
                            tabPanel("CoptPriorPrevalence", "placeholder page"),
                            tabPanel("plotROC", "placeholder page")
                 ),
                 navbarMenu("Section 3.4",
                            tabPanel("BetaPrior", "placeholder page"),
                            tabPanel("BNPAUCFemales", "placeholder page"),
                            tabPanel("BNPAUCMales", "placeholder page"),
                            tabPanel("BNPcFixedMales", "placeholder page"),
                            tabPanel("BNPCoptFemales", "placeholder page"),
                            tabPanel("BNPCoptMales", "placeholder page"),
                            tabPanel("BNPData", "placeholder page"),
                            tabPanel("Empiricals", "placeholder page"),
                            tabPanel("ForGammaPrior", "placeholder page"),
                            tabPanel("Smoother", "placeholder page"),
                            tabPanel("StoreBNPCoptFemales", "placeholder page")
                 ),
                 tabPanel("Contact", contact_page),
                 id = "navbarID",
                 theme = shinythemes::shinytheme("flatly"),
                 #theme = "main.css"
)

server <- function(input, output, session) {
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
  # FILES FROM ex_1prog_values
  output$ex1_prog_values = renderPrint({
    ex1prog(input$w, input$q)
  })
  output$Test_Plot = renderPlot({
    ex1prog_data = ex1prog_graph(input$n_size)
    plot(ex1prog_data$p, ex1prog_data$ROC1, type = "l", lty = 1, ylab = "ROC", xlab = "p",
         main = "template title")
    lines(ex1prog_data$p, ex1prog_data$ROC2, lty=2)
  })
  # FILES FROM realdataROC
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
  # FILES FROM ROC
  # inputs: pND, pD, nND, nD
  output$ROC_value_1 = renderPrint({
    simulate_data_ROC(input$ROC_pND, input$ROC_pD, input$ROC_nND, input$ROC_nD)
  })
  output$ROC_value_2 = renderPrint({
    ROC_compute_some_outputs_1(input$ROC_w, input$ROC_pND, input$ROC_pD)
  })
  
}

shinyApp(ui, server)