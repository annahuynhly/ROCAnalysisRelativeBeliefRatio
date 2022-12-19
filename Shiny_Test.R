# TODO LIST:
# conditionalROC.txt -> CANNOT RUN; waiting for fixed bugs
# conditiononAUCbig.txt -> Need review for plot titles & sanity checks
# ex1prog.txt -> Need review for plot titles & sanity checks
# readdata.txt -> CANNOT RUN; waiting for fixed bugs
# realdataROC.txt -> need to sanity check math & waiting for fixed bugs
# ROC.txt -> some error here?

# Working directory for now; subject to change!
setwd("C:\\Users\\AnnaH\\OneDrive\\Desktop\\Stats RA\\ShinyWebpage")

# Libraries for website creation
library(shiny)
library(shiny.router)

# Other libraries used for the code
library(rBeta2009)

# Accessing other R-codes (for proper coding structure)
source("ShinyHelperFunctions.R")
source("RoutesToLinks.R")

# TODO: ensure we obtain the same result EVERY time!
# Might be impossible within R.

# The server (inputs and outputs)
server = function(input, output, session) {
  router$server(input, output, session)
  # FILES FROM conditionalAUCbig
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
  
}


# Running the actual website
shinyApp(ui, server)

