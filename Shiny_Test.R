
# Working directory for now; subject to change!
setwd("C:\\Users\\AnnaH\\OneDrive\\Desktop\\Stats RA\\ShinyWebpage")

library(shiny)
library(shiny.router)

# Other dependencies
library(rBeta2009)
source("ShinyHelperFunctions.R")
source("pages_that_need_updating.R")
source("RoutesToLinks.R")

#The following is to ensure we obtain the same result EVERY time!

# TODO LIST:
# conditionalROC.txt -> CANNOT RUN; waiting for fixed bugs
# conditiononAUCbig.txt -> Need review for plot titles & sanity checks
# ex1prog.txt -> Need review for plot titles & sanity checks
# readdata.txt -> CANNOT RUN; waiting for fixed bugs
# realdataROC.txt -> need to sanity check math & waiting for fixed bugs
# ROC.txt -> some error here?

#testing to see if we can put the functions out

server = function(input, output, session) {
  router$server(input, output, session)
  # FILES FROM conditionalAUCbig
  output$conditionalAUCbig_values = renderPrint({
    # NOTE: shows all crit values; might be an issue since it's long lol
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
}

shinyApp(ui, server)

