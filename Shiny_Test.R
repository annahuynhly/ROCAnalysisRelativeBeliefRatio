
setwd("C:\\Users\\AnnaH\\OneDrive\\Desktop\\Stats RA\\ShinyWebpage")

library(shiny)
library(shiny.router)

# Other dependencies
library(rBeta2009)
source("ShinyHelperFunctions.R")
source("pages_that_need_updating.R")
source("RoutesToLinks.R")

# TODO LIST:
# conditionalROC.txt -> waiting for Mike Evans to show me the fixed bugs
# conditiononAUCbig.txt -> NEED TO CODE
# ex1prog.txt -> SHOULD ADD GRAPHS!! 
# readdata.txt -> NEED TO CODE
# realdataROC.txt -> some error here; also put in multiple sub parts
# ROC.txt -> some error here?

#testing to see if we can put the functions out

server = function(input, output, session) {
  router$server(input, output, session)
  #may not use this due to weird formatting issues
  output$values = renderPrint({
    prior_distribution_c_opt(input$nMonteprior, input$fND, input$fD, input$p)
  })
  output$ex1_prog_values = renderPrint({
    ex1prog(input$w, input$q)
  })
  output$Test_Plot = renderPlot({
    ex1prog_data = ex1prog_graph(input$n_size)
    plot(ex1prog_data$p, ex1prog_data$ROC1, type = "l", lty = 1, ylab = "ROC", xlab = "p",
         main = "template title")
    lines(ex1prog_data$p, ex1prog_data$ROC2, lty=2)
  })
}


shinyApp(ui, server)

