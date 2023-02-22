# OUTPUTS FROM betaprior
betaprior_1 = reactive(calculate_betaprior_values(input$betaprior_l, input$betaprior_u, input$betaprior_gamma, 
                                                  input$betaprior_error, input$betaprior_nmax, input$betaprior_taulow, 
                                                  input$betaprior_tauup, input$betaprior_n, input$betaprior_nD, 
                                                  input$betaprior_low, input$betaprior_up))
output$betaprior_output = renderPrint({
  betaprior_1()
})
output$betaprior_plot = renderPlot({
  plot(betaprior_1()$w, dbeta(betaprior_1()$w, betaprior_1()$a1, betaprior_1()$a2), type="l",
       main = "placeholder title", ylab = "placeholder y", xlab = "placeholder x")
})
output$gender_covid = DT::renderDataTable({
  DT::datatable(gender_covid_dataset)
})

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
output$BNPdata_output = renderPrint({
  BNPdata_output(input$BNPdata_gender)
})
output$BNPdata_plot = renderPlot({
  BNPdata_graphs(input$BNPdata_gender)
})
# OUTPUTS FROM empiricals
# ...
# OUTPUTS FROM itsforgammaprior
itsforgammaprior_result = reactive(
  itsforgammapriorfunction(input$itsforgammaprior_gam, input$itsforgammaprior_l0, 
                           input$itsforgammaprior_u0, input$itsforgammaprior_maxits, 
                           input$itsforgammaprior_alphaup, input$itsforgammaprior_alphalow, 
                           input$itsforgammaprior_eps))
output$itsforgammaprior_value = renderPrint({
  itsforgammaprior_result()
})
output$itsforgammaprior_plot_1 = renderPlot({
  plot(itsforgammaprior_result()$x, itsforgammaprior_result()$dens1,
       xlab="1/sigma^2",ylab="prior density",type="l",
       main = "placeholder title")
})
output$itsforgammaprior_plot_1 = renderPlot({
  plot(itsforgammaprior_result()$y, itsforgammaprior_result()$dens2,
       xlab="sigma^2",ylab="prior density",type="l",
       main = "placeholder title")
})
output$itsforgammaprior_plot_1 = renderPlot({
  plot(itsforgammaprior_result()$z, itsforgammaprior_result()$dens3,
       xlab="sigma",ylab="prior density",type="l",
       main = "placeholder title")
})
# OUTPUTS FROM smoother
# ...
# OUTPUTS FROM storeBNPcoptFemales
# ...