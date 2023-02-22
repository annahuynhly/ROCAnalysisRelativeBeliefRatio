# see if these variables stay around? idk lmao
# OUTPUTS FROM binormalAUCequalvariance
# ...
# OUTPUTS FROM binormalAUCunequalvariance
# ...
# OUTPUTS FROM binormalcoptequalvariance
# NEED TO MAKE REACTIVE EXPRESSIONS
#reactive_test = reactive(test_function(list("hello" = 1, "hello2" = 2)))
#output$binormalcoptequalvariance_value_1 = renderPrint({reactive_test()})
binocopteqvar_1 = reactive(prior_dist_copt(input$binormalcoptequalvariance_nMonteprior, 
                                           input$binormalcoptequalvariance_L, 
                                           input$section3.3_mu0, input$section3.3_tau0, input$section3.3_alpha0, 
                                           input$section3.3_beta0, input$section3.3_a1, input$section3.3_a2,
                                           input$section3.3_nND, input$section3.3_xND, input$section3.3_sND2, 
                                           input$section3.3_nD, input$section3.3_xD, input$section3.3_sD2))
binocopteqvar_2 = reactive(post_dist_copt(input$binormalcoptequalvariance_nMontepost, 
                                          input$binormalcoptequalvariance_L, 
                                          input$section3.3_mu0, input$section3.3_tau0, input$section3.3_alpha0, 
                                          input$section3.3_beta0, input$section3.3_a1, input$section3.3_a2,
                                          input$section3.3_nND, input$section3.3_xND, input$section3.3_sND2, 
                                          input$section3.3_nD, input$section3.3_xD, input$section3.3_sD2))
binocopteqvar_3 = reactive(
  relative_belief_ratio_inferences(input$binormalcoptequalvariance_nMonteprior,
                                   input$binormalcoptequalvariance_nMontepost, 
                                   input$binormalcoptequalvariance_L, 
                                   input$section3.3_mu0, input$section3.3_tau0, input$section3.3_alpha0, 
                                   input$section3.3_beta0, input$section3.3_a1, input$section3.3_a2,
                                   input$section3.3_nND, input$section3.3_xND, input$section3.3_sND2, 
                                   input$section3.3_nD, input$section3.3_xD, input$section3.3_sD2)
)
output$binormalcoptequalvariance_value_1 = renderPrint({
  binocopteqvar_1()
})
output$binormalcoptequalvariance_plot_1 = renderPlot({
  plot(binocopteqvar_1()$grid, binocopteqvar_1()$priorcmoddensity, xlab="cmod",
       ylab="prior", main = "placeholder title", type="l",lty=1)
})
output$binormalcoptequalvariance_value_2 = renderPrint({
  binocopteqvar_2()
})
output$binormalcoptequalvariance_plot_2 = renderPlot({
  plot(binocopteqvar_2()$grid, binocopteqvar_2()$postcmoddensity, xlab="cmod",
       ylab="prior and posterior",type="l",lty=1, main = "placeholder title")
  lines(binocopteqvar_2()$grid, binocopteqvar_2()$priorcmoddensity, type="l",lty=2)
})
output$binormalcoptequalvariance_value_3 = renderPrint({
  binocopteqvar_3()
})
output$binormalcoptequalvariance_plot_3 = renderPlot({
  plot(binocopteqvar_3()$grid, binocopteqvar_3()$RBcmod, xlab="cmod",
       ylab=expression("RB"),type="l",lty=1, main = "placeholder title")
})
# NOT DONE PUTTING OUTPUTS BUT TOO LAZY TO ADD THE REST AT THE MOMENT
# OUTPUTS FROM binormalcoptunequalvariance
binocoptuneqvar_1 = reactive(prior_dist_copt_unequalvar(input$binormalcoptunequalvariance_lambda,
                                                        input$binormalcoptunequalvariance_nMonteprior, 
                                                        input$binormalcoptunequalvariance_L, 
                                                        input$section3.3_mu0, input$section3.3_tau0, input$section3.3_alpha0, 
                                                        input$section3.3_beta0, input$section3.3_a1, input$section3.3_a2,
                                                        input$section3.3_nND, input$section3.3_xND, input$section3.3_sND2, 
                                                        input$section3.3_nD, input$section3.3_xD, input$section3.3_sD2))
binocoptuneqvar_2 = reactive(post_dist_copt_unequalvar(input$binormalcoptunequalvariance_lambda,
                                                       input$binormalcoptunequalvariance_nMontepost, 
                                                       input$binormalcoptunequalvariance_L, 
                                                       input$section3.3_mu0, input$section3.3_tau0, input$section3.3_alpha0, 
                                                       input$section3.3_beta0, input$section3.3_a1, input$section3.3_a2,
                                                       input$section3.3_nND, input$section3.3_xND, input$section3.3_sND2, 
                                                       input$section3.3_nD, input$section3.3_xD, input$section3.3_sD2))
binocoptuneqvar_3 = reactive(
  relative_belief_ratio_inferences_unequalvar(input$binormalcoptunequalvariance_lambda,
                                              input$binormalcoptunequalvariance_nMonteprior,
                                              input$binormalcoptunequalvariance_nMontepost, 
                                              input$binormalcoptunequalvariance_L, 
                                              input$section3.3_mu0, input$section3.3_tau0, input$section3.3_alpha0, 
                                              input$section3.3_beta0, input$section3.3_a1, input$section3.3_a2,
                                              input$section3.3_nND, input$section3.3_xND, input$section3.3_sND2, 
                                              input$section3.3_nD, input$section3.3_xD, input$section3.3_sD2)
)
output$binormalcoptunequalvariance_value_1 = renderPrint({
  binocoptuneqvar_1()
})
output$binormalcoptunequalvariance_plot_1 = renderPlot({
  plot(binocoptuneqvar_1()$grid, binocoptuneqvar_1()$priorcmoddensity, xlab="cmod",
       ylab="prior", main = "placeholder title", type="l",lty=1)
})
output$binormalcoptunequalvariance_value_2 = renderPrint({
  binocoptuneqvar_2()
})
output$binormalcoptunequalvariance_plot_2 = renderPlot({
  plot(binocoptuneqvar_2()$grid, binocoptuneqvar_2()$postcmoddensity, xlab="cmod",
       ylab="prior and posterior",type="l",lty=1, main = "placeholder title")
  lines(binocoptuneqvar_2()$grid, binocoptuneqvar_2()$priorcmoddensity, type="l",lty=2)
})
output$binormalcoptunequalvariance_value_3 = renderPrint({
  binocoptuneqvar_3()
})
output$binormalcoptunequalvariance_plot_3 = renderPlot({
  plot(binocoptuneqvar_3()$grid, binocoptuneqvar_3()$RBcmod, xlab="cmod",
       ylab=expression("RB"),type="l",lty=1, main = "placeholder title")
})

# ...
# OUTPUTS FROM coptpriorprevalence
# ...
# OUTPUTS FROM plotROC
output$plotROC_calculator = renderPrint({
  plotROC_crit_values(input$plotROC_muD, input$plotROC_sigmaD, input$plotROC_muND, 
                      input$plotROC_sigmaND, input$plotROC_w)
})
output$plotROC_plot = renderPlot({
  plotROC_data = plotROC_crit_values(input$plotROC_muD, input$plotROC_sigmaD, input$plotROC_muND, 
                                     input$plotROC_sigmaND, input$plotROC_w)
  plot(plotROC_data$p, plotROC_data$ROC, pch=20, xlab = "placeholder x", ylab = "placeholder y",
       main = "template title")
  lines(plotROC_data$p,plotROC_data$p,pch=4)
})