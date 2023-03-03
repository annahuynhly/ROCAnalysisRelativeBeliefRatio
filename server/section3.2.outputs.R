################################################################
# NUMERIC/TEXT OUTPUTS                                         #
################################################################

#output$theAUC_output1 = renderPrint({ # For testing
#  list("test_val" = sect3.2_AUC_post()$AUC) # will need to specify when
#})

output$theAUC_output1 = renderPrint({
  list("Prior Copt" = sect3.2_AUC_prior()$priorc_opt,
       "Post Copt" = sect3.2_AUC_post()$postc_opt,
       "Copt Estimate" = sect3.2_AUC_RBR()$c_optfDfND,
       "Error Characteristics" = as.data.frame(sect3.2_copt_est())) # will need to specify when
  # providing a vector
  # TODO: specify credible region
})

output$theAUC_hypoAUC_value = renderPrint({
  list1 = list("Plausible Region of the AUC" = sect3.2_pr()$plausible_region,
               "Posterior Content of the AUC" = sect3.2_AUC_post_content(),
               "Credible Region of the AUC" = sect3.2_cr()$credible_region,
               "Area Under the Line Plot (next section)" = sect3.2_lineplot_area())
  list2 = append(sect3.2_hypo_test(), list1)
  list2
})

################################################################
# HISTOGRAMS                                                   #
################################################################

theAUC_colours = reactive({
  if(input$theAUC_colour == 'default'){
    "default"
  }
  else if (input$theAUC_colour == 'manual'){
    c(paste("#", input$theAUC_colour_prior, sep = ""),
      paste("#", input$theAUC_colour_post, sep = ""),
      paste("#", input$theAUC_colour_rb, sep = ""))
  }
})

output$theAUC_postprior_graph = renderPlot({
  if(check.numeric(input$theAUC_gamma) == FALSE){
    density_hist_AUC_prior_post(delta = input$theAUC_delta, 
                                AUC_prior = sect3.2_AUC_prior()$AUC, 
                                AUC_post = sect3.2_AUC_post()$AUC, 
                                plausible_region = sect3.2_pr()$plausible_region,
                                densityplot = TRUE, 
                                showbars = showbarplots(),
                                colour_choice = theAUC_colours())
    
  } else if (as.numeric(input$theAUC_gamma) >= sect3.2_AUC_post_content()){
    density_hist_AUC_prior_post(delta = input$theAUC_delta, 
                                AUC_prior = sect3.2_AUC_prior()$AUC, 
                                AUC_post = sect3.2_AUC_post()$AUC, 
                                plausible_region = sect3.2_pr()$plausible_region,
                                densityplot = TRUE, 
                                showbars = showbarplots(),
                                colour_choice = theAUC_colours())
  } else {
    density_hist_AUC_prior_post(delta = input$theAUC_delta, 
                                AUC_prior = sect3.2_AUC_prior()$AUC, 
                                AUC_post = sect3.2_AUC_post()$AUC, 
                                plausible_region = sect3.2_pr()$plausible_region,
                                credible_region = sect3.2_cr()$credible_region,
                                densityplot = TRUE, 
                                showbars = showbarplots(),
                                colour_choice = theAUC_colours()) # MUST MODIFY
  }
})

output$theAUC_RB_graph = renderPlot({
  if(check.numeric(input$theAUC_gamma) == FALSE){
    density_hist_AUC_RBR(delta = input$theAUC_delta, 
                         AUC_RBR = sect3.2_AUC_RBR()$AUC_RBR, 
                         plausible_region = sect3.2_pr()$plausible_region, 
                         densityplot = TRUE,
                         showbars = showbarplots())
  } else if (as.numeric(input$theAUC_gamma) >= sect3.2_AUC_post_content()){
    density_hist_AUC_RBR(delta = input$theAUC_delta, 
                         AUC_RBR = sect3.2_AUC_RBR()$AUC_RBR, 
                         plausible_region = sect3.2_pr()$plausible_region, 
                         densityplot = TRUE,
                         showbars = showbarplots())
  } else {
    density_hist_AUC_RBR(delta = input$theAUC_delta, 
                         AUC_RBR = sect3.2_AUC_RBR()$AUC_RBR, 
                         plausible_region = sect3.2_pr()$plausible_region,
                         credible_region = sect3.2_cr()$credible_region, 
                         rb_line = sect3.2_cr()$rb_line,
                         densityplot = TRUE,
                         showbars = showbarplots()) # MUST MODIFY
  }
})
####### COPT PLOTS
output$theAUC_postprior_copt_graph = renderPlot({
  plots_AUC_copt(priorc_opt = sect3.2_AUC_prior()$priorc_opt, 
                 postc_opt = sect3.2_AUC_post()$postc_opt,
                 prior_label = as.numeric(input$theAUC_priorc_opt_label), 
                 post_label = as.numeric(input$theAUC_postc_opt_label))
})
output$theAUC_RB_copt_graph = renderPlot({
  plots_AUC_copt(RBc_opt = sect3.2_AUC_RBR()$RBc_opt,
                 rb_label = as.numeric(input$theAUC_rbc_opt_label))
})


################################################################
# DOWNLOAD DATAFRAME                                           #
################################################################

theAUC_download = reactive({
  theAUC_generate_dataframe(input$theAUC_delta, 
                            sect3.2_AUC_prior()$AUC, 
                            sect3.2_AUC_post()$AUC, 
                            sect3.2_AUC_RBR()$AUC_RBR)
})

output$theAUC_dataframe = renderDataTable({
  theAUC_download()
})

output$theAUC_downloadData = downloadHandler(
  filename = function() {
    paste(input$theAUC_filename, ".csv", sep = "")
  },
  content = function(file) {
    write.csv(theAUC_download(), file, row.names = FALSE)
  }
)
