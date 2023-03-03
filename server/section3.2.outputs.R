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
    # Total order of ALL colours: prior, posterior, relative belief ratio, 
    # plausible region, y = 1 line, credible region, 
    c("#FF6666", "#6699FF", "#05DEB2", "#947aff", "#3333FF", "#5b10a7")
  } else if (input$theAUC_colour == 'manual'){
    c(convert_to_hex(input$theAUC_colour_prior),
      convert_to_hex(input$theAUC_colour_post),
      convert_to_hex(input$theAUC_colour_rbr),
      convert_to_hex(input$theAUC_colour_pr),
      convert_to_hex(input$theAUC_colour_line_1),
      convert_to_hex(input$theAUC_colour_cr)
    )
  }
})

theAUC_copt_colours = reactive({
  if(input$theAUC_c_opt_carry_colour == 'default'){
    c("#FF6666", "#6699FF", "#05DEB2")
  } else if (input$theAUC_c_opt_carry_colour == 'custom'){
    theAUC_colours()[c(1, 2, 3)]
  } else if (input$theAUC_c_opt_carry_colour == 'manual'){
    c(convert_to_hex(input$theAUC_priorc_opt_colour),
      convert_to_hex(input$theAUC_postc_opt_colour),
      convert_to_hex(input$theAUC_rbrc_opt_colour))
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
                                colour_choice = theAUC_colours()[c(1, 2, 4, 6)],
                                transparency = input$theAUC_col_transparency)
    
  } else if (as.numeric(input$theAUC_gamma) >= sect3.2_AUC_post_content()){
    density_hist_AUC_prior_post(delta = input$theAUC_delta, 
                                AUC_prior = sect3.2_AUC_prior()$AUC, 
                                AUC_post = sect3.2_AUC_post()$AUC, 
                                plausible_region = sect3.2_pr()$plausible_region,
                                densityplot = TRUE, 
                                showbars = showbarplots(),
                                colour_choice = theAUC_colours()[c(1, 2, 4, 6)],
                                transparency = input$theAUC_col_transparency)
  } else {
    density_hist_AUC_prior_post(delta = input$theAUC_delta, 
                                AUC_prior = sect3.2_AUC_prior()$AUC, 
                                AUC_post = sect3.2_AUC_post()$AUC, 
                                plausible_region = sect3.2_pr()$plausible_region,
                                credible_region = sect3.2_cr()$credible_region,
                                densityplot = TRUE, 
                                showbars = showbarplots(),
                                colour_choice = theAUC_colours()[c(1, 2, 4, 6)],
                                transparency = input$theAUC_col_transparency)
  }
})

output$theAUC_RB_graph = renderPlot({
  if(check.numeric(input$theAUC_gamma) == FALSE){
    density_hist_AUC_RBR(delta = input$theAUC_delta, 
                         AUC_RBR = sect3.2_AUC_RBR()$AUC_RBR, 
                         plausible_region = sect3.2_pr()$plausible_region, 
                         densityplot = TRUE,
                         showbars = showbarplots(),
                         colour_choice = theAUC_colours()[c(3, 4, 5, 6)],
                         transparency = input$theAUC_col_transparency)
  } else if (as.numeric(input$theAUC_gamma) >= sect3.2_AUC_post_content()){
    density_hist_AUC_RBR(delta = input$theAUC_delta, 
                         AUC_RBR = sect3.2_AUC_RBR()$AUC_RBR, 
                         plausible_region = sect3.2_pr()$plausible_region, 
                         densityplot = TRUE,
                         showbars = showbarplots(),
                         colour_choice = theAUC_colours()[c(3, 4, 5, 6)],
                         transparency = input$theAUC_col_transparency)
  } else {
    density_hist_AUC_RBR(delta = input$theAUC_delta, 
                         AUC_RBR = sect3.2_AUC_RBR()$AUC_RBR, 
                         plausible_region = sect3.2_pr()$plausible_region,
                         credible_region = sect3.2_cr()$credible_region, 
                         rb_line = sect3.2_cr()$rb_line,
                         densityplot = TRUE,
                         showbars = showbarplots(),
                         colour_choice = theAUC_colours()[c(3, 4, 5, 6)],
                         transparency = input$theAUC_col_transparency)
  }
})
####### COPT PLOTS
output$theAUC_postprior_copt_graph = renderPlot({
  plots_AUC_copt(priorc_opt = sect3.2_AUC_prior()$priorc_opt, 
                 postc_opt = sect3.2_AUC_post()$postc_opt,
                 prior_label = as.numeric(input$theAUC_priorc_opt_label), 
                 post_label = as.numeric(input$theAUC_postc_opt_label),
                 colour_choice = theAUC_copt_colours())
})
output$theAUC_RB_copt_graph = renderPlot({
  plots_AUC_copt(RBc_opt = sect3.2_AUC_RBR()$RBc_opt,
                 rb_label = as.numeric(input$theAUC_rbc_opt_label),
                 colour_choice = theAUC_copt_colours())
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
