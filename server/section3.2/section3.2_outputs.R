################################################################
# NUMERIC/TEXT OUTPUTS                                         #
################################################################

output$finite_val_hypoAUC_value = renderPrint({
  list1 = list("Actual Estimate of the AUC from the Relative Belief Ratio" = RBR_estimate_of_AUC(closed_bracket_grid(input$finite_val_delta), sect3.2_AUC_RBR()$AUC_RBR))
  list3 = list("Plausible Region for the AUC" = sect3.2_pr()$plausible_region,
               "Posterior Content of the Plausible Region for the AUC" = sect3.2_AUC_post_content(),
               "Credible region for the AUC" = sect3.2_cr()$credible_region,
               "Area Under the Line Plot (next section)" = sect3.2_lineplot_area())
  list2 = append(list1, sect3.2_hypo_test())
  list2 = append(list2, list3)
  list2
})

output$finite_val_output1 = renderPrint({
  list("Prior Copt" = sect3.2_AUC_prior()$priorc_opt,
       "Post Copt" = sect3.2_AUC_post()$postc_opt,
       "Copt Estimate" = sect3.2_AUC_RBR()$c_optfDfND,
       "Error Characteristics" = as.data.frame(sect3.2_copt_est())) # will need to specify when
  # providing a vector
  # TODO: specify credible region
})

################################################################
# HISTOGRAMS                                                   #
################################################################

finite_val_colours = reactive({
  # Total order of ALL colours: prior, posterior, relative belief ratio, 
  # plausible region, y = 1 line, credible region, 
  if(input$finite_val_colour == 'default1'){
    c("#FF6666", "#6699FF", "#05DEB2", "#947aff", "#3333FF", "#5b10a7")
  } else if (input$finite_val_colour == 'default2'){
    c("blue", "green", "red", "#b3bfff", "royalblue1", "#81ddff")
  } else if (input$finite_val_colour == 'dull'){
    c("#EE4266", "#3cbbb1", "#b33c86", "#403f4c", "#0a0f0d", "#3185fc")
  } else if (input$finite_val_colour == 'lovelymei'){
    c("#3800c2", "#676bf8", "#58887a", "#e69eb7", "#372f66", "#a2cda3")
  } else if (input$finite_val_colour == 'manual'){
    c(convert_to_hex(input$finite_val_colour_prior),
      convert_to_hex(input$finite_val_colour_post),
      convert_to_hex(input$finite_val_colour_rbr),
      convert_to_hex(input$finite_val_colour_pr),
      convert_to_hex(input$finite_val_colour_line_1),
      convert_to_hex(input$finite_val_colour_cr)
    )
  }
})

finite_val_copt_colours = reactive({
  if(input$finite_val_c_opt_carry_colour == 'default1'){
    c("#FF6666", "#6699FF", "#05DEB2")
  } else if (input$finite_val_c_opt_carry_colour == 'default2'){
    c("blue", "green", "red")
  } else if (input$finite_val_c_opt_carry_colour == 'dull'){
    c("#EE4266", "#3cbbb1", "#b33c86", "#403f4c", "#0a0f0d", "#3185fc")
  } else if (input$finite_val_c_opt_carry_colour == 'lovelymei'){
    c("#3800c2", "#676bf8", "#58887a", "#e69eb7", "#372f66", "#a2cda3")
  } else if (input$finite_val_c_opt_carry_colour == 'custom'){
    finite_val_colours()[c(1, 2, 3)]
  } else if (input$finite_val_c_opt_carry_colour == 'manual'){
    c(convert_to_hex(input$finite_val_priorc_opt_colour),
      convert_to_hex(input$finite_val_postc_opt_colour),
      convert_to_hex(input$finite_val_rbrc_opt_colour))
  }
})

sect3.2_prior_post_lty = reactive({
  c(as.numeric(input$finite_val_lty_prior),
    as.numeric(input$finite_val_lty_post),
    as.numeric(input$finite_val_lty_pr),
    as.numeric(input$finite_val_lty_cr))
})

sect3.2_rbr_lty = reactive({
  c(as.numeric(input$finite_val_lty_rbr),
    as.numeric(input$finite_val_lty_pr),
    as.numeric(input$finite_val_lty_line_1),
    as.numeric(input$finite_val_lty_cr))
})

output$finite_val_postprior_graph = renderPlot({
  if(check.numeric(input$finite_val_gamma) == FALSE){
    density_hist_AUC_prior_post(delta = input$finite_val_delta, 
                                AUC_prior = sect3.2_AUC_prior()$AUC, 
                                AUC_post = sect3.2_AUC_post()$AUC, 
                                plausible_region = sect3.2_pr()$plausible_region,
                                densityplot = TRUE, 
                                showbars = showbarplots(),
                                colour_choice = finite_val_colours()[c(1, 2, 4, 6)],
                                lty_type = sect3.2_prior_post_lty(), 
                                transparency = input$finite_val_col_transparency)
    
  } else if (as.numeric(input$finite_val_gamma) >= sect3.2_AUC_post_content()){
    density_hist_AUC_prior_post(delta = input$finite_val_delta, 
                                AUC_prior = sect3.2_AUC_prior()$AUC, 
                                AUC_post = sect3.2_AUC_post()$AUC, 
                                plausible_region = sect3.2_pr()$plausible_region,
                                densityplot = TRUE, 
                                showbars = showbarplots(),
                                colour_choice = finite_val_colours()[c(1, 2, 4, 6)],
                                lty_type = sect3.2_prior_post_lty(), 
                                transparency = input$finite_val_col_transparency)
  } else {
    density_hist_AUC_prior_post(delta = input$finite_val_delta, 
                                AUC_prior = sect3.2_AUC_prior()$AUC, 
                                AUC_post = sect3.2_AUC_post()$AUC, 
                                plausible_region = sect3.2_pr()$plausible_region,
                                credible_region = sect3.2_cr()$credible_region,
                                densityplot = TRUE, 
                                showbars = showbarplots(),
                                colour_choice = finite_val_colours()[c(1, 2, 4, 6)],
                                lty_type = sect3.2_prior_post_lty(), 
                                transparency = input$finite_val_col_transparency)
  }
})

output$finite_val_RB_graph = renderPlot({
  if(check.numeric(input$finite_val_gamma) == FALSE){
    density_hist_AUC_RBR(delta = input$finite_val_delta, 
                         AUC_RBR = sect3.2_AUC_RBR()$AUC_RBR, 
                         plausible_region = sect3.2_pr()$plausible_region, 
                         densityplot = TRUE,
                         showbars = showbarplots(),
                         colour_choice = finite_val_colours()[c(3:6)],
                         lty_type = sect3.2_rbr_lty(), 
                         transparency = input$finite_val_col_transparency)
  } else if (as.numeric(input$finite_val_gamma) >= sect3.2_AUC_post_content()){
    density_hist_AUC_RBR(delta = input$finite_val_delta, 
                         AUC_RBR = sect3.2_AUC_RBR()$AUC_RBR, 
                         plausible_region = sect3.2_pr()$plausible_region, 
                         densityplot = TRUE,
                         showbars = showbarplots(),
                         colour_choice = finite_val_colours()[c(3:6)],
                         lty_type = sect3.2_rbr_lty(), 
                         transparency = input$finite_val_col_transparency)
  } else {
    density_hist_AUC_RBR(delta = input$finite_val_delta, 
                         AUC_RBR = sect3.2_AUC_RBR()$AUC_RBR, 
                         plausible_region = sect3.2_pr()$plausible_region,
                         credible_region = sect3.2_cr()$credible_region, 
                         rb_line = sect3.2_cr()$rb_line,
                         densityplot = TRUE,
                         showbars = showbarplots(),
                         colour_choice = finite_val_colours()[c(3:6)],
                         lty_type = sect3.2_rbr_lty(), 
                         transparency = input$finite_val_col_transparency)
  }
})

output$finite_val_postprior_copt_graph = renderPlot({
  plots_AUC_copt(priorc_opt = sect3.2_AUC_prior()$priorc_opt, 
                 postc_opt = sect3.2_AUC_post()$postc_opt,
                 prior_label = as.numeric(input$finite_val_priorc_opt_label), 
                 post_label = as.numeric(input$finite_val_postc_opt_label),
                 colour_choice = finite_val_copt_colours())
})
output$finite_val_RB_copt_graph = renderPlot({
  plots_AUC_copt(RBc_opt = sect3.2_AUC_RBR()$RBc_opt,
                 rb_label = as.numeric(input$finite_val_rbc_opt_label),
                 colour_choice = finite_val_copt_colours())
})


################################################################
# DOWNLOAD DATAFRAME                                           #
################################################################

finite_val_download = reactive({
  finite_val_generate_dataframe(input$finite_val_delta, 
                            sect3.2_AUC_prior()$AUC, 
                            sect3.2_AUC_post()$AUC, 
                            sect3.2_AUC_RBR()$AUC_RBR)
})

output$finite_val_dataframe = renderDataTable({
  finite_val_download()
})

output$finite_val_downloadData = downloadHandler(
  filename = function() {
    paste(input$finite_val_filename, ".csv", sep = "")
  },
  content = function(file) {
    write.csv(finite_val_download(), file, row.names = FALSE)
  }
)
