################################################################
# VARIABLES                                                    #
################################################################

sect3.2_prevalence_grid = reactive({
  if(input$finite_val_diag_case2 == 'A'){
    open_bracket_grid(input$finite_val_diag_prevalence_delta_alt)
  } else {
    open_bracket_grid(input$finite_val_diag_prevalence_delta)
  }
})

sect3.2_prevalence_info_1 = reactive({
  RBR_compute_values(alpha1w = input$finite_val_diag_prevalence_alpha1w, 
                     alpha2w = input$finite_val_diag_prevalence_alpha2w, 
                     n = input$finite_val_diag_prevalence_n, 
                     nD = input$finite_val_diag_prevalence_nD, 
                     grid = sect3.2_prevalence_grid())
})

sect3.2_prevalence_info_2 = reactive({
  w0_compute_values(alpha1w = input$finite_val_diag_prevalence_alpha1w, 
                    alpha2w = input$finite_val_diag_prevalence_alpha2w, 
                    n = input$finite_val_diag_prevalence_n, 
                    nD = input$finite_val_diag_prevalence_nD, 
                    w0 = input$finite_val_diag_prevalence_w0, 
                    relative_belief_ratio = sect3.2_prevalence_info_1()$relative_belief_ratio, 
                    grid = sect3.2_prevalence_grid())
})

sect3.2_prevalence_cred_region = reactive({
  compute_credible_region(alpha1w = input$finite_val_diag_prevalence_alpha1w, 
                          alpha2w = input$finite_val_diag_prevalence_alpha2w, 
                          n = input$finite_val_diag_prevalence_n, 
                          nD = input$finite_val_diag_prevalence_nD, 
                          grid = sect3.2_prevalence_grid(), 
                          gamma = input$finite_val_diag_prevalence_gamma, 
                          delta = input$finite_val_diag_prevalence_delta, 
                          relative_belief_ratio = sect3.2_prevalence_info_1()$relative_belief_ratio, 
                          posterior_content = sect3.2_prevalence_info_1()$posterior_content, 
                          plausible_region = sect3.2_prevalence_info_1()$plausible_region)
})

# This is for the prior case only
sect3.2_prevalence_prior = reactive({
  prior_compute_values(alpha1w = input$finite_val_diag_prevalence_alpha1w,
                       alpha2w = input$finite_val_diag_prevalence_alpha2w, 
                       grid = sect3.2_prevalence_grid())
})

################################################################
# NUMERIC/TEXT OUTPUTS                                         #
################################################################

output$finite_val_diag_prevalence_values1 = renderPrint({
  list("Plausible Region of Prevalence w" = sect3.2_prevalence_info_1()$plausible_region,
       "Relative Belief Estimate of Prevalence w" = sect3.2_prevalence_info_1()$RB_estimate_of_prevalence_w,
       "Prior Content" = sect3.2_prevalence_info_1()$prior_content,
       "Posterior Content" = sect3.2_prevalence_info_1()$posterior_content,
       "Credible Region of Prevalence w" = sect3.2_prevalence_cred_region()$credible_region,
       "Max RBR Value for the Credible Region" = sect3.2_prevalence_cred_region()$rb_line)
})

output$finite_val_diag_prevalence_values2 = renderPrint({
  #sect3.2_prevalence_info_2()
  list("Relative Belief Ratio at w0" = sect3.2_prevalence_info_2()$relative_belief_ratio_at_w0,
       "Strength" = sect3.2_prevalence_info_2()$strength)
})

output$finite_val_diag_prevalence_prior_values = renderPrint({
  list(
    "Relative Belief Estimate of Prevalence w" = (match(max(sect3.2_prevalence_prior()), sect3.2_prevalence_prior())/length(sect3.2_prevalence_grid()))
  )
})

################################################################
# PLOTS                                                        #
################################################################

sect3.2_prevalence_colours = reactive({
  # Total order of ALL colours: prior, posterior, relative belief ratio, 
  # plausible region, y = 1 line, credible region, 
  if(input$finite_val_diag_prevalence_colour == 'default1'){
    c("#FF6666", "#6699FF", "#05DEB2", "#947aff", "#3333FF", "#5b10a7")
  } else if (input$finite_val_diag_prevalence_colour == 'default2'){
    c("blue", "green", "red", "#b3bfff", "royalblue1", "#81ddff")
  } else if (input$finite_val_diag_prevalence_colour == 'dull'){
    c("#EE4266", "#3cbbb1", "#b33c86", "#403f4c", "#0a0f0d", "#3185fc")
  } else if (input$finite_val_diag_prevalence_colour == 'lovelymei'){
    c("#3800c2", "#676bf8", "#58887a", "#e69eb7", "#372f66", "#a2cda3")
  } else if (input$finite_val_diag_prevalence_colour == 'manual'){
    #c("#FF007F", "#FF00FF", "#7F00FF")
    c(convert_to_hex(input$finite_val_diag_prevalence_colour_prior),
      convert_to_hex(input$finite_val_diag_prevalence_colour_post),
      convert_to_hex(input$finite_val_diag_prevalence_colour_rbr),
      convert_to_hex(input$finite_val_diag_prevalence_colour_pr),
      convert_to_hex(input$finite_val_diag_prevalence_colour_line_1),
      convert_to_hex(input$finite_val_diag_prevalence_colour_cr)
    )
  }
})

output$finite_val_diag_prevalence_postprior_graph = renderPlot({
  if(check.numeric(input$finite_val_diag_prevalence_gamma) == FALSE){
    generate_prior_post_graph(prior = sect3.2_prevalence_info_1()$prior, 
                              post = sect3.2_prevalence_info_1()$post, 
                              plausible_region = sect3.2_prevalence_info_1()$plausible_region, 
                              grid = sect3.2_prevalence_grid(),
                              colour_choice = sect3.2_prevalence_colours()[c(1, 2, 4, 6)],
                              transparency = input$finite_val_diag_prevalence_col_transparency)
  } else if (as.numeric(input$finite_val_diag_prevalence_gamma) >= sect3.2_prevalence_info_1()$posterior_content){
    # Couldn't do the or statement for if because of the case where you can't do
    # as.numeric() for input$gamma
    generate_prior_post_graph(prior = sect3.2_prevalence_info_1()$prior, 
                              post = sect3.2_prevalence_info_1()$post, 
                              plausible_region = sect3.2_prevalence_info_1()$plausible_region, 
                              grid = sect3.2_prevalence_grid(),
                              colour_choice = sect3.2_prevalence_colours()[c(1, 2, 4, 6)],
                              transparency = input$finite_val_diag_prevalence_col_transparency)
  } else {
    generate_prior_post_graph(prior = sect3.2_prevalence_info_1()$prior, 
                              post = sect3.2_prevalence_info_1()$post, 
                              plausible_region = sect3.2_prevalence_info_1()$plausible_region, 
                              grid = sect3.2_prevalence_grid(),
                              credible_region = sect3.2_prevalence_cred_region()$credible_region,
                              colour_choice = sect3.2_prevalence_colours()[c(1, 2, 4, 6)],
                              transparency = input$finite_val_diag_prevalence_col_transparency)
  }
})

output$finite_val_diag_prevalence_RB_graph = renderPlot({
  if(check.numeric(input$finite_val_diag_prevalence_gamma) == FALSE){
    generate_rbr_graph(relative_belief_ratio = sect3.2_prevalence_info_1()$relative_belief_ratio, 
                       plausible_region = sect3.2_prevalence_info_1()$plausible_region, 
                       grid = sect3.2_prevalence_grid(),
                       colour_choice = sect3.2_prevalence_colours()[c(3:6)],
                       transparency = input$finite_val_diag_prevalence_col_transparency)
  } else if (as.numeric(input$finite_val_diag_prevalence_gamma) >= sect3.2_prevalence_info_1()$posterior_content){
    generate_rbr_graph(relative_belief_ratio = sect3.2_prevalence_info_1()$relative_belief_ratio, 
                       plausible_region = sect3.2_prevalence_info_1()$plausible_region, 
                       grid = sect3.2_prevalence_grid(),
                       colour_choice = sect3.2_prevalence_colours()[c(3:6)],
                       transparency = input$finite_val_diag_prevalence_col_transparency)
  } else {
    generate_rbr_graph(relative_belief_ratio = sect3.2_prevalence_info_1()$relative_belief_ratio, 
                       plausible_region = sect3.2_prevalence_info_1()$plausible_region, 
                       grid = sect3.2_prevalence_grid(),
                       credible_region = sect3.2_prevalence_cred_region()$credible_region, 
                       rb_line = sect3.2_prevalence_cred_region()$rb_line,
                       colour_choice = sect3.2_prevalence_colours()[c(3:6)],
                       transparency = input$finite_val_diag_prevalence_col_transparency)
  }
})

output$finite_val_diag_prevalence_w0_graph = renderPlot({
  generate_relative_belief_ratio_at_w0_graph(
    relative_belief_ratio = sect3.2_prevalence_info_1()$relative_belief_ratio, 
    relative_belief_ratio_at_w0 = sect3.2_prevalence_info_2()$relative_belief_ratio_at_w0,
    w0_interval = sect3.2_prevalence_info_2()$w0_interval, 
    grid = sect3.2_prevalence_grid())
})

# This is for the prior case only
output$finite_val_diag_prevalence_post_graph_alt = renderPlot({
  if(input$finite_val_diag_prevalence_colour_1 != 'manual'){
    generate_prior_graph(prior = sect3.2_prevalence_prior(), 
                         grid = sect3.2_prevalence_grid(),
                         colour_choice = input$finite_val_diag_prevalence_colour_1,
                         lty_type = as.numeric(input$finite_val_diag_prevalence_lty_1),
                         transparency = input$finite_val_diag_prevalence_col_transparency_1)
  } else if (input$finite_val_diag_prevalence_colour_1 == 'manual'){
    generate_prior_graph(prior = sect3.2_prevalence_prior(), 
                         grid = sect3.2_prevalence_grid(),
                         colour_choice = paste("#", input$finite_val_diag_prevalence_colour_2, sep = ""),
                         lty_type = as.numeric(input$finite_val_diag_prevalence_lty_1),
                         transparency = input$finite_val_diag_prevalence_col_transparency_1)
  }
})


################################################################
# DOWNLOAD DATAFRAME                                           #
################################################################

finite_val_diag_prevalence_download = reactive({
  RB_generate_dataframe(sect3.2_prevalence_grid(), 
                        sect3.2_prevalence_info_1()$prior, 
                        sect3.2_prevalence_info_1()$post, 
                        sect3.2_prevalence_info_1()$relative_belief)
})

output$finite_val_diag_prevalence_dataframe <- renderDataTable({
  finite_val_diag_prevalence_download()
})

output$finite_val_diag_prevalence_downloadData <- downloadHandler(
  filename = function() {
    paste(input$finite_val_diag_prevalence_filename, ".csv", sep = "")
  },
  content = function(file) {
    write.csv(finite_val_diag_prevalence_download(), file, row.names = FALSE)
  }
)

# This is for the prior only case
finite_val_diag_prevalence_download_prior = reactive({
  RB_generate_priorframe(grid = sect3.2_prevalence_grid(),
                         prior = sect3.2_prevalence_prior())
})

output$finite_val_diag_prevalence_dataframe_alt <- renderDataTable({
  finite_val_diag_prevalence_download_prior()
})

output$finite_val_diag_prevalence_downloadData_alt <- downloadHandler(
  filename = function() {
    paste(input$finite_val_diag_prevalence_filename_alt, ".csv", sep = "")
  },
  content = function(file) {
    write.csv(finite_val_diag_prevalence_download_prior(), file, row.names = FALSE)
  }
)