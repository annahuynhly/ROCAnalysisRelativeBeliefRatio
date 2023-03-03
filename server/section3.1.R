################################################################
# VARIABLES                                                    #
################################################################

sect_3.1_grid = reactive({
  RB_distance_that_matters(input$prevalence_setup_delta)
})

sect_3.1_info_1 = reactive({
  RBR_compute_values(alpha1w = input$prevalence_setup_alpha1w, 
                     alpha2w = input$prevalence_setup_alpha2w, 
                     n = input$prevalence_setup_n, 
                     nD = input$prevalence_setup_nD, 
                     grid = sect_3.1_grid())
})

sect_3.1_info_2 = reactive({
  w0_compute_values(alpha1w = input$prevalence_setup_alpha1w, 
                    alpha2w = input$prevalence_setup_alpha2w, 
                    n = input$prevalence_setup_n, 
                    nD = input$prevalence_setup_nD, 
                    w0 = input$prevalence_setup_w0, 
                    relative_belief_ratio = sect_3.1_info_1()$relative_belief_ratio, 
                    grid = sect_3.1_grid())
})

sect_3.1_cred_region = reactive({
  compute_credible_region(alpha1w = input$prevalence_setup_alpha1w, 
                          alpha2w = input$prevalence_setup_alpha2w, 
                          n = input$prevalence_setup_n, 
                          nD = input$prevalence_setup_nD, 
                          grid = sect_3.1_grid(), 
                          gamma = input$prevalence_setup_gamma, 
                          delta = input$prevalence_setup_delta, 
                          relative_belief_ratio = sect_3.1_info_1()$relative_belief_ratio, 
                          posterior_content = sect_3.1_info_1()$posterior_content, 
                          plausible_region = sect_3.1_info_1()$plausible_region)
})

# This is for the prior case only
sect_3.1_prior = reactive({
  prior_compute_values(alpha1w = input$prevalence_setup_alpha1w,
                       alpha2w = input$prevalence_setup_alpha2w, 
                       grid = sect_3.1_grid())
})

################################################################
# NUMERIC/TEXT OUTPUTS                                         #
################################################################

output$prevalence_setup_values1 = renderPrint({
  list("Plausible Region of Prevalence w" = sect_3.1_info_1()$plausible_region,
       "Relatibe Belief Estimate of Prevalence w" = sect_3.1_info_1()$RB_estimate_of_prevalence_w,
       "Prior Content" = sect_3.1_info_1()$prior_content,
       "Posterior Content" = sect_3.1_info_1()$posterior_content,
       "Credible Region of Prevalence w" = sect_3.1_cred_region()$credible_region,
       "Max RBR Value for the Credible Region" = sect_3.1_cred_region()$rb_line)
})

output$prevalence_setup_values2 = renderPrint({
  #sect_3.1_info_2()
  list("Relative Belief Ratio at w0" = sect_3.1_info_2()$relative_belief_ratio_at_w0,
       "Strength" = sect_3.1_info_2()$strength)
})

output$prevalence_setup_prior_values = renderPrint({
  list(
    "Relative Belief Estimate of Prevalence w" = (match(max(sect_3.1_prior()), sect_3.1_prior())/length(sect_3.1_grid()))
  )
})

################################################################
# PLOTS                                                        #
################################################################

prevalence_colours = reactive({
  if(input$prevalence_setup_colour == 'default'){
    # Total order of ALL colours: prior, posterior, relative belief ratio, 
    # plausible region, y = 1 line, credible region, 
    c("blue", "green", "red", "#b3bfff", "royalblue1", "#81ddff")
  }
  else if (input$prevalence_setup_colour == 'manual'){
    #c("#FF007F", "#FF00FF", "#7F00FF")
    c(convert_to_hex(input$prevalence_setup_colour_prior),
      convert_to_hex(input$prevalence_setup_colour_post),
      convert_to_hex(input$prevalence_setup_colour_rbr),
      convert_to_hex(input$prevalence_setup_colour_pr),
      convert_to_hex(input$prevalence_setup_colour_line_1),
      convert_to_hex(input$prevalence_setup_colour_cr)
    )
  }
})


output$prevalence_setup_postprior_graph = renderPlot({
  if(check.numeric(input$prevalence_setup_gamma) == FALSE){
    generate_prior_post_graph(prior = sect_3.1_info_1()$prior, 
                              post = sect_3.1_info_1()$post, 
                              plausible_region = sect_3.1_info_1()$plausible_region, 
                              grid = sect_3.1_grid(),
                              colour_choice = prevalence_colours()[c(1, 2, 4, 6)],
                              transparency = input$prevalence_setup_col_transparency)
  } else if (as.numeric(input$prevalence_setup_gamma) >= sect_3.1_info_1()$posterior_content){
    # Couldn't do the or statement for if because of the case where you can't do
    # as.numeric() for input$gamma
    generate_prior_post_graph(prior = sect_3.1_info_1()$prior, 
                              post = sect_3.1_info_1()$post, 
                              plausible_region = sect_3.1_info_1()$plausible_region, 
                              grid = sect_3.1_grid(),
                              colour_choice = prevalence_colours()[c(1, 2, 4, 6)],
                              transparency = input$prevalence_setup_col_transparency)
  } else {
    generate_prior_post_graph(prior = sect_3.1_info_1()$prior, 
                              post = sect_3.1_info_1()$post, 
                              plausible_region = sect_3.1_info_1()$plausible_region, 
                              grid = sect_3.1_grid(),
                              credible_region = sect_3.1_cred_region()$credible_region,
                              colour_choice = prevalence_colours()[c(1, 2, 4, 6)],
                              transparency = input$prevalence_setup_col_transparency)
  }
})

output$prevalence_setup_RB_graph = renderPlot({
  if(check.numeric(input$prevalence_setup_gamma) == FALSE){
    generate_rbr_graph(relative_belief_ratio = sect_3.1_info_1()$relative_belief_ratio, 
                       plausible_region = sect_3.1_info_1()$plausible_region, 
                       grid = sect_3.1_grid(),
                       colour_choice = prevalence_colours()[c(3:6)],
                       transparency = input$prevalence_setup_col_transparency)
  } else if (as.numeric(input$prevalence_setup_gamma) >= sect_3.1_info_1()$posterior_content){
    generate_rbr_graph(relative_belief_ratio = sect_3.1_info_1()$relative_belief_ratio, 
                       plausible_region = sect_3.1_info_1()$plausible_region, 
                       grid = sect_3.1_grid(),
                       colour_choice = prevalence_colours()[c(3:6)],
                       transparency = input$prevalence_setup_col_transparency)
  } else {
    generate_rbr_graph(relative_belief_ratio = sect_3.1_info_1()$relative_belief_ratio, 
                       plausible_region = sect_3.1_info_1()$plausible_region, 
                       grid = sect_3.1_grid(),
                       credible_region = sect_3.1_cred_region()$credible_region, 
                       rb_line = sect_3.1_cred_region()$rb_line,
                       colour_choice = prevalence_colours()[c(3:6)],
                       transparency = input$prevalence_setup_col_transparency)
  }
})

output$prevalence_setup_w0_graph = renderPlot({
  generate_relative_belief_ratio_at_w0_graph(
    relative_belief_ratio = sect_3.1_info_1()$relative_belief_ratio, 
    relative_belief_ratio_at_w0 = sect_3.1_info_2()$relative_belief_ratio_at_w0,
    w0_interval = sect_3.1_info_2()$w0_interval, 
    grid = sect_3.1_grid())
})

# This is for the prior case only
output$prevalence_setup_post_graph_alt = renderPlot({
  if(input$prevalence_setup_colour_1 != 'manual'){
    generate_prior_graph(prior = sect_3.1_prior(), 
                         grid = sect_3.1_grid(),
                         colour_choice = input$prevalence_setup_colour_1,
                         transparency = input$prevalence_setup_col_transparency_1)
  } else if (input$prevalence_setup_colour_1 == 'manual'){
    generate_prior_graph(prior = sect_3.1_prior(), 
                         grid = sect_3.1_grid(),
                         colour_choice = paste("#", input$prevalence_setup_colour_2, sep = ""),
                         transparency = input$prevalence_setup_col_transparency_1)
  }
})


################################################################
# DOWNLOAD DATAFRAME                                           #
################################################################

RB_download = reactive({
  RB_generate_dataframe(sect_3.1_grid(), sect_3.1_info_1()$prior, sect_3.1_info_1()$post, 
                        sect_3.1_info_1()$relative_belief)
})

output$RB_dataframe <- renderDataTable({
  RB_download()
})

output$RB_downloadData <- downloadHandler(
  filename = function() {
    paste(input$RB_filename, ".csv", sep = "")
  },
  content = function(file) {
    write.csv(RB_download(), file, row.names = FALSE)
  }
)

# This is for the prior only case
RB_download_prior = reactive({
  RB_generate_priorframe(grid = sect_3.1_grid(),
                         prior = sect_3.1_prior())
})

output$RB_dataframe_alt <- renderDataTable({
  RB_download_prior()
})

output$RB_downloadData_alt <- downloadHandler(
  filename = function() {
    paste(input$RB_filename_alt, ".csv", sep = "")
  },
  content = function(file) {
    write.csv(RB_download_prior(), file, row.names = FALSE)
  }
)