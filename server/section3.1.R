################################################################
# VARIABLES                                                    #
################################################################

sect_3.1_grid = reactive({
  RB_distance_that_matters(input$RB_delta)
})

sect_3.1_info_1 = reactive({
  RBR_compute_values(alpha1w = input$RB_setup_alpha1w, 
                     alpha2w = input$RB_setup_alpha2w, 
                     n = input$RB_setup_n, 
                     nD = input$RB_setup_nD, 
                     grid = sect_3.1_grid())
})

sect_3.1_info_2 = reactive({
  w0_compute_values(alpha1w = input$RB_setup_alpha1w, 
                    alpha2w = input$RB_setup_alpha2w, 
                    n = input$RB_setup_n, 
                    nD = input$RB_setup_nD, 
                    w0 = input$RB_setup_w0, 
                    relative_belief_ratio = sect_3.1_info_1()$relative_belief_ratio, 
                    grid = sect_3.1_grid())
})

sect_3.1_cred_region = reactive({
  compute_credible_region(alpha1w = input$RB_setup_alpha1w, 
                          alpha2w = input$RB_setup_alpha2w, 
                          n = input$RB_setup_n, 
                          nD = input$RB_setup_nD, 
                          grid = sect_3.1_grid(), 
                          gamma = input$RB_gamma, 
                          delta = input$RB_delta, 
                          relative_belief_ratio = sect_3.1_info_1()$relative_belief_ratio, 
                          posterior_content = sect_3.1_info_1()$posterior_content, 
                          plausible_region = sect_3.1_info_1()$plausible_region)
})

################################################################
# NUMERIC/TEXT OUTPUTS                                         #
################################################################

output$RB_setup_values1 = renderPrint({
  list("plausible_region" = sect_3.1_info_1()$plausible_region,
       "RB_estimate_of_prevalence_w" = sect_3.1_info_1()$RB_estimate_of_prevalence_w,
       "prior_content" = sect_3.1_info_1()$prior_content,
       "posterior_content" = sect_3.1_info_1()$posterior_content,
       "credible_region" = sect_3.1_cred_region()$credible_region,
       "rb_line" = sect_3.1_cred_region()$rb_line)
})

output$RB_setup_values2 = renderPrint({
  #sect_3.1_info_2()
  list("relative_belief_ratio_at_w0" = sect_3.1_info_2()$relative_belief_ratio_at_w0,
       "strength" = sect_3.1_info_2()$strength)
})

################################################################
# PLOTS                                                        #
################################################################

output$RB_setup_postprior_graph = renderPlot({
  if(check.numeric(input$RB_gamma) == FALSE){
    generate_prior_post_graph(prior = sect_3.1_info_1()$prior, 
                              post = sect_3.1_info_1()$post, 
                              plausible_region = sect_3.1_info_1()$plausible_region, 
                              grid = sect_3.1_grid())
  } else if (as.numeric(input$RB_gamma) >= sect_3.1_info_1()$posterior_content){
    # Couldn't do the or statement for if because of the case where you can't do
    # as.numeric() for input$gamma
    generate_prior_post_graph(prior = sect_3.1_info_1()$prior, 
                              post = sect_3.1_info_1()$post, 
                              plausible_region = sect_3.1_info_1()$plausible_region, 
                              grid = sect_3.1_grid())
  } else {
    generate_prior_post_graph(prior = sect_3.1_info_1()$prior, 
                              post = sect_3.1_info_1()$post, 
                              plausible_region = sect_3.1_info_1()$plausible_region, 
                              grid = sect_3.1_grid(),
                              credible_region = sect_3.1_cred_region()$credible_region)
  }
})

output$RB_setup_RB_graph = renderPlot({
  if(check.numeric(input$RB_gamma) == FALSE){
    generate_rbr_graph(relative_belief_ratio = sect_3.1_info_1()$relative_belief_ratio, 
                       plausible_region = sect_3.1_info_1()$plausible_region, 
                       grid = sect_3.1_grid())
  } else if (as.numeric(input$RB_gamma) >= sect_3.1_info_1()$posterior_content){
    generate_rbr_graph(relative_belief_ratio = sect_3.1_info_1()$relative_belief_ratio, 
                       plausible_region = sect_3.1_info_1()$plausible_region, 
                       grid = sect_3.1_grid())
  } else {
    generate_rbr_graph(relative_belief_ratio = sect_3.1_info_1()$relative_belief_ratio, 
                       plausible_region = sect_3.1_info_1()$plausible_region, 
                       grid = sect_3.1_grid(),
                       credible_region = sect_3.1_cred_region()$credible_region, 
                       rb_line = sect_3.1_cred_region()$rb_line)
  }
})

output$RB_setup_w0_graph = renderPlot({
  generate_relative_belief_ratio_at_w0_graph(
    relative_belief_ratio = sect_3.1_info_1()$relative_belief_ratio, 
    relative_belief_ratio_at_w0 = sect_3.1_info_2()$relative_belief_ratio_at_w0,
    w0_interval = sect_3.1_info_2()$w0_interval, 
    grid = sect_3.1_grid())
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