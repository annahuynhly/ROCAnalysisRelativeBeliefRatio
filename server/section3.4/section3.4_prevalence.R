################################################################
# VARIABLES                                                    #
################################################################

sect3.4_prevalence_n = reactive({
  length(nonpara_bayes_df()$nondiseased) + length(nonpara_bayes_df()$diseased)
})

sect3.4_prevalence_nD = reactive({
  length(nonpara_bayes_df()$diseased)
})

sect3.4_prevalence_grid = reactive({
  if(input$nonpara_bayes_case2 == 'A'){
    open_bracket_grid(input$nonpara_bayes_prevalence_delta_alt)
  } else {
    open_bracket_grid(input$nonpara_bayes_prevalence_delta)
  }
})

sect3.4_prevalence_info_1 = reactive({
  RBR_compute_values(alpha1w = input$nonpara_bayes_prevalence_alpha1w, 
                     alpha2w = input$nonpara_bayes_prevalence_alpha2w, 
                     n = sect3.4_prevalence_n(), #input$nonpara_bayes_prevalence_n, 
                     nD = sect3.4_prevalence_nD(), #input$nonpara_bayes_prevalence_nD, 
                     grid = sect3.4_prevalence_grid())
})

sect3.4_prevalence_info_2 = reactive({
  w0_compute_values(alpha1w = input$nonpara_bayes_prevalence_alpha1w, 
                    alpha2w = input$nonpara_bayes_prevalence_alpha2w, 
                    n = sect3.4_prevalence_n(), #input$nonpara_bayes_prevalence_n, 
                    nD = sect3.4_prevalence_nD(), #input$nonpara_bayes_prevalence_nD, 
                    w0 = input$nonpara_bayes_prevalence_w0, 
                    relative_belief_ratio = sect3.4_prevalence_info_1()$relative_belief_ratio, 
                    grid = sect3.4_prevalence_grid())
})

sect3.4_prevalence_cred_region = reactive({
  compute_credible_region(alpha1w = input$nonpara_bayes_prevalence_alpha1w, 
                          alpha2w = input$nonpara_bayes_prevalence_alpha2w, 
                          n = sect3.4_prevalence_n(), #input$nonpara_bayes_prevalence_n, 
                          nD = sect3.4_prevalence_nD(), #input$nonpara_bayes_prevalence_nD, 
                          grid = sect3.4_prevalence_grid(), 
                          gamma = input$nonpara_bayes_prevalence_gamma, 
                          delta = input$nonpara_bayes_prevalence_delta, 
                          relative_belief_ratio = sect3.4_prevalence_info_1()$relative_belief_ratio, 
                          posterior_content = sect3.4_prevalence_info_1()$posterior_content, 
                          plausible_region = sect3.4_prevalence_info_1()$plausible_region)
})

# For: a sample of nD from diseased and nND from non-diseased ##

sect3.4_prevalence_prior = reactive({
  prior_compute_values(alpha1w = input$nonpara_bayes_prevalence_alpha1w,
                       alpha2w = input$nonpara_bayes_prevalence_alpha2w, 
                       grid = sect3.4_prevalence_grid())
})

sect3.4_pr_short = reactive({
  pr = sect3.4_prevalence_info_1()$plausible_region
  pr = c(pr[1], pr[length(pr)])
})

################################################################
# NUMERIC/TEXT OUTPUTS                                         #
################################################################

output$nonpara_bayes_prevalence_values1 = renderPrint({
  list("Relative Belief Estimate of Prevalence w" = sect3.4_prevalence_info_1()$RB_estimate_of_prevalence_w,
       "Plausible Region of Prevalence w" = sect3.4_pr_short(),
       "Prior Content of the Plausible Region" = sect3.4_prevalence_info_1()$prior_content,
       "Posterior Content of the Plausible Region" = sect3.4_prevalence_info_1()$posterior_content,
       "Credible Region of Prevalence w" = sect3.4_prevalence_cred_region()$credible_region,
       "RBR Value for the Credible Region" = sect3.4_prevalence_cred_region()$rb_line)
})

output$nonpara_bayes_prevalence_values2 = renderPrint({
  #sect3.4_prevalence_info_2()
  list("Relative Belief Ratio at w0" = sect3.4_prevalence_info_2()$relative_belief_ratio_at_w0,
       "Strength" = sect3.4_prevalence_info_2()$strength)
})

output$nonpara_bayes_prevalence_prior_values = renderPrint({
  list(
    "Relative Belief Estimate of Prevalence w" = (match(max(sect3.4_prevalence_prior()), sect3.4_prevalence_prior())/length(sect3.4_prevalence_grid()))
  )
})

################################################################
# PLOTS                                                        #
################################################################

# Denoting colours #############################################

sect3.4_prevalence_colours = reactive({
  # Total order of ALL colours: prior, posterior, relative belief ratio, 
  # plausible region, y = 1 line, credible region, 
  if(input$nonpara_bayes_prevalence_colour == 'default1'){
    default1
  } else if (input$nonpara_bayes_prevalence_colour == 'default2'){
    default2
  } else if (input$nonpara_bayes_prevalence_colour == 'dull'){
    dull
  } else if (input$nonpara_bayes_prevalence_colour == 'lovelymei'){
    lovelymei
  } else if (input$nonpara_bayes_prevalence_colour == 'jackin'){
    jackin_execute
  } else if (input$nonpara_bayes_prevalence_colour == 'manual'){
    #c("#FF007F", "#FF00FF", "#7F00FF")
    c(convert_to_hex(input$nonpara_bayes_prevalence_colour_prior),
      convert_to_hex(input$nonpara_bayes_prevalence_colour_post),
      convert_to_hex(input$nonpara_bayes_prevalence_colour_rbr),
      convert_to_hex(input$nonpara_bayes_prevalence_colour_line_1),
      convert_to_hex(input$nonpara_bayes_prevalence_colour_cr)
    )
  }
})

sect3.4_prevalence_w0_colours = reactive({
  if(input$nonpara_bayes_diag_prevalence_colour_w0 == 'default1'){
    c("#FF6666", "#6699FF", "#05DEB2", "#947aff")
  } else if (input$nonpara_bayes_diag_prevalence_colour_w0 == 'default2'){
    c("blue", "green", "red", "#b3bfff")
  } else if (input$nonpara_bayes_diag_prevalence_colour_w0 == 'dull'){
    c("#EE4266", "#3cbbb1", "#b33c86", "#403f4c")
  } else if (input$nonpara_bayes_diag_prevalence_colour_w0 == 'lovelymei'){
    c("#3800c2", "#676bf8", "#58887a", "#e69eb7")
  } else if (input$nonpara_bayes_diag_prevalence_colour_w0 == 'jackin'){
    c("#0092d6", "#212c57", "#f85210", "#ffc710")
  } else if (input$nonpara_bayes_diag_prevalence_colour_w0 == 'manual'){
    c(convert_to_hex(input$nonpara_bayes_diag_prevalence_colour_rbr_w0),
      convert_to_hex(input$nonpara_bayes_diag_prevalence_colour_rbr_at_w0),
      convert_to_hex(input$nonpara_bayes_diag_prevalence_colour_interval),
      convert_to_hex(input$nonpara_bayes_diag_prevalence_colour_str)
    )
  }
})

# Denoting lines ###############################################

sect3.4_prevalence_prior_post_lty = reactive({
  c(as.numeric(input$nonpara_bayes_prevalence_lty_prior),
    as.numeric(input$nonpara_bayes_prevalence_lty_post))
})

sect3.4_prevalence_rbr_lty = reactive({
  c(as.numeric(input$nonpara_bayes_prevalence_lty_rbr),
    as.numeric(input$nonpara_bayes_prevalence_lty_line_1),
    as.numeric(input$nonpara_bayes_prevalence_lty_cr))
})

sect3.4_prevalence_w0_lty = reactive({
  c(as.numeric(input$nonpara_bayes_diag_prevalence_lty_rbr_w0),
    as.numeric(input$nonpara_bayes_diag_prevalence_lty_rbr_at_w0),
    as.numeric(input$nonpara_bayes_diag_prevalence_lty_interval))
})

# Denoting plots ###############################################

output$nonpara_bayes_prevalence_postprior_graph = renderPlot({
  if(check.numeric(input$nonpara_bayes_prevalence_gamma) == FALSE){
    generate_prior_post_graph(prior = sect3.4_prevalence_info_1()$prior, 
                              post = sect3.4_prevalence_info_1()$post, 
                              grid = sect3.4_prevalence_grid(),
                              colour_choice = sect3.4_prevalence_colours()[c(1, 2)],
                              lty_type = sect3.4_prevalence_prior_post_lty(),
                              transparency = input$nonpara_bayes_prevalence_col_transparency,
                              legend_position = input$nonpara_bayes_prevalence_legend_position)
  } else if (as.numeric(input$nonpara_bayes_prevalence_gamma) >= sect3.4_prevalence_info_1()$posterior_content){
    # Couldn't do the or statement for if because of the case where you can't do
    # as.numeric() for input$gamma
    generate_prior_post_graph(prior = sect3.4_prevalence_info_1()$prior, 
                              post = sect3.4_prevalence_info_1()$post, 
                              grid = sect3.4_prevalence_grid(),
                              colour_choice = sect3.4_prevalence_colours()[c(1, 2)],
                              lty_type = sect3.4_prevalence_prior_post_lty(),
                              transparency = input$nonpara_bayes_prevalence_col_transparency,
                              legend_position = input$nonpara_bayes_prevalence_legend_position)
  } else {
    generate_prior_post_graph(prior = sect3.4_prevalence_info_1()$prior, 
                              post = sect3.4_prevalence_info_1()$post, 
                              grid = sect3.4_prevalence_grid(),
                              colour_choice = sect3.4_prevalence_colours()[c(1, 2)],
                              lty_type = sect3.4_prevalence_prior_post_lty(),
                              transparency = input$nonpara_bayes_prevalence_col_transparency,
                              legend_position = input$nonpara_bayes_prevalence_legend_position)
  }
})

output$nonpara_bayes_prevalence_RB_graph = renderPlot({
  if(check.numeric(input$nonpara_bayes_prevalence_gamma) == FALSE){
    generate_rbr_graph(relative_belief_ratio = sect3.4_prevalence_info_1()$relative_belief_ratio, 
                       grid = sect3.4_prevalence_grid(),
                       colour_choice = sect3.4_prevalence_colours()[c(3:5)],
                       lty_type = sect3.4_prevalence_rbr_lty(),
                       transparency = input$nonpara_bayes_prevalence_col_transparency,
                       legend_position = input$nonpara_bayes_prevalence_legend_position)
  } else if (as.numeric(input$nonpara_bayes_prevalence_gamma) >= sect3.4_prevalence_info_1()$posterior_content){
    generate_rbr_graph(relative_belief_ratio = sect3.4_prevalence_info_1()$relative_belief_ratio, 
                       grid = sect3.4_prevalence_grid(),
                       colour_choice = sect3.4_prevalence_colours()[c(3:5)],
                       lty_type = sect3.4_prevalence_rbr_lty(),
                       transparency = input$nonpara_bayes_prevalence_col_transparency,
                       legend_position = input$nonpara_bayes_prevalence_legend_position)
  } else {
    generate_rbr_graph(relative_belief_ratio = sect3.4_prevalence_info_1()$relative_belief_ratio, 
                       grid = sect3.4_prevalence_grid(),
                       rb_line = sect3.4_prevalence_cred_region()$rb_line,
                       colour_choice = sect3.4_prevalence_colours()[c(3:5)],
                       lty_type = sect3.4_prevalence_rbr_lty(),
                       transparency = input$nonpara_bayes_prevalence_col_transparency,
                       legend_position = input$nonpara_bayes_prevalence_legend_position)
  }
})

output$nonpara_bayes_prevalence_w0_graph = renderPlot({
  generate_relative_belief_ratio_at_w0_graph(
    relative_belief_ratio = sect3.4_prevalence_info_1()$relative_belief_ratio, 
    relative_belief_ratio_at_w0 = sect3.4_prevalence_info_2()$relative_belief_ratio_at_w0,
    w0_interval = sect3.4_prevalence_info_2()$w0_interval, 
    grid = sect3.4_prevalence_grid(),
    colour_choice = sect3.4_prevalence_w0_colours(),
    lty_type = sect3.4_prevalence_w0_lty(),
    transparency = input$nonpara_bayes_diag_prevalence_col_transparency_w0,
    legend_position = input$nonpara_bayes_prevalence_legend_position_w0)
})

# For: a sample of nD from diseased and nND from non-diseased ##

output$nonpara_bayes_prevalence_post_graph_alt = renderPlot({
  if(input$nonpara_bayes_prevalence_colour_1 != 'manual'){
    generate_prior_graph(prior = sect3.4_prevalence_prior(), 
                         grid = sect3.4_prevalence_grid(),
                         colour_choice = input$nonpara_bayes_prevalence_colour_1,
                         lty_type = as.numeric(input$nonpara_bayes_prevalence_lty_1),
                         transparency = input$nonpara_bayes_prevalence_col_transparency_1)
  } else if (input$nonpara_bayes_prevalence_colour_1 == 'manual'){
    generate_prior_graph(prior = sect3.4_prevalence_prior(), 
                         grid = sect3.4_prevalence_grid(),
                         colour_choice = paste("#", input$nonpara_bayes_prevalence_colour_2, sep = ""),
                         transparency = input$nonpara_bayes_prevalence_col_transparency_1)
  }
})


################################################################
# DOWNLOAD DATAFRAME                                           #
################################################################

nonpara_bayes_prevalence_download = reactive({
  RB_generate_dataframe(sect3.4_prevalence_grid(), 
                        sect3.4_prevalence_info_1()$prior, 
                        sect3.4_prevalence_info_1()$post, 
                        sect3.4_prevalence_info_1()$relative_belief)
})

output$nonpara_bayes_prevalence_dataframe <- renderDataTable({
  nonpara_bayes_prevalence_download()
})

output$nonpara_bayes_prevalence_downloadData <- downloadHandler(
  filename = function() {
    paste(input$nonpara_bayes_prevalence_filename, ".csv", sep = "")
  },
  content = function(file) {
    write.csv(nonpara_bayes_prevalence_download(), file, row.names = FALSE)
  }
)

# For: a sample of nD from diseased and nND from non-diseased ##

nonpara_bayes_prevalence_download_prior = reactive({
  RB_generate_priorframe(grid = sect3.4_prevalence_grid(),
                         prior = sect3.4_prevalence_prior())
})

output$nonpara_bayes_prevalence_dataframe_alt <- renderDataTable({
  nonpara_bayes_prevalence_download_prior()
})

output$nonpara_bayes_prevalence_downloadData_alt <- downloadHandler(
  filename = function() {
    paste(input$nonpara_bayes_prevalence_filename_alt, ".csv", sep = "")
  },
  content = function(file) {
    write.csv(nonpara_bayes_prevalence_download_prior(), file, row.names = FALSE)
  }
)