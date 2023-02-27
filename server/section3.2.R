################################################################
# DEFINITIONS                                                  #
################################################################

output$formulas = renderImage({
  list(src = "./pages/section3.2/formulas.png",
       width = "70%", height = "100%")
  }, deleteFile = FALSE
)


# Defining the inputs
input_var_name <- c("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~",
              "Total Non-Diseased", 
              "Total Diseased",
              "Monte Carlo (Simulation) Sample Size",
              "Relevant Prevalence w", 
              "alphaND1, ..., alphaNDm",
              "alphaD1, ..., alphaDm",
              "fND",
              "fD",
              "Delta",
              "Gamma",
              "Hypothesized AUC (greater than)",
              "Input File Name")
input_var_description <- c("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~",
                     "  The amount of \"non-diseased\" individuals from the total sample size.",
                     "  The total amount of \"diseased\" individuals from the total sample size.",
                     "  The sample size to use the Monte Carlo method to simulate the prior and the posterior. 
                     When computations are taking too long, it is recommended to lower the sample size.",
                     "  TODO",
                     "  The parameters of the Dirichlet distribution of the non-diseased sample.",
                     "  The parameters of the Dirichlet distribution of the diseased sample.",
                     "  TODO",
                     "  TODO",
                     "  Also known as the distance that matters. It is the distance between any two points on the grid.",
                     "  A value that's less than the posterior content of the plausible region. It is used to 
                     determine the credible region to test the validity of where the maximum of the relative belief 
                     ratio is located.",
                     "  Input what the user hypothesizes AUC to be greater than.",
                     "  The name of the file you want to download. The .csv file will include the grid points, the prior, 
                     the posterior, and the relative belief ratio.")
input_test_df <- data.frame(input_var_description)
rownames(input_test_df) = input_var_name
colnames(input_test_df) = "Description of the Inputs"


# Defining the outputs
output_var_name <- c("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~",
                    "Plausible Region", 
                    "Posterior Content",
                    "Credible Region",
                    "Area Under The Line Plot",
                    "Prior copt",
                    "Post copt",
                    "copt Estimate",
                    "Error Characteristics",
                    "FPRest copt Estimate",
                    "FNRest copt Estimate",
                    "ERRORwest copt Estimate",
                    "FDRest copt Estimate",
                    "FNDRest copt Estimate",
                    "PPVest copt Estimate",
                    "Relative Belief Ratio of Event AUC > XX%",
                    "Posterior Probability of Event AUC > XX%")
output_var_description <- c("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~",
                            "This interval consists of all the values for which there is evidence in favor.", 
                            "This interval measures how strongly it is believed the true value lies in this set.",
                            "Provides the exact area under the line plot for the prior, posterior, instead of 
                            the histograms to show how accurate it is.", 
                            "TODO",
                            "TODO",
                            "TODO",
                            "TODO",
                            "TODO",
                            "TODO",
                            "See next tab.", 
                            "See next tab.",
                            "See next tab.",
                            "See next tab.",
                            "See next tab.",
                            "Provides the relative belief ratio given the hypothesized AUC.",
                            "Provides the posterior content given the hypothesized AUC.")
output_test_df <- data.frame(output_var_description)
rownames(output_test_df) = output_var_name
colnames(output_test_df) = "Description of the Outputs"



output$input_table_description <- render_tableHTML({
  #mtcars %>%
  input_test_df %>%
    tableHTML(border = 2) %>%
    #tableHTML(widths = c(140, rep(45, 11))) %>%
    add_css_header(css = list(c('height', 'background-color', 'text-align'),
                              c('30px', '#def1fd', 'left')),
                   headers = 1:12) %>%
    add_css_row(css = list('background-color', '#eaf6fe'),
                rows = even(1:12)) %>%
    add_css_row(css = list('background-color', '#ffffff'),
                rows = odd(1:12))
    #            rows = odd(1:12)) %>%
    #add_css_column(css = list(c('text-align', 'background-color'),
    #                          c('center', '#def1fd')),
    #               columns = c('rownames'))
})

output$output_table_description <- render_tableHTML({
  #mtcars %>%
  output_test_df %>%
    tableHTML(border = 2) %>%
    #tableHTML(widths = c(140, rep(45, 11))) %>%
    add_css_header(css = list(c('height', 'background-color', 'text-align'),
                              c('30px', '#def1fd', 'left')),
                   headers = 1:12) %>%
    add_css_row(css = list('background-color', '#eaf6fe'),
                rows = even(1:12)) %>%
    add_css_row(css = list('background-color', '#ffffff'),
                rows = odd(1:12))
  #            rows = odd(1:12)) %>%
  #add_css_column(css = list(c('text-align', 'background-color'),
  #                          c('center', '#def1fd')),
  #               columns = c('rownames'))
})

################################################################
# VARIABLES                                                    #
################################################################

# TODO: decide how we want to include alpha1w and alpha2w (should carry on from the prevalence section?)

sect3.2_AUC_prior = reactive({
  if(input$pick_case_1 == "case_1_opt"){
    simulate_AUC_mc_prior(nND = input$theAUC_nND, 
                          nD = input$theAUC_nD, 
                          nMonteCarlo = input$theAUC_nMonteCarlo,
                          w = input$global_prevalence_w, 
                          alpha_ND = input$theAUC_alpha_ND, 
                          alpha_D = input$theAUC_alpha_D)
  } else if (input$pick_case_2 == "case_a_opt" | input$pick_case_2 == "case_b_opt"){ 
    simulate_AUC_mc_prior(nND = input$theAUC_nND, 
                          nD = input$theAUC_nD, 
                          nMonteCarlo = input$theAUC_nMonteCarlo, 
                          w = FALSE, 
                          alpha1w = input$RB_setup_alpha1w, # from the prevalence
                          alpha2w = input$RB_setup_alpha2w, # from the prevalence
                          alpha_ND = input$theAUC_alpha_ND, 
                          alpha_D = input$theAUC_alpha_D)
  }
})

sect3.2_AUC_post = reactive({
  if(input$pick_case_1 == "case_1_opt"){
    simulate_AUC_mc_post(nND = input$theAUC_nND, 
                         nD = input$theAUC_nD, 
                         nMonteCarlo = input$theAUC_nMonteCarlo, 
                         w = input$global_prevalence_w, 
                         alpha_ND = input$theAUC_alpha_ND, 
                         alpha_D = input$theAUC_alpha_D, 
                         fND = input$theAUC_fND, 
                         fD = input$theAUC_fD)
  } else if (input$pick_case_2 == "case_a_opt"){ # only know the prior
    simulate_AUC_mc_post(nND = input$theAUC_nND, 
                         nD = input$theAUC_nD, 
                         nMonteCarlo = input$theAUC_nMonteCarlo, 
                         w = FALSE, 
                         alpha1w = input$RB_setup_alpha1w, # from the prevalence
                         alpha2w = input$RB_setup_alpha2w, # from the prevalence
                         version = "prior", 
                         alpha_ND = input$theAUC_alpha_ND, 
                         alpha_D = input$theAUC_alpha_D, 
                         fND = input$theAUC_fND, 
                         fD = input$theAUC_fD)
  } else if (input$pick_case_2 == "case_b_opt"){ # know both prior and posterior
    simulate_AUC_mc_post(nND = input$theAUC_nND, 
                         nD = input$theAUC_nD, 
                         nMonteCarlo = input$theAUC_nMonteCarlo, 
                         w = FALSE, 
                         alpha1w = input$RB_setup_alpha1w, # from the prevalence
                         alpha2w = input$RB_setup_alpha2w, # from the prevalence
                         version = "post",
                         alpha_ND = input$theAUC_alpha_ND, 
                         alpha_D = input$theAUC_alpha_D, 
                         fND = input$theAUC_fND, 
                         fD = input$theAUC_fD)
  }
})

################

sect3.2_AUC_RBR = reactive({
  compute_AUC_RBR(delta = input$theAUC_delta, 
                  AUC_prior = sect3.2_AUC_prior()$AUC, 
                  AUC_post = sect3.2_AUC_post()$AUC, 
                  priorc_opt = sect3.2_AUC_prior()$priorc_opt, 
                  postc_opt = sect3.2_AUC_post()$postc_opt)
})

sect3.2_pr = reactive({
  compute_AUC_plausible_region(delta = input$theAUC_delta, 
                               AUC_RBR = sect3.2_AUC_RBR()$AUC_RBR)
}) # Short for plausible region

sect3.2_AUC_post_content = reactive({
  compute_AUC_post_content(delta = input$theAUC_delta, 
                           AUC_post = sect3.2_AUC_post()$AUC, 
                           plausible_region = sect3.2_pr()$plausible_region)
})

sect3.2_cr = reactive({
  compute_AUC_credible_region(gamma = input$theAUC_gamma, 
                              grid = sect3.2_pr()$grid, 
                              density = sect3.2_pr()$density, 
                              AUC_post = sect3.2_AUC_post()$AUC, 
                              posterior_content = sect3.2_AUC_post_content(), 
                              plausible_region = sect3.2_pr()$plausible_region)
}) # Short for credible region

######################################

sect3.2_copt_prior = reactive({
  if(input$pick_case_1 == "case_1_opt"){
    AUC_prior_error_char_copt(c_optfDfND = sect3.2_AUC_RBR()$c_optfDfND, 
                              nMonteCarlo = input$theAUC_nMonteCarlo, 
                              w = input$global_prevalence_w, 
                              delta = input$theAUC_delta, 
                              pND_array = sect3.2_AUC_prior()$pND_array, 
                              pD_array = sect3.2_AUC_prior()$pD_array, 
                              FNR = sect3.2_AUC_prior()$FNR, 
                              FPR = sect3.2_AUC_prior()$FPR, 
                              ERROR_w = sect3.2_AUC_prior()$ERROR_w, 
                              PPV = sect3.2_AUC_prior()$PPV, 
                              priorc_opt = sect3.2_AUC_prior()$priorc_opt)
  } else if (input$pick_case_2 == "case_a_opt" | input$pick_case_2 == "case_b_opt"){
    AUC_prior_error_char_copt(c_optfDfND = sect3.2_AUC_RBR()$c_optfDfND, 
                              nMonteCarlo = input$theAUC_nMonteCarlo, 
                              w = FALSE, 
                              alpha1w = input$RB_setup_alpha1w, # from the prevalence
                              alpha2w = input$RB_setup_alpha2w, # from the prevalence 
                              delta = input$theAUC_delta, 
                              pND_array = sect3.2_AUC_prior()$pND_array, 
                              pD_array = sect3.2_AUC_prior()$pD_array, 
                              FNR = sect3.2_AUC_prior()$FNR, 
                              FPR = sect3.2_AUC_prior()$FPR, 
                              ERROR_w = sect3.2_AUC_prior()$ERROR_w, 
                              PPV = sect3.2_AUC_prior()$PPV, 
                              priorc_opt = sect3.2_AUC_prior()$priorc_opt)
  }
})

sect3.2_copt_post = reactive({
  if(input$pick_case_1 == "case_1_opt"){
    AUC_post_error_char_copt(c_optfDfND = sect3.2_AUC_RBR()$c_optfDfND, 
                             nMonteCarlo = input$theAUC_nMonteCarlo, 
                             w = input$global_prevalence_w, 
                             delta = input$theAUC_delta, 
                             pND_array = sect3.2_AUC_post()$pND_array, 
                             pD_array = sect3.2_AUC_post()$pD_array, 
                             FNR = sect3.2_AUC_post()$FNR, 
                             FPR = sect3.2_AUC_post()$FPR, 
                             ERROR_w = sect3.2_AUC_post()$ERROR_w, 
                             PPV = sect3.2_AUC_post()$PPV, 
                             postc_opt = sect3.2_AUC_post()$postc_opt)
  } else if (input$pick_case_2 == "case_a_opt"){ # know the prior only
    AUC_post_error_char_copt(c_optfDfND = sect3.2_AUC_RBR()$c_optfDfND, 
                             nMonteCarlo = input$theAUC_nMonteCarlo, 
                             w = FALSE, 
                             alpha1w = input$RB_setup_alpha1w, # from the prevalence
                             alpha2w = input$RB_setup_alpha2w, # from the prevalence
                             version = "prior", 
                             delta = input$theAUC_delta, 
                             pND_array = sect3.2_AUC_post()$pND_array, 
                             pD_array = sect3.2_AUC_post()$pD_array, 
                             FNR = sect3.2_AUC_post()$FNR,
                             FPR = sect3.2_AUC_post()$FPR,
                             ERROR_w = sect3.2_AUC_post()$ERROR_w, 
                             PPV = sect3.2_AUC_post()$PPV, 
                             postc_opt = sect3.2_AUC_post()$postc_opt)
  } else if (input$pick_case_2 == "case_b_opt"){ # know both prior and posterior
    AUC_post_error_char_copt(c_optfDfND = sect3.2_AUC_RBR()$c_optfDfND, 
                             nMonteCarlo = input$theAUC_nMonteCarlo, 
                             w = FALSE, 
                             alpha1w = input$RB_setup_alpha1w, # from the prevalence
                             alpha2w = input$RB_setup_alpha2w, # from the prevalence
                             nD = input$theAUC_nD, 
                             nND = input$theAUC_nND, 
                             version = "post", 
                             delta = input$theAUC_delta, 
                             pND_array = sect3.2_AUC_post()$pND_array, 
                             pD_array = sect3.2_AUC_post()$pD_array, 
                             FNR = sect3.2_AUC_post()$FNR, 
                             FPR = sect3.2_AUC_post()$FPR, 
                             ERROR_w = sect3.2_AUC_post()$ERROR_w, 
                             PPV = sect3.2_AUC_post()$PPV, 
                             postc_opt = sect3.2_AUC_post()$postc_opt)
  }
})

########################################


sect3.2_copt_est = reactive({
  compute_AUC_error_char_copt(delta = input$theAUC_delta, 
                              c_optfDfND = sect3.2_AUC_RBR()$c_optfDfND, 
                              priorFPRc_opt = sect3.2_copt_prior()$priorFPRc_opt, 
                              priorFNRc_opt = sect3.2_copt_prior()$priorFNRc_opt, 
                              priorERROR_wc_opt = sect3.2_copt_prior()$priorERROR_wc_opt, 
                              priorFDRc_opt = sect3.2_copt_prior()$priorFDRc_opt, 
                              priorFNDRc_opt = sect3.2_copt_prior()$priorFNDRc_opt,
                              priorPPVc_opt = sect3.2_copt_prior()$priorPPVc_opt,
                              postFPRc_opt = sect3.2_copt_post()$postFPRc_opt, 
                              postFNRc_opt = sect3.2_copt_post()$postFNRc_opt, 
                              postERROR_wc_opt = sect3.2_copt_post()$postERROR_wc_opt, 
                              postFDRc_opt = sect3.2_copt_post()$postFDRc_opt, 
                              postFNDRc_opt = sect3.2_copt_post()$postFNDRc_opt,
                              postPPVc_opt = sect3.2_copt_post()$postPPVc_opt)
})



sect3.2_copt_est = reactive({
  compute_AUC_error_char_copt(delta = input$theAUC_delta, 
                              c_optfDfND = sect3.2_AUC_RBR()$c_optfDfND, 
                              priorFPRc_opt = sect3.2_copt_prior()$priorFPRc_opt, 
                              priorFNRc_opt = sect3.2_copt_prior()$priorFNRc_opt, 
                              priorERROR_wc_opt = sect3.2_copt_prior()$priorERROR_wc_opt, 
                              priorFDRc_opt = sect3.2_copt_prior()$priorFDRc_opt, 
                              priorFNDRc_opt = sect3.2_copt_prior()$priorFNDRc_opt,
                              priorPPVc_opt = sect3.2_copt_prior()$priorPPVc_opt,
                              postFPRc_opt = sect3.2_copt_post()$postFPRc_opt, 
                              postFNRc_opt = sect3.2_copt_post()$postFNRc_opt, 
                              postERROR_wc_opt = sect3.2_copt_post()$postERROR_wc_opt, 
                              postFDRc_opt = sect3.2_copt_post()$postFDRc_opt, 
                              postFNDRc_opt = sect3.2_copt_post()$postFNDRc_opt,
                              postPPVc_opt = sect3.2_copt_post()$postPPVc_opt)
})

showbarplots = reactive({
  if(input$theAUC_hist_visual == "theAUC_withbars"){
    TRUE
  }
  else if (input$theAUC_hist_visual == "theAUC_withoutbars"){
    FALSE 
  }
})

sect3.2_lineplot_area = reactive({
  priorpost = density_hist_AUC_prior_post(delta = input$theAUC_delta, 
                              AUC_prior = sect3.2_AUC_prior()$AUC, 
                              AUC_post = sect3.2_AUC_post()$AUC, 
                              plausible_region = sect3.2_pr()$plausible_region)
  as.data.frame(priorpost)
})

sect3.2_hypo_test = reactive({
  RBR_hypo = hypothesized_AUC_compute_values(hypo_AUC = input$theAUC_hypoAUC, 
                                             delta = input$theAUC_delta,
                                             AUC_prior = sect3.2_AUC_prior()$AUC, 
                                             AUC_post = sect3.2_AUC_post()$AUC)
  strength = compute_AUC_post_content(delta = input$theAUC_delta, 
                                      AUC_post = sect3.2_AUC_post()$AUC, 
                                      plausible_region = c(as.numeric(input$theAUC_hypoAUC), 0.99999999))
  # header names change depending on context
  
  newlist = list(RBR_header = RBR_hypo, strength_header = strength)
  names(newlist) <- c(paste("Relative Belief Ratio of Event AUC > ", 
                            as.character(input$theAUC_hypoAUC), sep = ""), 
                      paste("Posterior Probability of Event AUC > ", 
                            as.character(input$theAUC_hypoAUC), sep = ""))
  
  return(newlist)
})

################################################################
# NUMERIC/TEXT OUTPUTS                                         #
################################################################

#output$theAUC_output1 = renderPrint({ # For testing
#  list("test_val" = sect3.2_AUC_post()$AUC) # will need to specify when
#})

output$theAUC_output1 = renderPrint({
  list("plausible_region" = sect3.2_pr()$plausible_region,
       "posterior_content" = sect3.2_AUC_post_content(),
       "credible_region" = sect3.2_cr()$credible_region,
       "area_under_the_line_plot" = sect3.2_lineplot_area(),
       "prior_copt" = sect3.2_AUC_prior()$priorc_opt,
       "post_copt" = sect3.2_AUC_post()$postc_opt,
       "copt_est" = sect3.2_AUC_RBR()$c_optfDfND,
       "error_characteristics" = as.data.frame(sect3.2_copt_est())) # will need to specify when
  # providing a vector
  # TODO: specify credible region
})

output$theAUC_hypoAUC_value = renderPrint({
  sect3.2_hypo_test()
})

################################################################
# HISTOGRAMS                                                   #
################################################################


#output$theAUC_postprior_graph = renderPlot({
#  density_hist_AUC_prior_post(delta = input$theAUC_delta, 
#                              AUC_prior = sect3.2_AUC_prior()$AUC, 
#                              AUC_post = sect3.2_AUC_post()$AUC, 
#                              plausible_region = sect3.2_pr()$plausible_region,
#                              credible_region = FALSE, 
#                              densityplot = TRUE, 
#                              showbars = showbarplots())
#})

output$theAUC_postprior_graph = renderPlot({
  if(check.numeric(input$theAUC_gamma) == FALSE){
    density_hist_AUC_prior_post(delta = input$theAUC_delta, 
                                AUC_prior = sect3.2_AUC_prior()$AUC, 
                                AUC_post = sect3.2_AUC_post()$AUC, 
                                plausible_region = sect3.2_pr()$plausible_region,
                                densityplot = TRUE, 
                                showbars = showbarplots())
    
  } else if (as.numeric(input$theAUC_gamma) >= sect3.2_AUC_post_content()){
    density_hist_AUC_prior_post(delta = input$theAUC_delta, 
                                AUC_prior = sect3.2_AUC_prior()$AUC, 
                                AUC_post = sect3.2_AUC_post()$AUC, 
                                plausible_region = sect3.2_pr()$plausible_region,
                                densityplot = TRUE, 
                                showbars = showbarplots())
  } else {
    density_hist_AUC_prior_post(delta = input$theAUC_delta, 
                                AUC_prior = sect3.2_AUC_prior()$AUC, 
                                AUC_post = sect3.2_AUC_post()$AUC, 
                                plausible_region = sect3.2_pr()$plausible_region,
                                credible_region = sect3.2_cr()$credible_region,
                                densityplot = TRUE, 
                                showbars = showbarplots()) # MUST MODIFY
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
                 postc_opt = sect3.2_AUC_post()$postc_opt)
})
output$theAUC_RB_copt_graph = renderPlot({
  plots_AUC_copt(RBc_opt = sect3.2_AUC_RBR()$RBc_opt)
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

