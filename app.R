################################################################
# TODO LIST                                                    #
################################################################

# SECTION 3.2 #######################################################
# conditionalROC.txt -> CHANGED 
# conditiononAUCbig.txt -> Need review for plot titles & sanity checks
# ex1prog.txt -> Need review for plot titles & sanity checks
# readdata.txt -> CANNOT RUN; waiting for fixed bugs
# realdataROC.txt -> need to sanity check math & waiting for fixed bugs
# ROC.txt -> some error here?

# SECTION 3.3 #######################################################
# binormalAUCequalvariance -> CANNOT RUN; waiting for fixed bugs
# binormalAUCunequalvariance -> NEED TO CODE FUNCTIONS
# binormalcoptequalvariance -> Need to adjust the website more
# binormalcoptunequalvariance -> NEED TO CODE FUNCTIONS
# coptpriorprevalence -> CANNOT RUN; waiting for fixed bugs
# plotROC -> need to sanity check, style the graphs, comment about hardcode

# SECTION 3.4 #######################################################
# betaprior ->  need review
# BNPAUCFemales -> NEED TO CODE FUNCTIONS (seeding issue)
# BNPcfixedMales -> CANNOT RUN; waiting for fixed bugs
# BNPcoptFemales -> need to see the difference between male and female
# BNPcoptMales -> 
# BNPdata -> Need review for plot titles
# empiricals -> hard to code since the input seems to be a series of values
# itsforgammaprior -> Need review for plot titles
# smoother -> CANNOT RUN; waiting for fixed bugs
# storeBNPcoptFemales -> CANNOT RUN; waiting for fixed bugs


################################################################
# LIBRARIES                                                    #
################################################################

# Libraries for website creation
library(shiny)
library(DT) # for tables
library(varhandle)
#library(sendmailR) # for emails
#library(shinyAce) #for emails


# Other libraries used for the code
library(rBeta2009)
library(tidyverse)
library(stringr)

# Accessing other R-codes
source("routes.R")

################################################################
# FRONTEND                                                     #
################################################################

ui <- navbarPage(title = " ROC Analysis & Relative Belief",
                 tabPanel("Home", home_page),
                 navbarMenu("Section 3.1",
                            tabPanel("ThePrevalence", page_RB_setup),
                 ),
                 navbarMenu("Section 3.2",
                            # To avoid the need to parse encoded URLs via utils::URLdecode use e.g.:
                            # tabPanel(title = "Section 3.2.1", "3.2.1 content", value = "section_3.2.1"),
                            tabPanel("TheAUC", page_theAUC),
                            tabPanel("ConditionalAUCbig", page_conditionalAUCbig),
                            tabPanel("Example1Program", page_ex1prog),
                            #tabPanel("ReadData", page_readdata),
                            #tabPanel("RealDataROC", page_realdataROC),
                            #tabPanel("ROC", page_ROC)
                 ),
                 navbarMenu("Section 3.3",
                            tabPanel("3.3 Variables", page_variables3.3),
                            tabPanel("BinormalAUCEqualVariance", page_binormalAUCequalvariance),
                            tabPanel("BinormalAUCUnequalVariance", page_binormalAUCunequalvariance),
                            #tabPanel("BinormalCoptEqualVariance", page_binormalcoptequalvariance),
                            #tabPanel("BinormalCoptUnequalVariance", page_binormalcoptunequalvariance),
                            #tabPanel("CoptPriorPrevalence", page_coptpriorprevalence),
                            #tabPanel("plotROC", page_plotROC)
                 ),
                 navbarMenu("Section 3.4",
                            tabPanel("3.4 Variables", page_variables3.4),
                            tabPanel("BetaPrior", page_betaprior),
                            tabPanel("BNPAUC", page_BNPAUC),
                            #tabPanel("BNPcFixedMales", page_BNPcfixedMales),
                            #tabPanel("BNPCoptFemales", page_BNPcoptFemales),
                            #tabPanel("BNPCoptMales", page_BNPcoptMales),
                            #tabPanel("BNPData", page_BNPdata),
                            #tabPanel("Empiricals", page_empiricals),
                            #tabPanel("ForGammaPrior", page_itsforgammaprior),
                            #tabPanel("Smoother", page_smoother),
                            #tabPanel("StoreBNPCoptFemales", page_storeBNPcoptFemales)
                 ),
                 tabPanel("Contact", contact_page),
                 id = "navbarID",
                 theme = shinythemes::shinytheme("flatly"),
                 #theme = "main.css"
)

################################################################
# BACKEND                                                      #
################################################################

server <- function(input, output, session) {
  # Making it so the tab changes when the user clicks to another tab
  observeEvent(session$clientData$url_hash, {
    currentHash <- utils::URLdecode(sub("#", "", session$clientData$url_hash))
    if(is.null(input$navbarID) || !is.null(currentHash) && currentHash != input$navbarID){
      freezeReactiveValue(input, "navbarID")
      updateNavbarPage(session, "navbarID", selected = currentHash)
    }
  }, priority = 1)
  
  observeEvent(input$navbarID, {
    currentHash <- sub("#", "", session$clientData$url_hash)
    pushQueryString <- paste0("#", input$navbarID)
    if(is.null(currentHash) || currentHash != input$navbarID){
      freezeReactiveValue(input, "navbarID")
      updateQueryString(pushQueryString, mode = "push", session)
    }
  }, priority = 0)
  
  ################################################################
  # SECTION 3.1                                                  #
  ################################################################
  sect_3.1_grid = reactive(RB_distance_that_matters(input$RB_delta))
  sect_3.1_info_1 = reactive(RBR_compute_values(input$RB_setup_alpha1w, 
                    input$RB_setup_alpha2w, input$RB_setup_n, input$RB_setup_nD, sect_3.1_grid()))
  sect_3.1_info_2 = reactive(w0_compute_values(input$RB_setup_alpha1w, input$RB_setup_alpha2w, 
                    input$RB_setup_n, input$RB_setup_nD, input$RB_setup_w0, 
                    sect_3.1_info_1()$relative_belief_ratio, sect_3.1_grid()))
  sect_3.1_cred_region = reactive(compute_credible_region(input$RB_setup_alpha1w, input$RB_setup_alpha2w, 
                                  input$RB_setup_n, input$RB_setup_nD, sect_3.1_grid(), input$RB_gamma, 
                                  input$RB_delta, sect_3.1_info_1()$relative_belief_ratio, 
                                  sect_3.1_info_1()$posterior_content, sect_3.1_info_1()$plausible_region))
  
  output$RB_setup_values1 = renderPrint({
    list("plausible_region" = sect_3.1_info_1()$plausible_region,
         "max_w" = sect_3.1_info_1()$max_w,
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
  output$RB_setup_postprior_graph = renderPlot({
    if(check.numeric(input$RB_gamma) == FALSE){
      generate_prior_post_graph(sect_3.1_info_1()$prior, sect_3.1_info_1()$post, 
                                sect_3.1_info_1()$plausible_region, sect_3.1_grid())
    } else if (as.numeric(input$RB_gamma) >= sect_3.1_info_1()$posterior_content){
      # Couldn't do the or statement for if because of the case where you can't do
      # as.numeric() for input$gamma
      generate_prior_post_graph(sect_3.1_info_1()$prior, sect_3.1_info_1()$post, 
                                sect_3.1_info_1()$plausible_region, sect_3.1_grid())
    } else {
      generate_prior_post_graph(sect_3.1_info_1()$prior, sect_3.1_info_1()$post, 
                                sect_3.1_info_1()$plausible_region, sect_3.1_grid(),
                                sect_3.1_cred_region()$credible_region)
    }
  })
  output$RB_setup_RB_graph = renderPlot({
    if(check.numeric(input$RB_gamma) == FALSE){
      generate_rb_graph(sect_3.1_info_1()$relative_belief_ratio, sect_3.1_info_1()$plausible_region, sect_3.1_grid())
    } else if (as.numeric(input$RB_gamma) >= sect_3.1_info_1()$posterior_content){
      generate_rb_graph(sect_3.1_info_1()$relative_belief_ratio, sect_3.1_info_1()$plausible_region, sect_3.1_grid())
    } else {
      generate_rb_graph(sect_3.1_info_1()$relative_belief_ratio, sect_3.1_info_1()$plausible_region, sect_3.1_grid(),
                        sect_3.1_cred_region()$credible_region, sect_3.1_cred_region()$rb_line)
    }
  })
  output$RB_setup_w0_graph = renderPlot({
    generate_relative_belief_ratio_at_w0_graph(sect_3.1_info_1()$relative_belief_ratio, 
                                               sect_3.1_info_2()$relative_belief_ratio_at_w0,
                                               sect_3.1_info_2()$w0_interval, sect_3.1_grid())
  })
  RB_download = reactive(
    RB_generate_dataframe(sect_3.1_grid(), sect_3.1_info_1()$prior, sect_3.1_info_1()$post, 
                          sect_3.1_info_1()$relative_belief)
  )
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
  ################################################################
  # SECTION 3.2                                                  #
  ################################################################
  sect3.2_AUC_prior = reactive(simulate_AUC_mc_prior(input$theAUC_nND, 
                      input$theAUC_nD, input$theAUC_nMonteCarlo, 
                      input$theAUC_alpha_ND, input$theAUC_alpha_D))
  sect3.2_AUC_post = reactive(simulate_AUC_mc_post(input$theAUC_nND, 
                     input$theAUC_nD, input$theAUC_nMonteCarlo, 
                     input$theAUC_alpha_ND, input$theAUC_alpha_D, input$theAUC_fND, 
                     input$theAUC_fD))
  sect3.2_AUC_RBR = reactive(compute_AUC_RBR(input$theAUC_delta, sect3.2_AUC_prior()$AUC, 
                   sect3.2_AUC_post()$AUC))
  # posterior content
  sect3.2_AUC_post_content = reactive(compute_AUC_post_content(input$theAUC_delta, 
                             sect3.2_AUC_post()$AUC, sect3.2_AUC_RBR()$plausible_region))
  #PLACEHOLDER - PLEASE CHANGE WHEN IMPLEMENTED
  sect3.2_AUC_credible_region = reactive(
    compute_AUC_credible_region(input$theAUC_gamma, input$theAUC_delta, 
    sect3.2_AUC_RBR()$AUC_RBR, sect3.2_AUC_post()$AUC,
    sect3.2_AUC_post_content(), sect3.2_AUC_RBR()$plausible_region))
  
  output$theAUC_output1 = renderPrint({
    list("plausible_region" = sect3.2_AUC_RBR()$plausible_region,
         "posterior_content" = sect3.2_AUC_post_content(),
         "credible_region" = sect3.2_AUC_credible_region()$credible_region) # will need to specify when
    # providing a vector
    # TODO: specify credible region
  })
  
  output$theAUC_postprior_graph = renderPlot({
    if(check.numeric(input$theAUC_gamma) == FALSE){
      density_hist_AUC_prior_post(input$theAUC_delta, sect3.2_AUC_prior()$AUC, 
                                  sect3.2_AUC_post()$AUC, sect3.2_AUC_RBR()$plausible_region)
      
    } else if (as.numeric(input$theAUC_gamma) >= sect3.2_AUC_post_content()){
      density_hist_AUC_prior_post(input$theAUC_delta, sect3.2_AUC_prior()$AUC, 
                                  sect3.2_AUC_post()$AUC, sect3.2_AUC_RBR()$plausible_region)
    } else {
      density_hist_AUC_prior_post(input$theAUC_delta, sect3.2_AUC_prior()$AUC, 
                                  sect3.2_AUC_post()$AUC, sect3.2_AUC_RBR()$plausible_region,
                                  sect3.2_AUC_credible_region()$credible_region) # MUST MODIFY
    }
  })
  output$theAUC_RB_graph = renderPlot({
    if(check.numeric(input$theAUC_gamma) == FALSE){
      density_hist_AUC_RBR(input$theAUC_delta, sect3.2_AUC_RBR()$AUC_RBR, 
                           sect3.2_AUC_RBR()$plausible_region)
    } else if (as.numeric(input$theAUC_gamma) >= sect3.2_AUC_post_content()){
      density_hist_AUC_RBR(input$theAUC_delta, sect3.2_AUC_RBR()$AUC_RBR, 
                           sect3.2_AUC_RBR()$plausible_region)
    } else {
      density_hist_AUC_RBR(input$theAUC_delta, sect3.2_AUC_RBR()$AUC_RBR, 
                           sect3.2_AUC_RBR()$plausible_region,
                           sect3.2_AUC_credible_region()$credible_region) # MUST MODIFY
    }
  })
  
  output$theAUC_hypoAUC_graph = renderPlot({
    density_hist_AUC_RBR(input$theAUC_delta, sect3.2_AUC_RBR()$AUC_RBR, 
                         sect3.2_AUC_RBR()$plausible_region, hypothesis = input$theAUC_hypoAUC)
  })
  output$theAUC_hypoAUC_value = renderPrint({
    hypothesized_AUC_compute_values(input$theAUC_hypoAUC, input$theAUC_delta,
                                    sect3.2_AUC_prior()$AUC, sect3.2_AUC_post()$AUC)
  })
  
  
  theAUC_download = reactive(
    theAUC_generate_dataframe(input$theAUC_delta, sect3.2_AUC_prior()$AUC, 
                              sect3.2_AUC_post()$AUC, sect3.2_AUC_RBR()$AUC_RBR)
  )
  
  output$theAUC_dataframe <- renderDataTable({
    theAUC_download()
  })
  
  output$theAUC_downloadData <- downloadHandler(
    filename = function() {
      paste(input$theAUC_filename, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(theAUC_download(), file, row.names = FALSE)
    }
  )
  
  
  ###########################################################################
  # OUTPUTS FROM conditionalAUCbig
  output$conditionalAUCbig_values = renderPrint({
    # NOTE: shows all crit values; might need to change issue since code is quite long
    conditionalAUCbig(input$conditionalAUCbig_nMonte, input$conditionalAUCbig_fND, 
                      input$conditionalAUCbig_fD)
  })
  output$conditionalAUCbig_hist = renderPlot({
    conditionalAUCbig_results = conditionalAUCbig(input$conditionalAUCbig_nMonte, 
                                                  input$conditionalAUCbig_fND, 
                                                  input$conditionalAUCbig_fD)
    hist(conditionalAUCbig_results$crit, freq=F, ylab = "y-axis label", xlab = "x-axis label",
         main = "Placeholder Histogram Title", col = "#9fcbec", border = F)
  })
  # OUTPUTS FROM conditionalROC
  # ...
  # OUTPUTS FROM ex_1prog
  output$ex1_prog_values = renderPrint({
    ex1prog(input$w, input$q)
  })
  output$Test_Plot = renderPlot({
    ex1prog_data = ex1prog_graph(input$n_size)
    plot(ex1prog_data$p, ex1prog_data$ROC1, type = "l", lty = 1, ylab = "ROC", xlab = "p",
         main = "template title")
    lines(ex1prog_data$p, ex1prog_data$ROC2, lty=2)
  })
  # OUTPUTS FROM readdata
  # ...
  # OUTPUTS FROM realdataROC
  output$realdataROC_value_1 = renderPrint({
    prior_distribution_c_opt(input$nMonteprior, input$fND, input$fD, input$realdataROC_p)
  })
  output$realdataROC_value_2 = renderPrint({
    post_distribution_c_opt(input$nMontepost, input$fND, input$fD, input$realdataROC_p)
  })
  output$realdataROC_value_3 = renderPrint({
    realdataROC_placeholder_1(input$nMonteprior, input$nMontepost, input$fND, 
                              input$fD, input$realdataROC_p)
  })
  output$realdataROC_value_4 = renderPrint({
    realdataROC_placeholder_2(input$realdataROC_ngrid, input$nMonteprior, input$nMontepost, 
                              input$fND, input$fD, input$realdataROC_p)
  })
  # OUTPUTS FROM ROC
  output$ROC_value_1 = renderPrint({
    simulate_data_ROC(input$ROC_pND, input$ROC_pD, input$ROC_nND, input$ROC_nD)
  })
  output$ROC_value_2 = renderPrint({
    ROC_compute_some_outputs_1(input$ROC_w, input$ROC_pND, input$ROC_pD)
  })
  ################################################################
  # SECTION 3.3                                                  #
  ################################################################
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
  ################################################################
  # SECTION 3.4                                                  #
  ################################################################
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
}

shinyApp(ui, server)