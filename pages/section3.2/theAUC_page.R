################################################################
# DESCRIPTION                                                  #
################################################################

theAUC_description = div(
  titlePanel("Page Description & Initial Setup"),
  sidebarLayout(
    sidebarPanel(width = 3, 
      numericInput(inputId = "theAUC_nND",
                   tags$p('Total Non-Diseased', style = "font-size: 90%;"),
                   value = 50, min = 1), #
      numericInput(inputId = "theAUC_nD", 
                   tags$p('Total Diseased', style = "font-size: 90%;"),
                   value = 100, min = 1),
      numericInput(inputId = "theAUC_nMonteCarlo", 
                   tags$p('Monte Carlo (Simulation) Sample Size', 
                          style = "font-size: 90%;"),value = 100000, min = 0),
    ),
    mainPanel(
      p("The goal of this section is for the user to compute the prior, posterior, the relative belief 
        ratio of the AUC. The user is free to download any results."),
      p("Plot images can be saved by right clicking them."),
      hr(style = "border-top: 1px solid #363e4f;")
    )
  )
)

################################################################
# OUTPUT 1 PAGE                                                #
################################################################

theAUC_plausible_region = div( 
  titlePanel("Plausible Region of w & More"),
  mainPanel(
    tabPanel("Plausible Region of w & More", verbatimTextOutput("theAUC_output1")),
  )
)

################################################################
# GRAPH 1 PAGE (HISTOGRAM)                                     #
################################################################

theAUC_plots = div( 
  titlePanel("Histograms"), 
  sidebarLayout(
    sidebarPanel(width = 3,
      numericInput(inputId = "theAUC_delta", 
                   tags$p("Delta"), value = 0.04, min = 0, max = 1),
      textInput(inputId = "theAUC_gamma", label = "Gamma (must be less than posterior content)", 
                value = "NA"),
      radioButtons(inputId = "theAUC_hist_visual", label = "Choose Visual:",
                   c("With Bars" = "theAUC_withbars",
                     "Without Bars" = "theAUC_withoutbars"),
                   selected = "theAUC_withbars"),
    ),
    mainPanel(
      tabPanel("Plots",
               fluidRow(splitLayout(cellWidths = c("50%", "50%"), 
                                    plotOutput("theAUC_postprior_graph"), 
                                    plotOutput("theAUC_RB_graph")))),
    )
  )
)

################################################################
# GRAPH 2 PAGE                                                 #
################################################################

default_copt_list = list("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5,
                      "6" = 6, "7" = 7, "8" = 8, "9" = 9, "10" = 10,
                      "11" = 11, "12" = 12, "13" = 13, "14" = 14, "15" = 15,
                      "16" = 16, "17" = 17, "18" = 18, "19" = 19, "20" = 20,
                      "21" = 21, "22" = 22, "23" = 23, "24" = 24, "25" = 25)

theAUC_copt_plots = div( 
  titlePanel("Copt Plots"), 
  sidebarLayout(
    sidebarPanel(width = 2,
      selectInput(inputId = "theAUC_priorc_opt_label", 
                   label = "Plot Symbol for Prior",
                   default_copt_list,
                   selected = 3),
      selectInput(inputId = "theAUC_postc_opt_label", 
                  label = "Plot Symbol for Posterior",
                  default_copt_list,
                  selected = 4),
      selectInput(inputId = "theAUC_rbc_opt_label", 
                  label = "Plot Symbol for RB Ratio",
                  default_copt_list,
                  selected = 8),
    ),
    mainPanel(
      tabPanel("Plots",
               fluidRow(splitLayout(cellWidths = c("55%", "55%"), 
                                    plotOutput("theAUC_postprior_copt_graph"), 
                                    plotOutput("theAUC_RB_copt_graph")))),
      
    )
  )
)

################################################################
# HYPOTHESIS TESTING                                           #
################################################################

theAUC_hypothesizedAUC = div( 
  titlePanel("Hypothesized AUC"),
  sidebarLayout(
    sidebarPanel(width = 3, 
      numericInput(inputId = "theAUC_hypoAUC",
                   tags$p('Hypothesized AUC (greater than)', style = "font-size: 90%;"),value = 0.5),
      textInput(inputId = "theAUC_alpha_ND",
                tags$p('alphaND1, ..., alphaNDm', style = "font-size: 90%;"),
                value = "1, 1, 1, 1, 1"),
      textInput(inputId = "theAUC_alpha_D",
                tags$p('alphaD1, ..., alphaDm', style = "font-size: 90%;"),
                value = "1, 1, 1, 1, 1"),
      textInput(inputId = "theAUC_fND",
                tags$p('fNd', style = "font-size: 90%;"),
                value = "29, 7, 4, 5, 5"),
      textInput(inputId = "theAUC_fD",
                tags$p('fD', style = "font-size: 90%;"),
                value = "14, 7, 25, 33, 21"),
    ),
    mainPanel(
      tabPanel("Relative Belief Plot of w0", verbatimTextOutput("theAUC_hypoAUC_value"))))
  
)

################################################################
# DOWNLOAD PAGE                                                #
################################################################

theAUC_download_1 = div( 
  titlePanel("Download Prior & Posterior"), 
  sidebarLayout(
    sidebarPanel(width = 3, 
      textInput(inputId = "theAUC_filename", "Input File Name", value = "AUC Values"),
      #radioButtons(inputId = "theAUC_choosefile", "Choose Which Data to Download",
      #             choices = list("Prior" = 1, "Posterior" = 2),
      #             selected = 1),
      #actionButton('theAUC_prev_five', 'Previous Cols'),
      #actionButton('theAUC_next_five', 'Next Cols'),
      downloadButton("theAUC_downloadData", "Download"),
    ),
    mainPanel(
      tabPanel("Download Output", dataTableOutput("theAUC_dataframe"))
    )
  )
)

theAUC_generate_dataframe = function(delta, AUC_prior, AUC_post, AUC_RBR){
  
  grid_pts = theAUC_grid(delta)
  #TEMPORARILY CHANGE THE AUC_prior_pts
  #AUC_prior_pts = c(0, grab_AUC_densities_breaks(delta, AUC_prior)*delta)
  #AUC_post_pts = c(0, grab_AUC_densities_breaks(delta, AUC_post)*delta)
  AUC_prior_pts = c(0, grab_AUC_densities_breaks(delta, AUC_prior)$density)
  AUC_post_pts = c(0, grab_AUC_densities_breaks(delta, AUC_post)$density)
  #print(c(length(grid_pts), length(AUC_prior_pts), length(AUC_post_pts), length(AUC_RBR)))
  df = data.frame(grid_pts, AUC_prior_pts, AUC_post_pts, AUC_RBR)
  colnames(df) = c("Grid point", "Prior of the AUC", "Posterior of the AUC", "Relative Belief Ratio of the AUC")
  return(df)
}

################################################################
# PAGE LOGIC                                                   #
################################################################

page_theAUC = div(
  titlePanel("Finite-valued Diagnostic"),
  # OUTPUTTING THE VALUES
  tabsetPanel(type = "tabs",
              tabPanel("Description", theAUC_description),
              tabPanel("Test AUC >= 0.5", theAUC_hypothesizedAUC),
              tabPanel("Plausible Region of w & More", theAUC_plausible_region),
              tabPanel("Histograms", theAUC_plots),
              tabPanel("Copt Plots", theAUC_copt_plots),
              tabPanel("Download Prior & Posterior", theAUC_download_1),
  )
)


################################################################
# CODE TO DELETE LATER - TESTING PURPOSES ONLY                 #
################################################################

# TESTING
# nND, nD, nMonteCarlo, alpha_ND, alpha_D
#nND = 50
#nD = 100
#nMonteCarlo = 10000
#alpha_ND = c(1, 1, 1, 1, 1) 
#alpha_D = c(1, 1, 1, 1, 1)
###m = 5
#fND = "29, 7, 4, 5, 5"
#fD = "14, 7, 25, 33, 21"
#delta = 0.01
#gamma = 0.5
#w = 0.65
#alternative
#alpha1w = 391.72
#alpha2w = 211.39

# For case 1: when w is given
#test1 = simulate_AUC_mc_prior(nND = nND, nD = nD, nMonteCarlo = nMonteCarlo, 
#                              w = w, alpha1w = NA, alpha2w = NA,
#                              alpha_ND = alpha_ND, alpha_D = alpha_D)
#test2 = simulate_AUC_mc_post(nND = nND, nD = nD, nMonteCarlo = nMonteCarlo, 
#                             w = w, alpha1w = NA, alpha2w = NA, version = "prior",
#                             alpha_ND = alpha_ND, alpha_D = alpha_D, 
#                             fND = fND, fD = fD)

#test3 = compute_AUC_RBR(delta = delta, AUC_prior = test1$AUC, AUC_post = test2$AUC, 
#                        priorc_opt = test1$priorc_opt, postc_opt = test2$postc_opt)
#testpr = compute_AUC_plausible_region(delta = delta, AUC_RBR = test3$AUC_RBR, num_average_pts = 3)

#density_hist_AUC_prior_post(delta = delta, AUC_prior = test1$AUC, AUC_post = test2$AUC, 
#                            plausible_region = testpr$plausible_region,
#                            credible_region = FALSE, densityplot = TRUE, showbars = TRUE)

# For case 2: when w isn't given
#test1 = simulate_AUC_mc_prior(nND = nND, nD = nD, nMonteCarlo = nMonteCarlo, 
#                              w = FALSE, alpha1w = alpha1w, alpha2w = alpha2w,
#                              alpha_ND = alpha_ND, alpha_D = alpha_D)
#test2 = simulate_AUC_mc_post(nND = nND, nD = nD, nMonteCarlo = nMonteCarlo, 
#                             w = FALSE, alpha1w = alpha1w, alpha2w = alpha2w, version = "post",
#                             alpha_ND = alpha_ND, alpha_D = alpha_D, 
#                             fND = fND, fD = fD)

#test3 = compute_AUC_RBR(delta = delta, AUC_prior = test1$AUC, AUC_post = test2$AUC, 
#                        priorc_opt = test1$priorc_opt, postc_opt = test2$postc_opt)
#testpr = compute_AUC_plausible_region(delta = delta, AUC_RBR = test3$AUC_RBR, num_average_pts = 3)

#density_hist_AUC_prior_post(delta = delta, AUC_prior = test1$AUC, AUC_post = test2$AUC, 
#                            plausible_region = testpr$plausible_region,
#                            credible_region = FALSE, densityplot = TRUE, showbars = TRUE)





#test1 = simulate_AUC_mc_prior(nND, nD, nMonteCarlo, w, alpha_ND, alpha_D)
#test2 = simulate_AUC_mc_post(nND, nD, nMonteCarlo, w, alpha_ND, alpha_D, fND, fD)
#grid = theAUC_grid(delta)
#prior_pts = c(0, grab_AUC_densities(delta, test1$AUC))

#test3 = compute_AUC_RBR(delta, test1$AUC, test2$AUC, test1$priorc_opt, test2$postc_opt)


#testpr = compute_AUC_plausible_region(delta, test3$AUC_RBR, 3)

#density_hist_AUC_RBR(delta, test3$AUC_RBR, testpr$plausible_region)

#convert_hist_to_density_plot(test_density$density, test_density$breaks, num_average_pts = 3, showplot = TRUE)

#testpc = compute_AUC_post_content(delta, test2$AUC, testpr$plausible_region)

#testcr = compute_AUC_credible_region(gamma, testpr$grid, testpr$density, 
#                                     test2$AUC, testpc, testpr$plausible_region)


#testxx = AUC_prior_error_char_copt(test3$c_optfDfND, nMonteCarlo, w, delta, test1$pND_array, test1$pD_array, 
#                                   test1$FNR, test1$FPR, test1$ERROR_w, test1$PPV, test1$priorc_opt)

#testyy = AUC_post_error_char_copt(test3$c_optfDfND, nMonteCarlo, w, delta, test2$pND_array, test2$pD_array, 
#                                  test2$FNR, test2$FPR, test2$ERROR_w, test2$PPV, test2$postc_opt)

#test_result = compute_AUC_error_char_copt(delta, test3$c_optfDfND, 
#                                          testxx$priorFPRc_opt, testxx$priorFNRc_opt, testxx$priorERROR_wc_opt, 
#                                          testxx$priorFDRc_opt, testxx$priorFNDRc_opt,
#                                          testyy$postFPRc_opt, testyy$postFNRc_opt, testyy$postERROR_wc_opt, 
#                                          testyy$postFDRc_opt, testyy$postFNDRc_opt)



#hypothesized_AUC_compute_values(0.5, delta, test3$AUC_RBR)

#grab_AUC_densities(delta, test2$AUC)*delta
#sum(grab_AUC_densities(delta, test2$AUC)*delta)

#test4 = compute_AUC_post_content(theAUC_delta, test2$AUC, test3$plausible_region)

#test5 = compute_AUC_credible_region(gamma, theAUC_delta, test3$AUC_RBR, test2$AUC,
#                            test4, test3$plausible_region)

#theAUC_generate_dataframe(delta, test1$AUC, test2$AUC, test3$AUC_RBR)

#par(mfrow=c(1,2))

#density_hist_AUC_prior_post(theAUC_delta, test1$AUC, test2$AUC, test3$plausible_region,
#                            test5$credible_region)

#density_hist_AUC_RBR(theAUC_delta, test3$AUC_RBR, testpr$plausible_region)


#################### testing for the plausible region


#initial_vals = grab_AUC_RBR_densities_breaks(delta, test3$AUC_RBR)
#hist_breaks = initial_vals$breaks
#hist_counts = initial_vals$counts
#length(hist_breaks)
#length(hist_counts)

#new_hist = convert_hist_to_density_plot(hist_counts, hist_breaks, num_average_pts = 5, showplot = FALSE)
#new_hist$grid
#new_hist$density
#new_hist$density = new_hist$density[-(length(new_hist$density))]
#length(new_hist$grid)
#length(new_hist$density)

#lines(new_hist$grid, new_hist$density)


#initial_vals = grab_AUC_RBR_densities_breaks(delta, test3$AUC_RBR)
#hist_breaks = initial_vals$breaks
#hist_counts = initial_vals$counts # NOTE: COUNTS, not DENSITY
# Converts it to a line plot
#line_plot = convert_hist_to_density_plot(hist_counts, hist_breaks, num_average_pts = 7)
#line_plot
#length(line_plot$counts)
#length(line_plot$density)
#length(line_plot$grid)

#smoothingSpline = smooth.spline(grid, prior_pts, spar=0.7)
#plot(grid,prior_pts)
#lines(smoothingSpline)
