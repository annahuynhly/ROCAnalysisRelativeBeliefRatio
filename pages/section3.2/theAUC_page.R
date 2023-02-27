################################################################
# DESCRIPTION                                                  #
################################################################

theAUC_description = div(
  titlePanel("Page Description & Initial Setup"),
  sidebarLayout(
    sidebarPanel(width = 3, 
      #numericInput(inputId = "theAUC_m", # CHANGE THIS
      #             tags$p('m (TODO DESCRIPTION)', style = "font-size: 90%;"),
      #             value = 5, min = 1),
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

theAUC_copt_plots = div( 
  titlePanel("Copt Plots"), 
  mainPanel(
    tabPanel("Plots",
              fluidRow(splitLayout(cellWidths = c("55%", "55%"), 
                                  plotOutput("theAUC_postprior_copt_graph"), 
                                  plotOutput("theAUC_RB_copt_graph")))),

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
# HELPER FUNCTIONS                                             #
################################################################

theAUC_grid = function(delta){ # MIGHT NEED TO MOVE THIS OUT - USED IN OTHER FUNCTS
  # Creates a grid of values from 0 to 1
  grid = seq(0, 1, length= (1/delta)+1)
  return(grid)
}

theAUC_generate_w = function(w = FALSE, alpha1w = NA, alpha2w = NA, nD = NA, nND = NA, version = NA){
  #Generates w based on the inputs.
  if(typeof(w) == "double"){
    return(w)
  } else if (w == FALSE & version == "prior"){ # This is a sanity check
    if(typeof(alpha1w) == "double" & typeof(alpha2w) == "double"){
      return(rbeta(1, alpha1w, alpha2w))
    } else {
      return("Invalid alpha1w, alpha2w.")
    }
  } else if (w == FALSE & (version == "post" | version == "posterior")){
    if(typeof(alpha1w) == "double" & typeof(alpha2w) == "double"){
      return(rbeta(1, alpha1w + nD, alpha2w + nND))
    }
  } else {
    return("Invalid value for w.")
  }
}


AUC_prior_error_char_copt = function(c_optfDfND, nMonteCarlo, w = FALSE, 
                                     alpha1w = NA, alpha2w = NA,
                                     delta, pND_array, pD_array, 
                                     FNR, FPR, ERROR_w, PPV, priorc_opt){
  # need to add support for PPV - unsure how
  
  if(length(pND_array) != length(pD_array)){
    return("Error: the length of pND_array and pD_array are different.")
  }
  A = theAUC_grid(delta)
  priorFPRc_opt = rep(0,(1/delta))
  priorFNRc_opt = rep(0,(1/delta))
  priorERROR_wc_opt = rep(0,(1/delta))
  priorFDRc_opt = rep(0,1/delta)
  priorFNDRc_opt = rep(0,1/delta)
  priorPPVc_opt = rep(0, 1/delta)
  #array(0*c(1:nMonteCarlo*m),dim=c(nMonteCarlo,m))
  #array(0*c(1:nMonteCarlo*m),dim=c(nMonteCarlo,m))
  for(i in 1:nMonteCarlo){
    # This is for the prevalence w.
    pre_w = theAUC_generate_w(w, alpha1w, alpha2w, version = "prior")
    
    FPRc_opt = FPR[i, ][c_optfDfND]
    FNRc_opt = FNR[i, ][c_optfDfND]
    ERROR_wc_opt = ERROR_w[i, ][c_optfDfND]
    PPVc_opt = PPV[i, ][c_optfDfND]
    #print(c(FPRc_opt, FNRc_opt, ERROR_wc_opt))
    
    if ((pre_w*(1-FNRc_opt)+(1-pre_w)*FPRc_opt) !=  0){
      FDRc_opt = (1-pre_w)*FPRc_opt/(pre_w*(1-FNRc_opt)+(1-pre_w)*FPRc_opt)}
    if ((pre_w*FNRc_opt+(1-pre_w)*(1-FPRc_opt)) != 0){
      FNDRc_opt = pre_w*FNRc_opt/(pre_w*FNRc_opt+(1-pre_w)*(1-FPRc_opt))}
    
    for (i in 1:length(A)) {
      if ((FPRc_opt > A[i]) & (FPRc_opt <= A[i+1])) {priorFPRc_opt[i]=priorFPRc_opt[i]+1}
      if ((FNRc_opt > A[i]) & (FNRc_opt <= A[i+1])) {priorFNRc_opt[i]=priorFNRc_opt[i]+1}
      if ((ERROR_wc_opt > A[i]) & (ERROR_wc_opt <= A[i+1])) {priorERROR_wc_opt[i]=priorERROR_wc_opt[i]+1}
      if ((FDRc_opt > A[i]) & (FDRc_opt <= A[i+1])) {priorFDRc_opt[i]=priorFDRc_opt[i]+1}
      if ((FNDRc_opt > A[i]) & (FNDRc_opt <= A[i+1])) {priorFNDRc_opt[i]=priorFNDRc_opt[i]+1}
      if ((PPVc_opt > A[i]) & (PPVc_opt <= A[i+1])) {priorPPVc_opt[i]=priorPPVc_opt[i]+1}
    }
  }
  priorFPRc_opt = priorFPRc_opt/nMonteCarlo
  priorFNRc_opt = priorFNRc_opt/nMonteCarlo
  priorERROR_wc_opt = priorERROR_wc_opt/nMonteCarlo
  priorFDRc_opt = priorFDRc_opt/nMonteCarlo
  priorFNDRc_opt = priorFNDRc_opt/nMonteCarlo
  priorPPVc_opt = priorPPVc_opt/nMonteCarlo
  newlist = list("priorFPRc_opt" = priorFPRc_opt, "priorFNRc_opt" = priorFNRc_opt,
                 "priorERROR_wc_opt" = priorERROR_wc_opt, "priorFDRc_opt" = priorFDRc_opt,
                 "priorFNDRc_opt" = priorFNDRc_opt, "priorPPVc_opt" = priorPPVc_opt)
  return(newlist)
}

AUC_post_error_char_copt = function(c_optfDfND, nMonteCarlo, w = FALSE, 
                                    alpha1w = NA, alpha2w = NA, nD = NA, nND = NA, version = NA,
                                    delta, pND_array, pD_array, 
                                    FNR, FPR, ERROR_w, PPV, postc_opt){
  # need to add support for PPV - unsure how
  
  if(length(pND_array) != length(pD_array)){
    return("Error: the length of pND_array and pD_array are different.")
  }
  A = theAUC_grid(delta)
  postFPRc_opt = rep(0,(1/delta))
  postFNRc_opt = rep(0,(1/delta))
  postERROR_wc_opt = rep(0,(1/delta))
  postFDRc_opt = rep(0,1/delta)
  postFNDRc_opt = rep(0,1/delta)
  postPPVc_opt = rep(0,1/delta)
  #array(0*c(1:nMonteCarlo*m),dim=c(nMonteCarlo,m))
  #array(0*c(1:nMonteCarlo*m),dim=c(nMonteCarlo,m))
  for(i in 1:nMonteCarlo){
    # This is for the prevalence w.
    pre_w = theAUC_generate_w(w, alpha1w, alpha2w, nD, nND, version)
    
    FPRc_opt = FPR[i, ][c_optfDfND]
    FNRc_opt = FNR[i, ][c_optfDfND]
    ERROR_wc_opt = ERROR_w[i, ][c_optfDfND]
    PPVc_opt = PPV[i, ][c_optfDfND]
    
    if ((pre_w*(1-FNRc_opt)+(1-pre_w)*FPRc_opt) !=  0){
      FDRc_opt = (1-pre_w)*FPRc_opt/(pre_w*(1-FNRc_opt)+(1-pre_w)*FPRc_opt)}
    if ((pre_w*FNRc_opt+(1-pre_w)*(1-FPRc_opt)) != 0){
      FNDRc_opt = pre_w*FNRc_opt/(pre_w*FNRc_opt+(1-pre_w)*(1-FPRc_opt))}
    
    for (i in 1:length(A)) {
      if ((FPRc_opt > A[i]) & (FPRc_opt <= A[i+1])) {postFPRc_opt[i] = postFPRc_opt[i]+1}
      if ((FNRc_opt > A[i]) & (FNRc_opt <= A[i+1])) {postFNRc_opt[i] = postFNRc_opt[i]+1}
      if ((ERROR_wc_opt > A[i]) & (ERROR_wc_opt <= A[i+1])) {postERROR_wc_opt[i] = postERROR_wc_opt[i]+1}
      if ((FDRc_opt > A[i]) & (FDRc_opt <= A[i+1])) {postFDRc_opt[i] = postFDRc_opt[i]+1}
      if ((FNDRc_opt > A[i]) & (FNDRc_opt <= A[i+1])) {postFNDRc_opt[i] = postFNDRc_opt[i]+1}
      if ((PPVc_opt > A[i]) & (PPVc_opt <= A[i+1])) {postPPVc_opt[i] = postPPVc_opt[i]+1}
    }
  }
  postFPRc_opt = postFPRc_opt/nMonteCarlo
  postFNRc_opt = postFNRc_opt/nMonteCarlo
  postERROR_wc_opt = postERROR_wc_opt/nMonteCarlo
  postFDRc_opt = postFDRc_opt/nMonteCarlo
  postFNDRc_opt = postFNDRc_opt/nMonteCarlo
  postPPVc_opt = postPPVc_opt/nMonteCarlo
  newlist = list("postFPRc_opt" = postFPRc_opt, "postFNRc_opt" = postFNRc_opt,
                 "postERROR_wc_opt" = postERROR_wc_opt, "postFDRc_opt" = postFDRc_opt,
                 "postFNDRc_opt" = postFNDRc_opt, "postPPVc_opt" = postPPVc_opt)
  return(newlist)
}

################################################################
# FUNCTIONS FOR COMPUTATIONS                                   #
################################################################

simulate_AUC_mc_prior = function(nND, nD, nMonteCarlo, w = FALSE, 
                                 alpha1w = NA, alpha2w = NA,
                                 alpha_ND, alpha_D){ 
  # This is meant to simulate the prior of the AUC.
  # Remark: this is because the input can be a string due to R shiny's inputs
  alpha_priorND = create_necessary_vector(alpha_ND)
  alpha_priorD = create_necessary_vector(alpha_D)
  
  if (length(alpha_priorND) != length(alpha_priorD)){
    return("Lengths of alpha prior ND and alpha prior D are not the same.")
  }
  m = length(alpha_priorND)
  
  priorc_opt = rep(0,m) # NEW
  
  pND_array = array(0*c(1:nMonteCarlo*m),dim=c(nMonteCarlo,m))
  pD_array = array(0*c(1:nMonteCarlo*m),dim=c(nMonteCarlo,m))
  FNR = array(0*c(1:nMonteCarlo*m),dim=c(nMonteCarlo,m))
  FPR = array(0*c(1:nMonteCarlo*m),dim=c(nMonteCarlo,m))
  ERROR_w = array(0*c(1:nMonteCarlo*m),dim=c(nMonteCarlo,m))
  PPV = array(0*c(1:nMonteCarlo*m),dim=c(nMonteCarlo,m))
  
  AUC = rep(0, nMonteCarlo)
  
  for(i in 1:nMonteCarlo){
    # This is for the prevalence w.
    pre_w = theAUC_generate_w(w, alpha1w, alpha2w, version = "prior")
    
    pND_array[i, ] = rdirichlet(1,alpha_priorND)
    pD_array[i, ] = rdirichlet(1,alpha_priorD)
    FNR[i, ] = cumsum(pD_array[i, ]) #sum(pD_prior[1:i])
    FPR[i, ] = 1 - cumsum(pND_array[i, ])
    ERROR_w[i, ] = pre_w*FNR[i, ] + (1-pre_w)*FPR[i, ]
    PPV[i, ] = (pre_w*(1 - FNR[i, ]))/(pre_w*(1 - FNR[i, ]) + (1-pre_w)*FPR[i, ]) # TPR[i, ] = 1 - FNR[i, ]
    
    AUC[i] = sum((1-FNR[i, ])*pND_array[i,])
    
    # update the prior distribution of c_opt
    c_opt = which.min(ERROR_w[i, ])
    priorc_opt[c_opt] <- priorc_opt[c_opt]+1
  }
  
  priorc_opt = priorc_opt/nMonteCarlo
  
  # might also want to make a downloadable list
  newlist = list("pND_array" = pND_array, "pD_array" = pD_array,
                 "FNR" = FNR, "FPR" = FPR, "ERROR_w" = ERROR_w, 
                 "PPV" = PPV, "priorc_opt" = priorc_opt,
                 "AUC" = AUC)
  return(newlist)
}

simulate_AUC_mc_post = function(nND, nD, nMonteCarlo, w = FALSE, 
                                alpha1w = NA, alpha2w = NA, version = NA,
                                alpha_ND, alpha_D, fND, fD){ # removed m
  
  # Remark: this is because the input can be a string due to R shiny's inputs
  alpha_priorND = create_necessary_vector(alpha_ND)
  alpha_priorD = create_necessary_vector(alpha_D)
  fND = create_necessary_vector(fND)
  fD = create_necessary_vector(fD)
  
  test_valid_list = c(length(alpha_priorD), length(fND), length(fD))
  for(i in test_valid_list){
    if(i != length(alpha_priorND)){
      return("At least one of the vectors (alpha ND, alpha D, fND, or fD) are not the same length.")
    }
  }
  m = length(alpha_priorND)
  
  postc_opt = rep(0,m) # NEW
  
  pND_array = array(0*c(1:nMonteCarlo*m),dim=c(nMonteCarlo,m))
  pD_array = array(0*c(1:nMonteCarlo*m),dim=c(nMonteCarlo,m))
  FNR = array(0*c(1:nMonteCarlo*m),dim=c(nMonteCarlo,m))
  FPR = array(0*c(1:nMonteCarlo*m),dim=c(nMonteCarlo,m))
  ERROR_w = array(0*c(1:nMonteCarlo*m),dim=c(nMonteCarlo,m))
  PPV = array(0*c(1:nMonteCarlo*m),dim=c(nMonteCarlo,m))
  
  AUC = rep(0, nMonteCarlo)
  
  for(i in 1:nMonteCarlo){
    # This is for the prevalence w.
    pre_w = theAUC_generate_w(w, alpha1w, alpha2w, nD, nND, version)
    
    pND_array[i, ] = rdirichlet(1,alpha_priorND + fND)
    pD_array[i, ] = rdirichlet(1,alpha_priorD + fD)
    FNR[i, ] = cumsum(pD_array[i, ])
    FPR[i, ] = 1 - cumsum(pND_array[i, ])
    ERROR_w[i, ] = pre_w*FNR[i, ] + (1-pre_w)*FPR[i, ]
    PPV[i, ] = (pre_w*(1 - FNR[i, ]))/(pre_w*(1 - FNR[i, ]) + (1-pre_w)*FPR[i, ]) # TPR[i, ] = 1 - FNR[i, ]
    
    AUC[i] = sum((1-FNR[i, ])*pND_array[i,])
    
    # update the posterior distribution of c_opt
    c_opt = which.min(ERROR_w[i, ])
    postc_opt[c_opt] = postc_opt[c_opt]+1
  }
  
  #print(postc_opt)
  postc_opt = postc_opt/nMonteCarlo
  
  # might also want to make a downloadable list
  newlist = list("pND_array" = pND_array, "pD_array" = pD_array,
                 "FNR" = FNR, "FPR" = FPR, "ERROR_w" = ERROR_w, 
                 "PPV" = PPV, "postc_opt" = postc_opt,
                 "AUC" = AUC)
  return(newlist)
}

grab_AUC_densities_breaks = function(delta, AUC){
  # Essentially grabs the information of the histogram
  bins = theAUC_grid(delta)
  x = hist(AUC, breaks = bins, plot = FALSE)
  return(list("density" = x$density, "breaks" = x$breaks))
}

grab_AUC_RBR_densities_breaks = function(delta, AUC){
  # similar as grab_AUC_densities_breaks BUT for RBR data only.
  bins = theAUC_grid(delta)
  AUC[is.na(AUC)] = 0
  
  myhist <-list(breaks=bins, counts=AUC, density=AUC/delta)
  class(myhist) = "histogram"
  #print(myhist)
  #myhist$density = myhist$density[-length(myhist$density)] # removing the last element (not needed)
  
  return(list("density" = myhist$density, "breaks" = myhist$breaks, "counts" = myhist$counts))
}

compute_AUC_RBR = function(delta, AUC_prior, AUC_post, priorc_opt, postc_opt){
  # NEED TO COMPUTE THE FOLLOWING: RBR, plausible region, etc...
  
  RBc_opt = postc_opt/priorc_opt
  
  bins = theAUC_grid(delta)
  AUC_RBR = rep(0, length(bins))
  #AUC_RBR = rep(0, length(bins))
  AUC_prior_pts = grab_AUC_densities_breaks(delta, AUC_prior)$density
  AUC_post_pts = grab_AUC_densities_breaks(delta, AUC_post)$density
  
  for(i in 1:(length(bins)-1)){
    # if statement is to prevent division by 0
    if((AUC_prior_pts[i] > 0) == TRUE){
      AUC_RBR[i] = AUC_post_pts[i] / AUC_prior_pts[i]
    } else {
      AUC_RBR[i] = NA
    }
  }
  
  c_optfDfND <- which.max(RBc_opt)
  
  # REMARK: AUC_RBR goes from (0 to bin[1]), ..., to (bin[1/delta], 1)
  newlist = list("grid" = bins, "AUC_RBR" = AUC_RBR, "RBc_opt" = RBc_opt, 
                 "c_optfDfND" = c_optfDfND)
  return(newlist)
}

compute_AUC_plausible_region = function(delta, AUC_RBR, num_average_pts = 3){
  # Grab the initial densities and breaks
  initial_vals = grab_AUC_RBR_densities_breaks(delta, AUC_RBR)
  hist_breaks = initial_vals$breaks
  hist_counts = initial_vals$counts # NOTE: COUNTS, not DENSITY
  # Converts it to a line plot
  line_plot = convert_hist_to_density_plot(hist_counts, hist_breaks, num_average_pts)
  density = line_plot$density # note: not actually density in this case.
  grid = line_plot$grid
  
  density[is.na(density)] = 0 # change values to 0
  
  # Outputs a plausible region 
  plausible_region = c()
  for (i in 1:(length(density)-1)){
    if (density[i] > 1){
      #print(i)
      plausible_region = c(plausible_region, grid[i])
    }
  }
  plausible_region = c(plausible_region[1], plausible_region[length(plausible_region)])
  
  newlist = list("plausible_region" = plausible_region, "density" = density, "grid" = grid)
  return(newlist)
}

compute_AUC_credible_region = function(gamma, grid, density, AUC_post,
                                       posterior_content, plausible_region){
  # Note: credible region is now based on the line plot.
  
  # Computes the credible region. At first, there's no default input to avoid generating
  # a credible region automatically (it is not necessary.)
  if (check.numeric(gamma) == FALSE){
    err_msg = "Need to put in a valid input for gamma (see graph 1.)"
    return(list("credible_region" = err_msg, "rb_line" = err_msg))
  }
  else { # This condition runs if the user provides an actual numeric input.
    gamma = as.numeric(gamma)
    if(gamma >= posterior_content){
      err_msg = "Gamma must be less than the posterior content of the plausible region."
      return(list("credible_region" = err_msg, "rb_line" = err_msg))
    } 
    # NEED TO MODIFY THIS!!
    else {
      RBR_values = sort(density, decreasing = TRUE)
      RBR_values = RBR_values[RBR_values > 1] # sorting for values larger than 1
      for(i in 2:length(RBR_values)){ # doesnt start at the top as the AREA of a line is 0
        rb_line = RBR_values[i]
        credible_region = c()
        # find the region associated with it
        # WARNING: BOLD ASSUMPTION NO BREAKPOINTS/PEAKS
        for(j in 1:length(density)){
          if(density[j] > rb_line){
            credible_region = c(credible_region, grid[j])
          }
        }
        credible_region = c(min(credible_region), max(credible_region))
        
        delta = grid[3] - grid[2] # hardcoded, but very unlikely that grid values will be less than 3.
        test_area = compute_AUC_post_content(delta, AUC_post, credible_region)
        if(test_area >= gamma){
          break # This means the credible region was actually found
        }
      }
      newlist = list("credible_region" = credible_region, "rb_line" = rb_line)
      #print(newlist)
      # Note: rb_line should be the upper dotted line - helps define a valid credible region
      return(newlist)
    }
  }
}

compute_AUC_error_char_copt = function(delta, c_optfDfND, priorFPRc_opt, priorFNRc_opt, 
                                       priorERROR_wc_opt, priorFDRc_opt, priorFNDRc_opt,
                                       priorPPVc_opt, postFPRc_opt, postFNRc_opt, 
                                       postERROR_wc_opt, postFDRc_opt, postFNDRc_opt,
                                       postPPVc_opt){
  grid = theAUC_grid(delta)
  
  RBFPRc_opt = postFPRc_opt/priorFPRc_opt
  FPRc_optfDfND = grid[which.max(RBFPRc_opt)]
  
  RBFNRc_opt = postFNRc_opt/priorFNRc_opt
  FNRc_optfDfND = grid[which.max(RBFNRc_opt)]
  
  RBERROR_wc_opt = postERROR_wc_opt/priorERROR_wc_opt
  ERROR_wc_optfDfND = grid[which.max(RBERROR_wc_opt)]
  
  RBFDRc_opt = postFDRc_opt/priorFDRc_opt
  FDRc_optfDfND = grid[which.max(RBFDRc_opt)]
  
  RBFNDRc_opt=postFNDRc_opt/priorFNDRc_opt
  FNDRc_optfDfND=grid[which.max(RBFNDRc_opt)]
  
  RBPPVc_opt = postPPVc_opt/priorPPVc_opt
  PPVc_optfDfND=grid[which.max(RBPPVc_opt)]
  
  newlist = list("FPRest(copt_est)" = FPRc_optfDfND, "FNRest(copt_est)" = FNRc_optfDfND,
                 "ERRORwest(copt_est)" = ERROR_wc_optfDfND, "FDRest(copt_est)" = FDRc_optfDfND,
                 "FNDRest(copt_est)" = FNDRc_optfDfND, "PPVest(copt_est)" = PPVc_optfDfND)
  return(newlist)
}

compute_AUC_post_content = function(delta, AUC_post, plausible_region){
  # Delta doesn't exactly start at 0,
  # so the area computed here is different than what's on the paper!!!!!
  bins = theAUC_grid(delta)
  #bins = c(bins, 1)
  AUC_post_content = 0
  AUC_post_pts = grab_AUC_densities_breaks(delta, AUC_post)$density
  
  # Check to see if a match exists:
  if(plausible_region[1] %in% bins){
    start = match(plausible_region[1], bins)
  } else {
    for(i in 1:length(bins)){
      if(bins[i] > plausible_region[1]){
        start = i
        break
      }
    }
  }
  if(plausible_region[2] %in% bins){
    end = match(plausible_region[2], bins)
  } else {
    for(i in length(bins):1){
      if(bins[i] < plausible_region[2]){
        end = i
        break
      }
    }
  }
  
  for(i in start:end){
    AUC_post_content = AUC_post_content + AUC_post_pts[i] * delta
  }
  
  return(AUC_post_content)
}

grab_density_plot_area = function(grid, density){
  area = sum(diff(grid) * (head(density,-1)+tail(density,-1)))/2
  return(area)
}

hypothesized_AUC_compute_values = function(hypo_AUC, delta, AUC_prior, AUC_post){
  priors = c(0, grab_AUC_densities_breaks(delta, AUC_prior)$density*delta)
  posts = c(0, grab_AUC_densities_breaks(delta, AUC_post)$density*delta)
  
  grid = theAUC_grid(delta)
  for(i in 1:length(grid)){
    if(grid[i] >= hypo_AUC){
      start_loop = i
      break
    }
  }
  
  priorprob=0
  postprob=0
  for(i in start_loop:length(grid)){
    priorprob=priorprob+priors[i]
    postprob=postprob+posts[i]
  }
  RB=postprob/priorprob

  return(RB)
}

################################################################
# FUNCTIONS FOR GRAPHS                                         #
################################################################

convert_hist_to_density_plot = function(hist_density, hist_breaks, num_average_pts = 3, 
                                        showplot = FALSE, colour = "black"){
  # num_average_pts: the number of density bins closely added to each other to get
  # a smoother density plot. (Reduce peaks.)
  
  if(num_average_pts %% 2 == 0){
    # Note: the even case is harder to code. For this instance, since the number of average points
    # will be pre-determined for the user (in terms of the plots), I have decided to not add
    # even functionality for now.
    "Error: num_average_pts must be an odd number."
  } 
  new_grid = rep(0, (length(hist_breaks) - 1))
  for(i in 1:(length(hist_breaks)-1)){
    new_grid[i] = (hist_breaks[i] + hist_breaks[i+1])/2
  }
  if(num_average_pts == 1){
    if(showplot == TRUE){
      lines(new_grid, hist_density, lty = 2, type = "l", lwd = 3, col = colour)#, add = TRUE)
    }
    return(list("grid" = new_grid, "density" = hist_density))
    #plot(new_grid, hist_density, lty = 1, type = "l")#, add = TRUE)
  } else {
    # when we have more items to average out
    new_density = rep(0, length(hist_density))
    
    num_neighbours = floor(num_average_pts/2)
    pts = 1
    for(i in 1:length(hist_density)){
      if(i < num_neighbours | (length(hist_density) - i) < num_neighbours){
        if(i == 1 | i == length(hist_density)){
          new_density[i] = hist_density[i]
        } else {
          new_density[i] = sum(hist_density[(i-pts):(i+pts)])/(2*pts + 1)
          pts = pts + 1
        }
      } else {
        pts = 1
        lower_int = i - num_neighbours
        upper_int = i + num_neighbours
        new_density[i] = sum(hist_density[lower_int:upper_int])/(2*num_neighbours + 1)
      }
    }
    #print(new_grid)
    #print(new_density)
    if(showplot == TRUE){
      lines(new_grid, new_density, lty = 2, type = "l", lwd = 3, col = colour)#, add = TRUE)
    }
  }
  return(list("grid" = new_grid, "density" = new_density))
}

density_hist_AUC_prior_post = function(delta, AUC_prior, AUC_post, plausible_region,
                                       credible_region = FALSE, densityplot = FALSE, 
                                       showbars = FALSE){
  bins = theAUC_grid(delta)
  
  if(densityplot == FALSE & showbars == FALSE){ # this is to extract the density values only
    hist_post = hist(AUC_post, breaks = bins,plot = FALSE) 
    
    hist_prior = hist(AUC_prior, breaks = bins,plot = FALSE)
    
    prior_vals = convert_hist_to_density_plot(hist_post$density, hist_post$breaks)
    
    post_vals = convert_hist_to_density_plot(hist_prior$density, hist_prior$breaks)
    
    prior_linearea = grab_density_plot_area(prior_vals$grid, prior_vals$density)
    post_linearea = grab_density_plot_area(post_vals$grid, post_vals$density)
    
    return(list("PriorArea" = prior_linearea, "PostArea" = post_linearea))
  }
  
  
  if(showbars == FALSE){
    hist_colours = c("#ffffff", "#ffffff")
  } else {
    hist_colours = c(rgb(102/255, 153/255, 255/255, alpha = 0.5),
                     rgb(255/255, 102/255, 102/255, alpha = 0.5))
  }

  hist_post = hist(AUC_post, prob = TRUE, breaks = bins, xlab="AUC", ylab="Density",
                    #ylim = c(0, max(AUC_prior,AUC_post)),
                    main="Density Histogram: The Prior & Posterior of the AUC", 
                    col = hist_colours[1], border = "#ffffff") 
  
  hist_prior = hist(AUC_prior, prob = TRUE, breaks = bins, xlab="AUC", ylab="Density",
       #     ylim = c(0, max(AUC_prior,AUC_post)),
       #main="Density Histogram of AUC", 
       col = hist_colours[2], border = "#ffffff", add = TRUE)
  
  abline(v=plausible_region[1], col="#947aff", lwd = 2, lty = 3)
  abline(v=plausible_region[2], col="#947aff", lwd = 2, lty = 3)
  
  legend("topleft", inset=.02, c("Prior","Posterior"), 
         fill=c(rgb(255/255, 102/255, 102/255, alpha = 0.5),
                rgb(102/255, 153/255, 255/255, alpha = 0.5)), 
         horiz=FALSE, cex=0.8)
  
  if(densityplot == TRUE){
    convert_hist_to_density_plot(hist_post$density, hist_post$breaks, num_average_pts = 3, showplot = TRUE,
                                 colour = rgb(102/255, 153/255, 255/255))
    convert_hist_to_density_plot(hist_prior$density, hist_prior$breaks, num_average_pts = 3, showplot = TRUE,
                                 colour = rgb(255/255, 102/255, 102/255))
  }
  
  if(typeof(credible_region) == "double"){
    abline(v=credible_region[1], col="#5b10a7", lwd = 2, lty = 3) # might change lty?
    abline(v=credible_region[2], col="#5b10a7", lwd = 2, lty = 3)
    
  }
}

density_hist_AUC_RBR = function(delta, AUC_RBR, plausible_region, credible_region = FALSE,
                                rb_line = FALSE, hypothesis = FALSE, densityplot = FALSE,
                                showbars = FALSE){
  bins = theAUC_grid(delta)
  #bins = c(0, bins, 1)
  
  if(showbars == FALSE){
    colours = "#ffffff"
  } else {
    colours = rgb(0/255, 255/255, 204/255, alpha = 0.5)
  }
  
  AUC_RBR[is.na(AUC_RBR)] = 0
  
  myhist <-list(breaks=bins, counts=AUC_RBR, density=AUC_RBR/delta)
  class(myhist) = "histogram"
  myhist$counts = myhist$counts[-length(myhist$counts)] #removing the last element
  
  plot(myhist, xlab = "AUC", ylab = "Relative Belief Ratio", 
       main = "Density Histogram: The Relative Belief Ratio of the AUC",
       col = colours, freq = TRUE,
       border = "#ffffff")
  abline(h=1, col="#3333FF", lwd = 2, lty = 2)
  abline(v=plausible_region[1], col="#947aff", lwd = 2, lty = 3)
  abline(v=plausible_region[2], col="#947aff", lwd = 2, lty = 3)
  #warning message doesn't seem to be much of an issue
  
  if(densityplot == TRUE){
    rbr_density_plot = convert_hist_to_density_plot(myhist$counts, myhist$breaks, 
                                                    num_average_pts = 3, showplot = TRUE,
                                                    colour = rgb(5/255, 222/255, 178/255))
    # Colouring in the area between the plausible region and when the RBR > 1
    l <- min(which(rbr_density_plot$grid >= plausible_region[1]))
    h <- max(which(rbr_density_plot$grid <= plausible_region[2]))
    polygon(c(rbr_density_plot$grid[c(l, l:h, h)]),
            c(1, rbr_density_plot$density[l:h], 1),
            col = rgb(255/255, 75/255, 195/255, alpha = 0.2), border = NA)
  }
  
  if((typeof(credible_region) == "double") & (typeof(rb_line) == "double") & (hypothesis == FALSE)){
    abline(h=rb_line, col="#3333FF", lwd = 2, lty = 2)
    abline(v=credible_region[1], col="#5b10a7", lwd = 2, lty = 3) # might change lty?
    abline(v=credible_region[2], col="#5b10a7", lwd = 2, lty = 3)
    
    polygon(x = c(credible_region[1], credible_region[1], credible_region[2], credible_region[2]), 
            y = c(0, rb_line, rb_line, 0), col = rgb(148/255, 180/255, 255/255, alpha = 0.2), 
            border = NA)   
    
    legend("topleft", legend = c("Plausible Region", "Credible Region"), lwd = 2, 
           col = c('#947aff', '#5b10a7'), lty = c(3, 3), inset=.02, cex=0.8)
    
  } else if ((typeof(credible_region) != "double") & (hypothesis == FALSE)) {
    legend("topleft", legend = c("Plausible Region"), lwd = 2, 
           col = c('#947aff'), lty = c(3), inset=.02, cex=0.8)
  } else if ((typeof(credible_region) != "double") & (check.numeric(hypothesis) == TRUE)){
    abline(v=hypothesis, col="#f1239d", lwd = 2, lty = 3) # might change lty?
    polygon(x = c(hypothesis, hypothesis, 1, 1),
            y = c(0, max(AUC_RBR), max(AUC_RBR), 0),
            col = rgb(241/255, 35/255, 157/255, alpha = 0.1),
            border = NA)
    legend("topleft", legend = c("Plausible Region", "Hypothesized AUC"), lwd = 2, 
           col = c('#947aff', "#f1239d"), lty = c(3, 1), inset=.02, cex=0.8)
  } else if ((typeof(credible_region) == "double") & (typeof(rb_line) == "double") & 
             (check.numeric(hypothesis) == TRUE)){
    abline(h=rb_line, col="#3333FF", lwd = 2, lty = 2)
    
    abline(v=credible_region[1], col="#5b10a7", lwd = 2, lty = 3) # might change lty?
    abline(v=credible_region[2], col="#5b10a7", lwd = 2, lty = 3)
    
    polygon(x = c(credible_region[1], credible_region[1], credible_region[2], credible_region[2]), 
            y = c(0, rb_line, rb_line, 0), col = rgb(148/255, 180/255, 255/255, alpha = 0.2), border = NA)
    abline(v=hypothesis, col="#f1239d", lwd = 2, lty = 3) # might change lty?
    polygon(x = c(hypothesis, hypothesis, 1, 1),
            y = c(0, max(AUC_RBR), max(AUC_RBR), 0),
            col = rgb(241/255, 35/255, 157/255, alpha = 0.1),
            border = NA)
    legend("topleft", legend = c("Plausible Region", "Credible Region", "Hypothesized AUC"), lwd = 2, 
           col = c('#947aff', '#5b10a7', '#f1239d'), 
           lty = c(3, 3, 2), inset=.02, cex=0.8)
  }
}

plots_AUC_copt = function(priorc_opt = FALSE, postc_opt = FALSE, RBc_opt = FALSE){
  if((typeof(priorc_opt) == "double") & (typeof(postc_opt) == "double")){
    if(length(priorc_opt) != length(postc_opt)){
      return("Error: length of priorc_opt and postc_opt are not the same.")
    }
    m = length(priorc_opt)
    plot(1:m,priorc_opt,xlab="X",ylab="probability",pch=3, lwd = 2, col = "red", 
         main = "Plot of the Prior and the Posterior of Copt")
    points(1:m,postc_opt,pch=4, lwd = 2, col = "blue")
    
    legend("topright", legend = c("Prior", "Posterior"),
           lwd = 2, cex = 1.2, col = c("red", "blue"), pch = c(3, 4), lty = c(NA, NA))
  } else if(typeof(RBc_opt) == "double"){
    m = length(RBc_opt)
    plot(1:m, RBc_opt, xlab="X", ylab="Relative Belief Ratio", pch=8, col = "#16957C",
         main = "Plot of the Relative Belief Ratio of Copt")
    legend("topright", legend = c("Relative Belief Ratio"),
           lwd = 2, cex = 1.2, col = c("#16957C"), pch = c(8), lty = c(NA))
  }
}

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
