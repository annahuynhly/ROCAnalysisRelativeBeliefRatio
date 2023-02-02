################################################################
# DESCRIPTION                                                  #
################################################################

theAUC_description = div(
  titlePanel("Page Description"),
  sidebarLayout(
    sidebarPanel(
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
      hr(style = "border-top: 1px solid #363e4f;"),
      
      h4("Inputs and their Meanings"),
      p(HTML("<ul>
            <li><b>Total Non-Diseased:</b> The amount of \"non-diseased\" individuals from
                   the total sample size. </li>
            <li><b>Total Diseased:</b> the total amount of \"diseased\" individuals from the
                   total sample size. </li>
            <li><b>Monte Carlo (Simulation) Sample Size:</b> the sample size to use the Monte Carlo method
                   to simulate the prior and the posterior. When computations are taking too long,
                   it is recommended to lower the sample size. </li>
            <li><b>Delta:</b> The distance that matters. It is the distance between any
                   two points on the grid. TODO: might change. </li>
            <li><b>alphaND1, ..., alphaNDm:</b> The parameters of the Dirichlet distribution of the
                   non-diseased sample. </li>
            <li><b>alphaD1, ..., alphaDm:</b> The parameters of the Dirichlet distribution of the
                   diseased sample. </li>
            <li><b>fND:</b> TO WRITE </li>
            <li><b>fD:</b> TO WRITE </li>
            <li><b>Gamma:</b> A value that's supposed to be less than the posterior content
                              of the plausible region.</li>
            <li><b>Hypothesized AUC (greater than):</b> input what the user hypothesizes AUC to be
                   greater than. </li>
            <li><b>Input File Name:</b> The name of the file you want to download. The .csv file
                   will include the grid points, the prior, the posterior,
                   and the relative belief ratio. </li>
         </ul>")),
      
     h4("Outputs and their Meanings"),
    p(HTML("<ul>
            <li><b>plausible_region:</b> plausible region. PR = { w : RB(w | ... ) > 1 }</li>
            <li><b>credible_region:</b> see the definition in the literature. :) </li>
            <li><b>OUTPUT_VARIABLE:</b> TODO </li>
         </ul>"))
    )
  )
)

################################################################
# OUTPUT 1 PAGE                                                #
################################################################

theAUC_plausible_region = div( 
  titlePanel("Plausible Region & More"),
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "theAUC_delta", 
                   tags$p("Delta"), value = 0.04, min = 0, max = 1),
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
      tabPanel("Plausible Region & More", verbatimTextOutput("theAUC_output1")),
    )
  )
)

################################################################
# GRAPH 1 PAGE                                                 #
################################################################

theAUC_plots = div( 
  titlePanel("Plots"), 
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "theAUC_gamma", label = "Gamma (must be less than posterior content)", 
                value = "NA")
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
# OUTPUT + GRAPH 2 PAGE                                        #
################################################################

theAUC_hypothesizedAUC = div( 
  titlePanel("Hypothesized AUC"),
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "theAUC_hypoAUC",
                   tags$p('Hypothesized AUC (greater than)', style = "font-size: 90%;"),value = 0.5)
    ),
    mainPanel(
      tabPanel("Relative Belief Plot of w0",
               fluidRow(splitLayout(cellWidths = c("60%", "35%"), 
                                    plotOutput(outputId = "theAUC_hypoAUC_graph"), 
                                    verbatimTextOutput("theAUC_hypoAUC_value")))),
    )
  )
)

################################################################
# DOWNLOAD PAGE                                                #
################################################################

theAUC_download_1 = div( 
  titlePanel("Download Prior & Posterior"), 
  sidebarLayout(
    sidebarPanel(
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

################################################################
# PAGE LOGIC                                                   #
################################################################

page_theAUC = div(
  titlePanel("Section 3.2: The AUC"),
  # OUTPUTTING THE VALUES
  tabsetPanel(type = "tabs",
              tabPanel("Description", theAUC_description),
              tabPanel("Plausible Region & More", theAUC_plausible_region),
              tabPanel("Plots", theAUC_plots),
              tabPanel("Hypothesized AUC", theAUC_hypothesizedAUC),
              tabPanel("Download Prior & Posterior", theAUC_download_1),
  )
)

################################################################
# MAIN FUNCTIONS                                               #
################################################################

#RB_distance_that_matters
theAUC_grid = function(delta){ # MIGHT NEED TO MOVE THIS OUT - USED IN OTHER FUNCTS
  # Creates a grid of values from 0 to 1
  grid = seq(0, 1, length= (1/delta)+1)
  return(grid)
}

simulate_AUC_mc_prior = function(nND, nD, nMonteCarlo, alpha_ND, alpha_D){ # removed m
  # Remark: this is because the input can be a string due to R shiny's inputs
  alpha_priorND = create_necessary_vector(alpha_ND)
  alpha_priorD = create_necessary_vector(alpha_D)
  
  if (length(alpha_priorND) != length(alpha_priorD)){
    return("Lengths of alpha prior ND and alpha prior D are not the same.")
  }
  m = length(alpha_priorND)
  
  pND_array = array(0*c(1:nMonteCarlo*m),dim=c(nMonteCarlo,m))
  pD_array = array(0*c(1:nMonteCarlo*m),dim=c(nMonteCarlo,m))
  FNR = array(0*c(1:nMonteCarlo*m),dim=c(nMonteCarlo,m))
  AUC = rep(0, nMonteCarlo)
  
  for(i in 1:nMonteCarlo){
    pND_array[i, ] = rdirichlet(1,alpha_priorND)
    pD_array[i, ] = rdirichlet(1,alpha_priorD)
    FNR[i, ] = cumsum(pD_array[i, ])
    AUC[i] = sum((1-FNR[i, ])*pND_array[i,])
  }
  
  # might also want to make a downloadable list
  newlist = list("pND_array" = pND_array, "pD_array" = pD_array,
                 "FNR" = FNR, "AUC" = AUC)
  return(newlist)
}

# post = short for posterior
simulate_AUC_mc_post = function(nND, nD, nMonteCarlo, alpha_ND, alpha_D, fND, fD){ # removed m
  
  # Remark: this is because the input can be a string due to R shiny's inputs
  alpha_priorND = create_necessary_vector(alpha_ND)
  alpha_priorD = create_necessary_vector(alpha_D)
  fND = create_necessary_vector(fND)
  fD = create_necessary_vector(fD)
  
  #print(c(alpha_priorND, alpha_priorD, fND, fD))
  #print(c(length(alpha_priorND), length(alpha_priorD), length(fND), length(fD)))
  
  test_valid_list = c(length(alpha_priorD), length(fND), length(fD))
  for(i in test_valid_list){
    if(i != length(alpha_priorND)){
      return("At least one of the vectors (alpha ND, alpha D, fND, or fD) are not the same length.")
    }
  }
  m = length(alpha_priorND)
  
  pND_array = array(0*c(1:nMonteCarlo*m),dim=c(nMonteCarlo,m))
  pD_array = array(0*c(1:nMonteCarlo*m),dim=c(nMonteCarlo,m))
  FNR = array(0*c(1:nMonteCarlo*m),dim=c(nMonteCarlo,m))
  AUC = rep(0, nMonteCarlo)
  
  for(i in 1:nMonteCarlo){
    pND_array[i, ] = rdirichlet(1,alpha_priorND + fND)
    pD_array[i, ] = rdirichlet(1,alpha_priorD + fD)
    FNR[i, ] = cumsum(pD_array[i, ])
    AUC[i] = sum((1-FNR[i, ])*pND_array[i,])
  }
  
  # might also want to make a downloadable list
  newlist = list("pND_array" = pND_array, "pD_array" = pD_array,
                 "FNR" = FNR, "AUC" = AUC)
  return(newlist)
}

#probably should change function name
grab_AUC_densities = function(delta, AUC){
  bins = theAUC_grid(delta)
  x = hist(AUC, breaks = bins, plot = FALSE)
  return(x$density)
}

# MIGHT BE ABLE TO GET THE LITERAL POINTS USING HIST
# TO MODIFY
compute_AUC_RBR = function(delta, AUC_prior, AUC_post){
  # NEED TO COMPUTE THE FOLLOWING: RBR, plausible region, etc...
  bins = theAUC_grid(delta)
  AUC_RBR = rep(0, length(bins))
  #AUC_RBR = rep(0, length(bins))
  AUC_prior_pts = grab_AUC_densities(delta, AUC_prior)
  AUC_post_pts = grab_AUC_densities(delta, AUC_post)
  
  for(i in 1:(length(bins)-1)){
    # if statement is to prevent division by 0
    if((AUC_prior_pts[i] > 0) == TRUE){
      AUC_RBR[i] = AUC_post_pts[i] / AUC_prior_pts[i]
    } else {
      AUC_RBR[i] = NA
    }
  }
  
  # Outputs a plausible region 
  temp_AUC_RBR = replace(AUC_RBR, is.na(AUC_RBR), 0) # temporarily replaces NA with 0 for pr
  plausible_region = c()
  for (i in 1:length(AUC_RBR)){
    if (temp_AUC_RBR[i] > 1){
      plausible_region = c(plausible_region, bins[i])
    }
  }
  plausible_region = c(plausible_region[1], plausible_region[length(plausible_region)])
  
  # REMARK: AUC_RBR goes from (0 to bin[1]), ..., to (bin[1/delta], 1)
  newlist = list("grid" = bins, "AUC_RBR" = AUC_RBR, "plausible_region" = plausible_region)
  
  return(newlist)
}

# PROBABLY SHOULD MENTION THIS FOR THE MEETING: delta doesn't exactly start at 0,
# so the area computed here is different than what's on the paper!!!!!

compute_AUC_post_content = function(delta, AUC_post, plausible_region){
  bins = theAUC_grid(delta)
  #bins = c(bins, 1)
  AUC_post_content = 0
  AUC_post_pts = grab_AUC_densities(delta, AUC_post)
  
  start = match(plausible_region[1], bins)
  end = match(plausible_region[2], bins)
  
  for(i in start:end){
    AUC_post_content = AUC_post_content + AUC_post_pts[i] * delta
  }
  
  return(AUC_post_content)
}

# NEED TO THINK ABOUT THE CREDIBLE REGION MORE -- NOT COMPLETE, DUE TO BIN EDGE CASE
compute_AUC_credible_region = function(gamma, delta, AUC_RBR, AUC_post,
                                       posterior_content, plausible_region){
  # temporarily turn NA to 0
  AUC_RBR[is.na(AUC_RBR)] = 0
  
  bins = theAUC_grid(delta)
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
      RBR_values = sort(AUC_RBR, decreasing = TRUE)
      RBR_values = RBR_values[RBR_values > 1] # sorting for values larger than 1
      for(i in 2:length(RBR_values)){ # doesnt start at the top as the AREA of a line is 0
        rb_line = RBR_values[i]
        credible_region = c()
        # find the region associated with it
        # WARNING: BOLD ASSUMPTION NO BREAKPOINTS, WHEN THEY EXIST
        for(j in 1:length(AUC_RBR)){
          if(AUC_RBR[j] > rb_line){
            credible_region = c(credible_region, bins[j])
          }
        }
        credible_region = c(min(credible_region), max(credible_region))
        
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

###### HYPOTHESIS TESTING
hypothesized_AUC_compute_values = function(hypo_AUC, delta, AUC_prior, AUC_post){
  priors = c(0, grab_AUC_densities(delta, AUC_prior)*delta)
  posts = c(0, grab_AUC_densities(delta, AUC_post)*delta)
  
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

############################# GRAPH BUILDING


density_hist_AUC_prior_post = function(delta, AUC_prior, AUC_post, plausible_region,
                                       credible_region = FALSE){
  bins = theAUC_grid(delta)
  
  hist(AUC_post, prob = TRUE, breaks = bins, xlab="AUC", ylab="Density",
       #ylim = c(0, max(AUC_prior,AUC_post)),
       main="Density Histogram: The Prior & Posterior of the AUC", 
       col = rgb(102/255, 153/255, 255/255, alpha = 0.5), border = "#ffffff")
  hist(AUC_prior, prob = TRUE, breaks = bins, xlab="AUC", ylab="Density",
       #     ylim = c(0, max(AUC_prior,AUC_post)),
       #main="Density Histogram of AUC", 
       col = rgb(255/255, 102/255, 102/255, alpha = 0.5), border = "#ffffff", add = TRUE)
  abline(v=plausible_region[1], col="#947aff", lwd = 2, lty = 3)
  abline(v=plausible_region[2], col="#947aff", lwd = 2, lty = 3)
  
  legend("topleft", inset=.02, c("Prior","Posterior"), 
         fill=c(rgb(255/255, 102/255, 102/255, alpha = 0.5),
                rgb(102/255, 153/255, 255/255, alpha = 0.5)), 
         horiz=FALSE, cex=0.8)
  
  if(typeof(credible_region) == "double"){
    abline(v=credible_region[1], col="#5b10a7", lwd = 2, lty = 3) # might change lty?
    abline(v=credible_region[2], col="#5b10a7", lwd = 2, lty = 3)
    
  }
}

density_hist_AUC_RBR = function(delta, AUC_RBR, plausible_region, credible_region = FALSE,
                                hypothesis = FALSE){
  bins = theAUC_grid(delta)
  #bins = c(0, bins, 1)
  
  AUC_RBR[is.na(AUC_RBR)] = 0
  
  myhist <-list(breaks=bins, counts=AUC_RBR, density=AUC_RBR/delta)
  class(myhist) = "histogram"
  
  plot(myhist, xlab = "AUC", ylab = "Relative Belief Ratio", 
       main = "Density Histogram: The Relative Belief Ratio of the AUC",
       col = rgb(0/255, 255/255, 204/255, alpha = 0.5), freq = TRUE,
       border = "#ffffff")
  abline(h=1, col="#2e10a7", lwd = 2, lty = 2)
  abline(v=plausible_region[1], col="#947aff", lwd = 2, lty = 3)
  abline(v=plausible_region[2], col="#947aff", lwd = 2, lty = 3)
  #warning message doesn't seem to be much of an issue
  
  if((typeof(credible_region) == "double") & (hypothesis == FALSE)){
    abline(v=credible_region[1], col="#5b10a7", lwd = 2, lty = 3) # might change lty?
    abline(v=credible_region[2], col="#5b10a7", lwd = 2, lty = 3)
    
    polygon(x = c(credible_region[1], credible_region[1], credible_region[2], credible_region[2]), 
            y = c(0, max(AUC_RBR), max(AUC_RBR), 0), col = rgb(148/255, 180/255, 255/255, alpha = 0.2), 
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
  } else if ((typeof(credible_region) == "double") & (check.numeric(hypothesis) == TRUE)){
    abline(v=credible_region[1], col="#5b10a7", lwd = 2, lty = 3) # might change lty?
    abline(v=credible_region[2], col="#5b10a7", lwd = 2, lty = 3)
    
    polygon(x = c(credible_region[1], credible_region[1], credible_region[2], credible_region[2]), 
            y = c(0, max(AUC_RBR), max(AUC_RBR), 0), col = rgb(148/255, 180/255, 255/255, alpha = 0.2), border = NA)
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

theAUC_generate_dataframe = function(delta, AUC_prior, AUC_post, AUC_RBR){
  
  grid_pts = theAUC_grid(delta)
  #TEMPORARILY CHANGE THE AUC_prior_pts
  #AUC_prior_pts = c(0, grab_AUC_densities(delta, AUC_prior)*delta)
  #AUC_post_pts = c(0, grab_AUC_densities(delta, AUC_post)*delta)
  AUC_prior_pts = c(0, grab_AUC_densities(delta, AUC_prior))
  AUC_post_pts = c(0, grab_AUC_densities(delta, AUC_post))
  #print(c(length(grid_pts), length(AUC_prior_pts), length(AUC_post_pts), length(AUC_RBR)))
  df = data.frame(grid_pts, AUC_prior_pts, AUC_post_pts, AUC_RBR)
  colnames(df) = c("Grid point", "Prior of the AUC", "Posterior of the AUC", "Relative Belief Ratio of the AUC")
  return(df)
}




# TESTING
# nND, nD, nMonteCarlo, alpha_ND, alpha_D
#nND = 50
#nD = 100
#nMonteCarlo = 10000
#alpha_ND = c(1, 1, 1, 1, 1) 
#alpha_D = c(1, 1, 1, 1, 1)
####m = 5
#fND = "29, 7, 4, 5, 5"
#fD = "14, 7, 25, 33, 21"
#delta = 0.01
#gamma = 0.5

#test1 = simulate_AUC_mc_prior(nND, nD, nMonteCarlo, alpha_ND, alpha_D)
#test2 = simulate_AUC_mc_post(nND, nD, nMonteCarlo, alpha_ND, alpha_D, fND, fD)
#grid = theAUC_grid(delta)
#prior_pts = c(0, grab_AUC_densities(delta, test1$AUC))

#smoothingSpline = smooth.spline(grid, prior_pts, spar=0.7)
#plot(grid,prior_pts)
#lines(smoothingSpline)



#test3 = compute_AUC_RBR(delta, test1$AUC, test2$AUC)

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

#density_hist_AUC_RBR(theAUC_delta, test3$AUC_RBR, test3$plausible_region, test5$credible_region)



# FOR THE SEPARATE CASE
#density_hist_AUC_IGNORE(theAUC_delta, test$AUC, "prior")
#density_hist_AUC_IGNORE(theAUC_delta, test2$AUC, "posterior")




#OLD

# older version - might keep depending on what Mike wants.
density_hist_AUC_IGNORE = function(theAUC_delta, AUC, type){
  if(tolower(type) == "prior"){
    hist_type = "prior"
  } else if (tolower(type) == "posterior" | tolower(type) == "post"){
    hist_type = "posterior"
  } else {
    return("Invalid type. You need to either use prior or posterior.")
  }
  # TODO: might want to generalize the graph?
  bins = theAUC_grid(theAUC_delta)
  hist(AUC, prob = TRUE, breaks = bins, xlab="AUC", ylab="Density",
       main=paste("Histogram of AUC ", "(from the ", hist_type, ")", sep = ""), 
       col = '#cbcbcb', border = "#ffffff")
}

hypothesized_AUC_compute_values_IGNORE = function(hypo_AUC, delta, AUC_RBR){
  # note: for hypo_AUC, we assume it will be for (hypo_AUC, 1)
  AUC_RBR[is.na(AUC_RBR)] = 0
  
  grid = theAUC_grid(delta)
  
  for(i in 1:length(grid)){
    if(grid[i] >= hypo_AUC){
      start_loop = i
      break
    }
  }
  area = 0
  for(j in start_loop:(length(grid))){
    area = area + (AUC_RBR[j] * delta)
  }
  return(area)
}

