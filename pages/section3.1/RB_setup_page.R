################################################################
# DESCRIPTION PAGE                                             #
################################################################

RB_setup_description = div( 
  titlePanel("Page Description"),
  p("Need to work on this section. Come back later!"),
  p("The goal of this section is for the user to compute the prior, posterior, the relative belief 
    ratio, and the strength."),
  p("Some inputs are shown in different pages based on the outputs."),
  p("Plot images can be saved by right clicking them."),
  hr(style = "border-top: 1px solid #363e4f;"),
  
  h4("Inputs and their Meanings"),
  p(HTML("<ul>
            <li><b>alpha1w:</b> The first parameter for beta. </li>
            <li><b>alpha2w:</b> The second parameter for beta. </li>
            <li><b>Total Sample Size:</b> The amount of  </li>
            <li><b>Total Diseased:</b> the total amount of \"diseased\" individuals from the
                   total sample size. </li>
            <li><b>Delta:</b> The distance that matters. It is the distance between any
                              two points on the grid. </li>
            <li><b>Gamma:</b> TODO </li>
            <li><b>Hypothesis w = w0:</b> TODO </li>
            <li><b>Input File Name:</b> The name of the file you want to download. The .csv file
                                        will include the grid points, the prior, the posterior,
                                        and the relative belief ratio. </li>
         </ul>")),
  
  h4("Outputs and their Meanings"),
  p(HTML("<ul>
            <li><b>pr_interval:</b> plausible region. PR = { w : RB(w | ... ) > 1 }</li>
            <li><b>max_w:</b> the chosen value for w (when RB(w | ... ) is maximized. 
            It is actually just n/nD.)</li>
            <li><b>relative_belief_ratio_at_w0:</b> directly calculated from RB(w0 | ... )</li>
            <li><b>w0_interval:</b> the region once RB(w | ... ) > RB(w0 | ... )</li>
            <li><b>strength:</b> given as π({w : RB(w | ... ) <= RB(w0 | ... )| ... }) 
            It is highlighted in the second graph.</li>
         </ul>")),
)

################################################################
# OUTPUT 1 PAGE                                                #
################################################################

RB_setup_plausible_region = div( 
  titlePanel("Plausible Region & Max w"),
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "RB_setup_alpha1w", # CHANGE THIS
                   tags$p('alpha1w', style = "font-size: 90%;"),value = 391.72),
      numericInput(inputId = "RB_setup_alpha2w", # CHANGE THIS
                   tags$p('alpha2w', style = "font-size: 90%;"),value = 211.39),
      numericInput(inputId = "RB_setup_n", # CHANGE THIS
                   tags$p('Total Sample Size', style = "font-size: 90%;"),value = 100, min = 1),
      numericInput(inputId = "RB_setup_nD", # CHANGE THIS
                   tags$p('Total Diseased', style = "font-size: 90%;"),value = 68, min = 0),
      numericInput(inputId = "RB_delta", 
                   tags$p("Delta"), value = 0.001, min = 0, max = 1),
    ),
    mainPanel(
      tabPanel("Plausible Region & Max w", verbatimTextOutput("RB_setup_values1")),
    )
  )
)

################################################################
# GRAPH 1 PAGE                                                 #
################################################################

RB_setup_plots = div( 
  titlePanel("Plots"), 
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "RB_gamma", label = "Gamma (must be less than sup_gamma)", 
                value = "NA")
      #numericInput(inputId = "RB_gamma", # Changed to text to deal with edge case where gamma is not chosen
      #             tags$p('Gamma', style = "font-size: 90%;"),value = 0.8), # need to change value
    ),
    mainPanel(
      tabPanel("Plots",
               fluidRow(splitLayout(cellWidths = c("50%", "50%"), 
                                    plotOutput("RB_setup_postprior_graph"), 
                                    plotOutput("RB_setup_RB_graph")))),
    )
  )
)

################################################################
# OUTPUT + GRAPH 2 PAGE                                        #
################################################################

RB_setup_relative_belief_plot_of_w0 = div( 
  titlePanel("Plot of Relative Belief Ratio at w0"),
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "RB_setup_w0",
                   tags$p('Hypothesis w = w0', style = "font-size: 90%;"),value = 0.6)
    ),
    mainPanel(
      tabPanel("Relative Belief Plot of w0",
               fluidRow(splitLayout(cellWidths = c("50%", "45%"), 
                                    plotOutput(outputId = "RB_setup_w0_graph"), 
                                    verbatimTextOutput("RB_setup_values2")))),
    )
  )
)

################################################################
# DOWNLOAD PAGE                                                #
################################################################

page_RB_download = div( 
  titlePanel("Download Output"), 
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "RB_filename", "Input File Name", 
                value = "PriorPostRelativeBelief"),
      downloadButton("RB_downloadData", "Download"),
    ),
    mainPanel(
      tabPanel("Download Output", dataTableOutput("RB_dataframe"))
    )
  )
)


################################################################
# PAGE LOGIC                                                   #
################################################################

page_RB_setup = div( 
  # This is the page that that connects to app.R
  titlePanel("Section 3.1: Relative Belief Ratio Setup"), 
  tabsetPanel(type = "tabs",
              tabPanel("Description", RB_setup_description), 
              tabPanel("Plausible Region & Max w", RB_setup_plausible_region),
              tabPanel("Plots", RB_setup_plots),
              #tabPanel("Strength of w0", RB_setup_Strength_of_w0),
              tabPanel("Relative Belief Plot of w0", RB_setup_relative_belief_plot_of_w0),
              tabPanel("Download Output", page_RB_download)
  )
)

################################################################
# MAIN FUNCTIONS                                               #
################################################################

RB_distance_that_matters = function(delta){
  # Creates a grid of values from 0 to 1
  grid = seq(delta/2, (1-delta/2), length= 1/delta)
  return(grid)
}

RB_compute_values = function(alpha1w, alpha2w, n, nD, grid){
  # This computes the prior, posterior, and the relative beief ratio.
  nND = n - nD # obtaining number not diseased
  
  prior = dbeta(grid, alpha1w, alpha2w)
  post = dbeta(grid, alpha1w + nD, alpha2w + nND) # post is short for posterior
  
  relative_belief_ratio = post/prior
  
  # Force NA to be 0 for easier computations - 
  relative_belief_ratio[is.na(relative_belief_ratio)] = 0
  
  # Outputs a plausible region
  plausible_region = c()
  for (i in 1:length(grid)){
    if (relative_belief_ratio[[i]] > 1){
      plausible_region = c(plausible_region, as.numeric(grid[i]))
    }
  }
  
  # Finding maximum w based on the grid points
  max_w = (match(max(relative_belief_ratio), relative_belief_ratio))/length(grid)
  
  # This finds a plausible region.
  # WARNING: the following interval assumes there are no breaks, so it wouldn't work if there's a "peak".
  pr_interval = c(plausible_region[1], plausible_region[length(plausible_region)])
  
  # finding the prior and posterior content
  prior_content = pbeta(pr_interval[2], alpha1w, alpha2w) - pbeta(pr_interval[1], alpha1w, alpha2w)
  posterior_content = pbeta(pr_interval[2], alpha1w + nD, alpha2w + nND) - pbeta(pr_interval[1], alpha1w + nD, alpha2w + nND)
  
  # finding the area for π(pl(x) | x)... Need to change variable name!
  sup_gamma = pr_interval[2] - pr_interval[1] # Note that length is 1, so...
  
  newlist = list("nND" = nND, "prior" = prior, "post" = post, 
                 "relative_belief_ratio" = relative_belief_ratio, "pr_interval" = pr_interval,
                 "prior_content" = prior_content, "posterior_content" = posterior_content,
                 "max_w" = max_w, "sup_gamma" = sup_gamma)
  return(newlist)
}

compute_credible_region = function(gamma, relative_belief, grid, sup_gamma, pr_interval){
  # Computes the credible region. At first, there's no default input to avoid generating
  # a credible region automatically (it is not necessary.)
  if (check.numeric(gamma) == FALSE){
    err_msg = "Need to put in a valid input for gamma (see graph 1.)"
    return(list("credible_region" = err_msg, "rb_line" = err_msg))
  }
  else{ # This condition runs if the user provides an actual numeric input.
    gamma = as.numeric(gamma)
    if(gamma >= sup_gamma){
      err_msg = "Gamma must be less than sup_gamma."
      return(list("credible_region" = err_msg, "rb_line" = err_msg))
    } 
    else {
      dist_from_pr = sup_gamma - gamma
      credible_region = c(pr_interval[1] + dist_from_pr/2, pr_interval[2] - dist_from_pr/2)
      
      # Finding a new horizontal axis
      rb_vals = c() # will need to change variable name
      for (i in 1:length(grid)){
        if(grid[i] > credible_region[1] & grid[i] < credible_region[2]){
           rb_vals = c(rb_vals, relative_belief[i])
        }
      }
      rb_line = min(rb_vals)
      
      newlist = list("credible_region" = credible_region, "rb_line" = rb_line)
      return(newlist)
    }
  }
}


generate_prior_post_graph = function(prior, post, pr_interval, grid, credible_region = FALSE){
  # This generates the graph for the prior and the posterior.
  
  # Determining what x-axis to show for a better graph
  x_region = c()
  for (i in 1:length(grid)){
    if ((prior[[i]] > 0.1) | (post[[i]] > 0.1)){
      x_region = c(x_region, as.numeric(grid[i]))
    }
  }
  # Constructs an interval for the x and y region
  x_interval = c(x_region[1], x_region[length(x_region)])
  y_interval = c(0, max(c(prior, post)))
  
  # Plots of the Prior and the Posterior
  plot(grid, prior, type='l', lty = 2, lwd = 2, xlim = x_interval, ylim = y_interval,
       main = "Graph of the Prior and Posterior", ylab = "Values", xlab = "w", col = "blue")
  lines(grid, post,col="green", type = "l", lty = 2, lwd = 2)
  abline(v=pr_interval[1], col="#b3bfff", lwd = 2, lty = 3)
  abline(v=pr_interval[2], col="#b3bfff", lwd = 2, lty = 3)
  polygon(grid, post, col = rgb(146/255, 255/255, 133/255, alpha = 0.3), border = NA)
  polygon(grid, prior, col = rgb(133/255, 198/255, 255/255, alpha = 0.3), border = NA)
  
  if (typeof(credible_region) == "double") { # need both to run properly
    abline(v=credible_region[1], col="#81ddff", lwd = 2, lty = 3) # might change lty?
    abline(v=credible_region[2], col="#81ddff", lwd = 2, lty = 3)
    #abline(h=rb_line, col="#81ddff", lwd = 2, lty = 2)
    legend("topleft", legend = c("Prior", "Posterior", "Plausible Region", "Credible Region"), lwd = 2, 
           col = c('blue', 'green', '#b3bfff', "#81f5ff"), 
           lty = c(2, 2, 3, 3))
  } else {
    legend("topleft", legend = c("Prior", "Posterior", "Plausible Region"), lwd = 2, 
           col = c('blue', 'green', '#b3bfff'), 
           lty = c(2, 2, 3))
  }
}

generate_rb_graph = function(relative_belief_ratio, pr_interval, grid, credible_region = FALSE,
                             rb_line = FALSE){
  # This generates the graph for the relative belief ratio.
  
  # Determining what x-axis to show for a better graph
  x_region = c()
  for (i in 1:length(grid)){
    if (relative_belief_ratio[[i]] > 0.05){
      x_region = c(x_region, as.numeric(grid[i]))
    }
  }
  # Constructs an interval for the x and y region
  x_interval = c(x_region[1], x_region[length(x_region)])
  y_interval = c(0, max(relative_belief_ratio))
  # For the Plausible Region
  lower_bd = pr_interval[1]
  upper_bd = pr_interval[length(pr_interval)]
  
  plot(grid, relative_belief_ratio, type='l', lty = 2, lwd = 2, xlim = x_interval, ylim = y_interval,
       main = "Graph of the Relative Belief Ratio", ylab = "Values", xlab = "w", col = "red")
  abline(h=1, col="royalblue1", lwd = 2, lty = 2)
  abline(v=lower_bd, col="#b3bfff", lwd = 2, lty = 3)
  abline(v=upper_bd, col="#b3bfff", lwd = 2, lty = 3)
  # Colouring in the area between the plausible region and when the RBR > 1
  l <- min(which(grid >= pr_interval[1]))
  h <- max(which(grid < pr_interval[length(pr_interval)]))
  polygon(c(grid[c(l, l:h, h)]),
          c(1, relative_belief_ratio[l:h], 1),
          col = rgb(197/255, 132/255, 255/255, alpha = 0.3), border = NA)
  
  if ((typeof(credible_region) == "double") & (typeof(rb_line) == "double")) { # need both to run properly
    abline(v=credible_region[1], col="#81ddff", lwd = 2, lty = 3) # might change lty?
    abline(v=credible_region[2], col="#81ddff", lwd = 2, lty = 3)
    abline(h=rb_line, col="#81ddff", lwd = 2, lty = 2)
    legend("bottomleft", legend = c("Relative Belief Ratio", "Plausible Region", "Credible Region"), lwd = 2, 
           col = c('red', '#b3bfff', '#81f5ff'), lty = c(2, 3, 3))
  } else {
    legend("topleft", legend = c("Relative Belief Ratio", "Plausible Region"), lwd = 2, 
           col = c('red', '#b3bfff'), lty = c(2, 3))
  }
}


w0_compute_values = function(alpha1w, alpha2w, n, nD, w0, relative_belief_ratio, grid){
  # This is for hypothesis testing and computing the strength.
  nND = n - nD
  prior_w0 = dbeta(w0, alpha1w, alpha2w)
  post_w0 = dbeta(w0, alpha1w + nD, alpha2w + nND)
  relative_belief_ratio_at_w0 = post_w0/prior_w0
  
  RB_integrand = function(w){dbeta(w, alpha1w + nD, alpha2w + nND)/dbeta(w, alpha1w, alpha2w)}
  
  # Find an interval where the prior isn't 0 in order to properly compute the integrand 
  valid_interval = c()
  for (i in 1:length(grid)){
    if (dbeta(grid[i], alpha1w, alpha2w) != 0){
      valid_interval = c(valid_interval, as.numeric(grid[i]))
    }
  }
  
  # Locating the intersections when R(w | .) > R(w0 | .)
  nonstrength_interval = c()
  for (i in 1:length(grid)){
    if ((relative_belief_ratio[[i]] > relative_belief_ratio_at_w0) | (relative_belief_ratio[[i]] == relative_belief_ratio_at_w0)){
      nonstrength_interval = c(nonstrength_interval, as.numeric(grid[i]))
    }
  }
  
  if (is.null(nonstrength_interval)){ # This case would mean that R(w | .) < R(w0 | .) always!
    strength = 0
  } else {
    interval = c(nonstrength_interval[1], nonstrength_interval[length(nonstrength_interval)])
    # Whole area of RB(w | .)
    x = integrate(RB_integrand, lower = valid_interval[1], upper = valid_interval[length(valid_interval)])
    # Subtract region that contains where RB(w0 | .) > RB(w | .)
    y = integrate(RB_integrand, lower = interval[1], upper = interval[length(interval)])
    # Adding absolute error onto their area (otherwise we may receive a negative value)
    strength = (x$value + x$abs.error) - (y$value + y$abs.error)
  }
  
  newlist = list("relative_belief_ratio_at_w0" = relative_belief_ratio_at_w0, "w0_interval" = interval, 
                 "strength" = strength)
  return(newlist)
}

# TODO: CHANGE FUNCTION NAME
generate_relative_belief_ratio_at_w0_graph = function(relative_belief_ratio, relative_belief_ratio_at_w0, 
                                                      w0_interval, grid){
  x_region = c()
  for (i in 1:length(grid)){
    if (relative_belief_ratio[[i]] > 0.05){
      x_region = c(x_region, as.numeric(grid[i]))
    }
  }
  # note: the following interval assumes there are breaks (for now):
  x_interval = c(x_region[1], x_region[length(x_region)])
  y_interval = c(0, max(relative_belief_ratio))
  lower_bd = w0_interval[1]
  upper_bd = w0_interval[length(w0_interval)]
  
  plot(grid, relative_belief_ratio, type='l', lty = 2, lwd = 2, xlim = x_interval, 
       ylim = y_interval, main = "Graph of the Relative Belief Ratio at w0", ylab = "Values", xlab = "w", col = "red")
  abline(h=relative_belief_ratio_at_w0, lwd = 2, lty = 2, col="navy")
  abline(v=lower_bd, col="#7c83e8", lwd = 2, lty = 3)
  abline(v=upper_bd, col="#7c83e8", lwd = 2, lty = 3)
  
  legend("topleft", legend = c("Relative Belief Ratio", "Relative Belief Ratio of w0", 
                               "Interval", "Strength (Area)"), 
         lwd = c(2,2,2,2), 
         col = c('red', 'navy', '#7c83e8', rgb(153/255, 109/255, 236/255, alpha = 0.3)), 
         lty = c(2, 2, 3, 1))
  
  polygon(c(grid[grid >= upper_bd], upper_bd),
          c(relative_belief_ratio[grid >= upper_bd], 0),
          col = rgb(153/255, 109/255, 236/255, alpha = 0.3), border = NA)
  polygon(c(grid[grid <= lower_bd ], lower_bd),
          c(relative_belief_ratio[grid <= lower_bd ], 0),
          col = rgb(153/255, 109/255, 236/255, alpha = 0.3), border = NA)
}

RB_generate_dataframe = function(grid, prior, post, relative_belief_ratio){
  # This loop is to bring back the NA values to ensure users
  # do not misuse when downloaded.
  for(i in 1:length(prior)){
    if(is.na(post[i]/prior[i])){
      relative_belief_ratio[i] = NA
    }
  }
  RB_df = data.frame(grid, prior, post, relative_belief_ratio)
  colnames(RB_df) = c("Grid Point", "Prior", "Posterior", "Relative Belief")
  return(RB_df)
}





# ROUGH - PREVIOUS SET UPS

################################################################
# OUTPUT 2 PAGE                                                #
################################################################

#RB_setup_Strength_of_w0 = div(
#  titlePanel("Strength of w0"),
#  sidebarLayout(
#    sidebarPanel(
#      numericInput(inputId = "RB_setup_w0",
#                   tags$p('Hypothesis w = w0', style = "font-size: 90%;"),value = 0.6)
#    ),
#    mainPanel(
#      tabPanel("Strength of w0", verbatimTextOutput("RB_setup_values2"))
#    )
#  )
#)

################################################################
# GRAPH 2 PAGE                                                 #
################################################################

#RB_setup_relative_belief_plot_of_w0 = div( 
#  titlePanel("Plot of Relative Belief Ratio at w0"),
#  mainPanel(
#    tabPanel("Relative Belief Plot of w0", plotOutput(outputId = "RB_setup_w0_graph")),
#  )
#)


#distance_that_matters(0.01)

#test_function = function(delta){
#  newarray = c(delta/2)
#  print((1/delta)-1)
#  for(i in 1:((1/delta)-1)){
#    change = 1 + 2 * i
#    newarray = c(newarray, (change * delta)/2)
#  }
#  return(newarray)
#}
#test_function(0.01)

# TESTING
#alpha1w = 391.72
#alpha2w = 211.39
#n = 100 # total number
#nD = 68 # total number diseased
#nND = n - nD
#w0 = 0.6

#grid = RB_distance_that_matters(0.001)
#test_1 = RB_compute_values(alpha1w, alpha2w, n, nD, grid)
#test_2 = compute_credible_region(0.06, test_1$relative_belief_ratio, grid, test_1$sup_gamma, 
#                                 test_1$pr_interval)
#generate_prior_post_graph(test_1$prior, test_1$post, test_1$pr_interval, grid, credible_region = FALSE,
#                                           rb_line = FALSE)
#generate_prior_post_graph(test_1$prior, test_1$post, test_1$pr_interval, grid, test_2$credible_region,
#                          test_2$rb_line)

#y = RB_compute_values(alpha1w, alpha2w, n, nD)
#test = RB_generate_dataframe(y$prior, y$post, y$relative_belief_ratio)

#TODO ONCE ERRORS ARE FIXED

#section3.2_grid_length = 1000
#section3.2_grid = seq(0, 1, length= 1 + section3.2_grid_length)

#y = RB_compute_values(alpha1w, alpha2w, n, nD)
#z = w0_compute_values(alpha1w, alpha2w, n, nD, w0, y$relative_belief_ratio)
#generate_relative_belief_ratio_at_w0_graph(y$relative_belief_ratio, y$x_interval, y$y_interval, z$relative_belief_ratio_at_w0)

#par(mfrow = c(1, 2))
#generate_prior_post_graph(y$grid, y$prior, y$post)
#generate_rb_graph(y$grid, y$relative_belief_ratio, y$pr_interval)

