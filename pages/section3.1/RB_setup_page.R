
################################################################
# DESCRIPTION                                                  #
################################################################

RB_setup_description = div( # CHANGE THIS
  titlePanel("Page Description"),
  p("Need to work on this section. Come back later!"),
  p("The goal of this section is for the user to compute the prior, posterior, the relative belief 
    ratio, and the strength."),
  hr(style = "border-top: 1px solid #363e4f;"),
  h4("Outputs and their Meanings"),
  #p(HTML("<b>Outputs and their Meanings</b>")),
  p(HTML("<ul>
            <li><b>pr_interval:</b> plausible region. PR = { w : RB(w | ... ) > 1 }</li>
            <li><b>max_w:</b> the chosen value for w (when RB(w | ... ) is maximized. 
            It is actually just n/nD.)</li>
            <li><b>relative_belief_w0:</b> directly calculated from RB(w0 | ... )</li>
            <li><b>w0_interval:</b> the region once RB(w | ... ) > RB(w0 | ... )</li>
            <li><b>strength:</b> given as π({w : RB(w | ... ) <= RB(w0 | ... )| ... }) 
            It is highlighted in the second graph.</li>
            </ul>")),
  #p(HTML("<b>pr_interval:</b> plausible region. PR = { w : RB(w | ... ) > 1 }")),
  #p(HTML("<b>max_w:</b> the chosen value for w (when RB(w | ... ) is maximized. 
  #       It is actually just n/nD.)")),
  #p(HTML("<b>relative_belief_w0:</b> directly calculated from RB(w0 | ... )")),
  #p(HTML("<b>w0_interval:</b> the region once RB(w | ... ) > RB(w0 | ... )")),
  #p(HTML("<b>strength:</b> given as π({w : RB(w | ... ) <= RB(w0 | ... )| ... }) 
  #       It is highlighted in the second graph.")),
)

################################################################
# PAGE LOGIC                                                   #
################################################################

page_RB_setup = div( # CHANGE THIS
  titlePanel("Section 3.4: RB_setup"), # CHANGE THIS
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Slider for the number of bins ----
      numericInput(inputId = "RB_setup_alpha1w", # CHANGE THIS
                   tags$p('alpha1w', style = "font-size: 90%;"),value = 391.72),
      numericInput(inputId = "RB_setup_alpha2w", # CHANGE THIS
                   tags$p('alpha2w', style = "font-size: 90%;"),value = 211.39),
      numericInput(inputId = "RB_setup_n", # CHANGE THIS
                   tags$p('Total Sample Size', style = "font-size: 90%;"),value = 100, min = 1),
      numericInput(inputId = "RB_setup_nD", # CHANGE THIS
                   tags$p('Total Diseased', style = "font-size: 90%;"),value = 68, min = 0),
      numericInput(inputId = "RB_setup_w0", # CHANGE THIS
                   tags$p('Hypothesized w', style = "font-size: 90%;"),value = 0.6),
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # OUTPUTTING THE VALUES
      tabsetPanel(type = "tabs",
                  tabPanel("Description", RB_setup_description), # CHANGE THIS
                  tabPanel("Plausible Region & Max w", verbatimTextOutput("RB_setup_values1")),
                  tabPanel("Plots", # PLACEHOLDER FLUIDROW EXAMPLE
                           fluidRow(splitLayout(cellWidths = c("50%", "50%"), 
                                    plotOutput("RB_setup_postprior_graph"), 
                                    plotOutput("RB_setup_RB_graph")))),
                  tabPanel("Strength of w0", verbatimTextOutput("RB_setup_values2")),
                  tabPanel("Relative Belief Plot of w0", plotOutput(outputId = "RB_setup_w0_graph")),
      )
    )
  )
)
################################################################
# MAIN FUNCTIONS                                               #
################################################################

#TODO ONCE ERRORS ARE FIXED

section3.2_grid_length = 1000
section3.2_grid = seq(0, 1, length= 1 + section3.2_grid_length)

# TESTING
#alpha1w = 391.72
#alpha2w = 211.39
#n = 100 # total number
#nD = 68 # total number diseased

#w0 = 0.6

#y = RB_compute_values(alpha1w, alpha2w, n, nD)
#z = w0_compute_values(alpha1w, alpha2w, n, nD, w0, y$relative_belief)
#generate_w0_graph(y$relative_belief, y$x_interval, y$y_interval, z$relative_belief_w0)


# functions

RB_compute_values = function(alpha1w, alpha2w, n, nD){
  nND = n - nD # obtaining number not diseased
  
  prior = dbeta(section3.2_grid, alpha1w, alpha2w)
  post = dbeta(section3.2_grid, alpha1w + nD, alpha2w + nND) # posterior
  
  relative_belief = post/prior
  
  # force NaN to be 0 for easier computations
  relative_belief[is.na(relative_belief)] = 0
  
  # want to output a plausible region
  plausible_region = c()
  for (i in 1:length(section3.2_grid)){
    if (relative_belief[[i]] > 1){
      plausible_region = c(plausible_region, as.numeric(section3.2_grid[i]))
    }
  }
  
  # find maximum w
  max_w = (match(max(relative_belief), relative_belief) - 1)/section3.2_grid_length
  
  # note: the following interval assumes there are no breaks (for now):
  pr_interval = c(plausible_region[1], plausible_region[length(plausible_region)])
  
  newlist = list("nND" = nND, "prior" = prior, "post" = post, 
                 "relative_belief" = relative_belief, "pr_interval" = pr_interval,
                 "max_w" = max_w)
  return(newlist)
}

generate_prior_post_graph = function(prior, post){
  # for graphs: determining the x-axis
  x_region = c()
  for (i in 1:length(section3.2_grid)){
    if ((prior[[i]] > 0.1) | (post[[i]] > 0.1)){
      x_region = c(x_region, as.numeric(section3.2_grid[i]))
    }
  }
  # note: the following interval assumes there are breaks (for now):
  x_interval = c(x_region[1], x_region[length(x_region)])
  y_interval = c(0, max(c(prior, post)))
  
  plot(section3.2_grid, prior, type='l', lty = 2, lwd = 2, xlim = x_interval, ylim = y_interval,
       main = "Graph of the Prior and Posterior", ylab = "Values", xlab = "w", col = "blue")
  lines(section3.2_grid,post,col="green", type = "l", lty = 2, lwd = 2)
  legend("topleft", legend = c("Prior", "Posterior"), lwd = 2, col = c('blue', 'green'), 
         lty = c(2, 2))
  polygon(section3.2_grid, post, col = rgb(146/255, 255/255, 133/255, alpha = 0.3), border = NA)
  polygon(section3.2_grid, prior, col = rgb(133/255, 198/255, 255/255, alpha = 0.3), border = NA)
}

generate_rb_graph = function(relative_belief, pr_interval){
  x_region = c()
  for (i in 1:length(section3.2_grid)){
    if (relative_belief[[i]] > 0.05){
      x_region = c(x_region, as.numeric(section3.2_grid[i]))
    }
  }
  # note: the following interval assumes there are no breaks (for now):
  x_interval = c(x_region[1], x_region[length(x_region)])
  y_interval = c(0, max(relative_belief))
  lower_bd = pr_interval[1]
  upper_bd = pr_interval[length(pr_interval)]
  
  plot(section3.2_grid, relative_belief, type='l', lty = 2, lwd = 2, xlim = x_interval, ylim = y_interval,
       main = "Graph of the Relative Belief", ylab = "Values", xlab = "w", col = "red")
  abline(h=1, col="royalblue1", lwd = 2, lty = 2)
  abline(v=lower_bd, col="#b3bfff", lwd = 2, lty = 3)
  abline(v=upper_bd, col="#b3bfff", lwd = 2, lty = 3)
  legend("topleft", legend = c("Relative Belief", "Plausible Region"), lwd = 2, col = c('red', '#b3bfff'), 
         lty = c(2, 3))

  # Lower and higher indices on the X-axis
  l <- min(which(section3.2_grid >= pr_interval[1]))
  h <- max(which(section3.2_grid < pr_interval[length(pr_interval)]))
  polygon(c(section3.2_grid[c(l, l:h, h)]),
          c(1, relative_belief[l:h], 1),
          col = rgb(197/255, 132/255, 255/255, alpha = 0.3), border = NA)
}

#par(mfrow = c(1, 2))
#generate_prior_post_graph(y$section3.2_grid, y$prior, y$post)
#generate_rb_graph(y$section3.2_grid, y$relative_belief, y$pr_interval)

# for hypothesis testing and computing strength

w0_compute_values = function(alpha1w, alpha2w, n, nD, w0, relative_belief){
  
  nND = n - nD
  prior_w0 = dbeta(w0, alpha1w, alpha2w)
  post_w0 = dbeta(w0, alpha1w + nD, alpha2w + nND)
  relative_belief_w0 = post_w0/prior_w0
  
  RB_integrand = function(w){dbeta(w, alpha1w + nD, alpha2w + nND)/dbeta(w, alpha1w, alpha2w)}
  
  # find an interval where the prior isn't 0 - to properly compute integrand 
  valid_interval = c()
  for (i in 1:length(section3.2_grid)){
    if (dbeta(section3.2_grid[i], alpha1w, alpha2w) != 0){
      valid_interval = c(valid_interval, as.numeric(section3.2_grid[i]))
    }
  }
  
  # locating the intersections when R(w | .) > R(w0 | .)
  nonstrength_interval = c()
  for (i in 1:length(section3.2_grid)){
    if ((relative_belief[[i]] > relative_belief_w0) | (relative_belief[[i]] == relative_belief_w0)){
      nonstrength_interval = c(nonstrength_interval, as.numeric(section3.2_grid[i]))
    }
  }
  
  if (is.null(nonstrength_interval)){ # this case would mean that R(w | .) < R(w0 | .) always!
    strength = 0
  } else {
    interval = c(nonstrength_interval[1], nonstrength_interval[length(nonstrength_interval)])
    # whole area of RB(w | .)
    x = integrate(RB_integrand, lower = valid_interval[1], upper = valid_interval[length(valid_interval)])
    #subtract region that contains where RB(w0 | .) > RB(w | .)
    y = integrate(RB_integrand, lower = interval[1], upper = interval[length(interval)])
    #need to add absolute error onto them
    strength = (x$value + x$abs.error) - (y$value + y$abs.error)
  }
  
  newlist = list("relative_belief_w0" = relative_belief_w0, "w0_interval" = interval, 
                 "strength" = strength)
  return(newlist)
}

generate_w0_graph = function(relative_belief, relative_belief_w0, w0_interval){
  x_region = c()
  for (i in 1:length(section3.2_grid)){
    if (relative_belief[[i]] > 0.05){
      x_region = c(x_region, as.numeric(section3.2_grid[i]))
    }
  }
  # note: the following interval assumes there are breaks (for now):
  x_interval = c(x_region[1], x_region[length(x_region)])
  y_interval = c(0, max(relative_belief))
  lower_bd = w0_interval[1]
  upper_bd = w0_interval[length(w0_interval)]
  
  plot(section3.2_grid, relative_belief, type='l', lty = 2, lwd = 2, xlim = x_interval, 
       ylim = y_interval, main = "Graph of the Relative Belief", ylab = "Values", xlab = "w", col = "red")
  abline(h=relative_belief_w0, lwd = 2, lty = 2, col="navy")
  abline(v=lower_bd, col="#7c83e8", lwd = 2, lty = 3)
  abline(v=upper_bd, col="#7c83e8", lwd = 2, lty = 3)
  legend("topleft", legend = c("Relative Belief", "Relative Belief of w0", "Region"), lwd = 2, 
         col = c('red', 'navy', '#7c83e8'), lty = c(2, 2, 3))
  
  polygon(c(section3.2_grid[section3.2_grid >= upper_bd], upper_bd),
          c(relative_belief[section3.2_grid >= upper_bd], 0),
          col = rgb(153/255, 109/255, 236/255, alpha = 0.3), border = NA)
  polygon(c(section3.2_grid[section3.2_grid <= lower_bd ], lower_bd),
          c(relative_belief[section3.2_grid <= lower_bd ], 0),
          col = rgb(153/255, 109/255, 236/255, alpha = 0.3), border = NA)
}











