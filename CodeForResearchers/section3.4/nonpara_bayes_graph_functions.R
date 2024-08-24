
nonpara_bayes_prior_post_graph = function(delta, prior, post, 
                                          colour_choice = c("blue", "green"),
                                          lty_type = c(2, 2),
                                          transparency = 0.1,
                                          legend_position = "topleft"){
  # This generates the graph for the prior and the posterior of the AUC (binom val diag case.)
  # colour choice goes in the following order: prior, posterior, plausible region, 
  # and credible region.
  grid = open_bracket_grid(delta)
  x_interval = c(0, 1)
  # Constructs an interval for the y region
  y_interval = c(0, max(c(prior, post)))
  # Plots of the Prior and the Posterior
  plot(grid, prior, type='l', lty = lty_type[1], lwd = 2, xlim = x_interval, ylim = y_interval,
       main = "Graph of the Prior and Posterior of the AUC", ylab = "Densities", xlab = "AUC", 
       col = colour_choice[1])
  lines(grid, post, col = colour_choice[2], type = "l", lty = lty_type[2], lwd = 2)
  # Getting inner rgb colour for transparent effect
  rgb_prior = col2rgb(colour_choice[1])
  rgb_post = col2rgb(colour_choice[2])
  polygon(grid, post, col = rgb(rgb_post[1]/255, rgb_post[2]/255, rgb_post[3]/255, 
                                alpha = transparency), border = NA)
  polygon(grid, force_bounds_0(prior), 
          col = rgb(rgb_prior[1]/255, rgb_prior[2]/255, rgb_prior[3]/255, 
                    alpha = transparency), border = NA)
  legend(legend_position, legend = c("Prior", "Posterior"), lwd = 2, 
         col = c(colour_choice[1], colour_choice[2]), 
         lty = c(lty_type[1], lty_type[2]))
}

nonpara_bayes_rbr_graph = function(delta, relative_belief_ratio, 
                                   rb_line = FALSE, 
                                   colour_choice = c("red", "royalblue1", "#81ddff"),
                                   lty_type = c(2, 2, 3),
                                   transparency = 0.1,
                                   legend_position = "topleft"){
  # This generates the graph for the rbr of the AUC from the binormal valued diagnostic.
  # colour choice goes in the following order: relative belief ratio, plausible region,
  # line of y = 1, credible region.
  grid = open_bracket_grid(delta)
  x_interval = c(0, 1)
  # Temporarily set NaNs to 0 for graphing purposes
  relative_belief_ratio[is.na(relative_belief_ratio)] = 0
  # Constructs an interval for the x and y region
  y_interval = c(0, max(relative_belief_ratio))
  # Plotting the graph of the relative belief ratio
  plot(grid, relative_belief_ratio, type='l', lty = lty_type[1], lwd = 2, xlim = x_interval, 
       ylim = y_interval,
       main = "Graph of the Relative Belief Ratio of the AUC", 
       ylab = "RBR", xlab = "AUC", col = colour_choice[1])
  # For the Plausible Region
  abline(h = 1, col = colour_choice[2], lwd = 2, lty = lty_type[2])
  # Colouring in the area between the plausible region and when the RBR > 1
  rgb_rb = col2rgb(colour_choice[1])
  polygon(grid, relative_belief_ratio,
          col = rgb(rgb_rb[1]/255, rgb_rb[2]/255, rgb_rb[3]/255, alpha = transparency), 
          border = NA)
  if (typeof(rb_line) == "double") { 
    abline(h = rb_line, col = colour_choice[3], lwd = 2, lty = lty_type[3])
    legend(legend_position, legend = c("Relative Belief Ratio", "Credible Region Line", "RBR = 1"), 
           lwd = 2, 
           col = c(colour_choice[1], colour_choice[3], colour_choice[2]), 
           lty = c(lty_type[1], lty_type[3], lty_type[2]))
  } else {
    legend(legend_position, legend = c("Relative Belief Ratio", "RBR = 1"), lwd = 2, 
           col = c(colour_choice[1], colour_choice[2]), 
           lty = c(lty_type[1], lty_type[2]))
  }
}


# note: see if this needs to be modified or if it is the same as the binormal case.
nonpara_bayes_plots_AUC_copt = function(grid, # used gridcopt
                                        prior = FALSE, 
                                        post = FALSE,
                                        rbr = FALSE,
                                        rb_line = FALSE,
                                        lty_type = c(2, 1, 6, 2, 3),
                                        colour_choice = c("blue", "red", "green",
                                                          "royalblue1", "#81ddff"),
                                        transparency = 0.1,
                                        x_title = "cmod",
                                        legend_position = "topleft"){
  # x_title should either be cmod or copt
  # Colour choice order: prior, posterior, rbr, plausible region, rb line, credible region
  #grid = open_bracket_grid(delta)
  # TYPE 1: GRAPH OF PRIOR AND POSTERIOR
  if((typeof(prior) == "double") & (typeof(post) == "double")){
    
    title = paste("Plot of the Prior and the Posterior of", str_to_title(x_title), sep = " ")
    #Graph of posterior
    plot(grid, post, main = title, 
         xlab = x_title, ylab = "Prior and Posterior", type = "l", 
         lty = lty_type[2], lwd = 2, col = colour_choice[2])
    # Graph of prior 
    lines(grid, prior, type = "l", lty = lty_type[1], lwd = 2, col = colour_choice[1])
    # Fill & Transparency effect
    rgb_prior = col2rgb(colour_choice[1])
    rgb_post = col2rgb(colour_choice[2])
    polygon(grid, post, col = rgb(rgb_post[1]/255, rgb_post[2]/255, rgb_post[3]/255, 
                                             alpha = transparency), border = NA)
    polygon(grid, force_bounds_0(prior), 
            col = rgb(rgb_prior[1]/255, rgb_prior[2]/255, rgb_prior[3]/255, 
                      alpha = transparency), border = NA)
    # Checking if a credible region exists
    legend(legend_position, legend = c("Prior", "Posterior"), 
            col = c(colour_choice[1], colour_choice[2]), lwd = 2,
            lty = c(lty_type[1], lty_type[2]))
    # TYPE 2: GRAPH OF THE RELATIVE BELIEF RATIO
  } else if(typeof(rbr) == "double"){
    
    # Graph of the relative belief ratio
    title = paste("Plot of the Relative Belief Ratio of", str_to_title(x_title), sep = " ")
  
    plot(grid, rbr, main = title,
         xlab=x_title, ylab=expression("Relative Belief Ratio"), type="l", 
         lty = lty_type[3], lwd = 2, col = colour_choice[3])
    # Fill & Transparency effect
    rgb_rbr = col2rgb(colour_choice[3])
    # Colouring in the area between the plausible region and when the RBR > 1
    abline(h = 1, col = colour_choice[4], lwd = 2, lty = lty_type[4])
    rgb_rb = col2rgb(colour_choice[3])
    polygon(grid, rbr,
            col = rgb(rgb_rb[1]/255, rgb_rb[2]/255, rgb_rb[3]/255, alpha = transparency), 
            border = NA)
    # Checking if a credible region exists
    if (typeof(rb_line) == "double") { 
      abline(h = rb_line, col = colour_choice[5], lwd = 2, lty = lty_type[5])
      legend(legend_position, legend = c("Relative Belief Ratio", "Credible Region Line", "RBR = 1"), 
             lwd = 2, 
             col = c(colour_choice[3], colour_choice[5], colour_choice[4]), 
             lty = c(lty_type[3], lty_type[5], lty_type[4]))
    } else {
      legend(legend_position, legend = c("Relative Belief Ratio", "RBR = 1"), lwd = 2, 
             col = c(colour_choice[3], colour_choice[4]), 
             lty = c(lty_type[3], lty_type[4]))
    }
  }
}

