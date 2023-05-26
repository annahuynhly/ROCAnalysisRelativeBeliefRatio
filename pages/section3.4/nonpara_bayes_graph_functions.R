
nonpara_bayes_prior_post_graph = function(delta, prior, post, 
                                          plausible_region,
                                          credible_region = FALSE,
                                          colour_choice = c("blue", "green", "#b3bfff", "#81ddff"),
                                          lty_type = c(2, 2, 3, 3),
                                          transparency = 0.1){
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
  abline(v = plausible_region[1], col = colour_choice[3], lwd = 2, lty = lty_type[3])
  abline(v = plausible_region[2], col = colour_choice[3], lwd = 2, lty = lty_type[3])
  # Getting inner rgb colour for transparent effect
  rgb_prior = col2rgb(colour_choice[1])
  rgb_post = col2rgb(colour_choice[2])
  polygon(grid, post, col = rgb(rgb_post[1]/255, rgb_post[2]/255, rgb_post[3]/255, 
                                alpha = transparency), border = NA)
  polygon(grid, force_bounds_0(prior), 
          col = rgb(rgb_prior[1]/255, rgb_prior[2]/255, rgb_prior[3]/255, 
                    alpha = transparency), border = NA)
  #original colours: green rgb(146/255, 255/255, 133/255, alpha = 0.3) 
  # blue rgb(133/255, 198/255, 255/255, alpha = 0.3)
  if (typeof(credible_region) == "double") { # need both to run properly
    abline(v = credible_region[1], col = colour_choice[4], lwd = 2, lty = lty_type[4]) 
    abline(v = credible_region[2], col = colour_choice[4], lwd = 2, lty = lty_type[4])
    legend("topleft", legend = c("Prior", "Posterior", "Plausible Region", "Credible Region"), 
           lwd = 2, 
           col = c(colour_choice[1], colour_choice[2], colour_choice[3], colour_choice[4]), 
           lty = c(lty_type[1], lty_type[2], lty_type[3], lty_type[4]))
  } else {
    legend("topleft", legend = c("Prior", "Posterior", "Plausible Region"), lwd = 2, 
           col = c(colour_choice[1], colour_choice[2], colour_choice[3]), 
           lty = c(lty_type[1], lty_type[2], lty_type[3]))
  }
}

nonpara_bayes_rbr_graph = function(delta, relative_belief_ratio, 
                                   plausible_region,
                                   credible_region = FALSE,
                                   rb_line = FALSE, 
                                   colour_choice = c("red", "#b3bfff", "royalblue1", "#81ddff"),
                                   lty_type = c(2, 3, 2, 3),
                                   transparency = 0.1){
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
  abline(h = 1, col = colour_choice[3], lwd = 2, lty = lty_type[3])
  abline(v = plausible_region[1], col = colour_choice[2], lwd = 2, lty = lty_type[2])
  abline(v = plausible_region[2], col = colour_choice[2], lwd = 2, lty = lty_type[2])
  # Colouring in the area between the plausible region and when the RBR > 1
  l = min(which(grid >= plausible_region[1]))
  h = max(which(grid < plausible_region[2]))
  rgb_rb = col2rgb(colour_choice[1])
  polygon(c(grid[c(l, l:h, h)]),
          c(1, relative_belief_ratio[l:h], 1),
          col = rgb(rgb_rb[1]/255, rgb_rb[2]/255, rgb_rb[3]/255, alpha = transparency), 
          border = NA)
  if ((typeof(credible_region) == "double") & (typeof(rb_line) == "double")) { 
    abline(v = credible_region[1], col = colour_choice[4], lwd = 2, lty = lty_type[4]) 
    abline(v = credible_region[2], col = colour_choice[4], lwd = 2, lty = lty_type[4])
    abline(h = rb_line, col = colour_choice[4], lwd = 2, lty = lty_type[4])
    rgb_cr = col2rgb(colour_choice[4])
    legend("topleft", legend = c("Relative Belief Ratio", "Plausible Region", 
                                 "Credible Region", "Gamma (Area)"), 
           lwd = 2, 
           col = c(colour_choice[1], colour_choice[2], colour_choice[4], 
                   rgb(rgb_cr[1]/255, rgb_cr[2]/255, rgb_cr[3]/255, alpha = transparency)), 
           lty = c(lty_type[1], lty_type[2], lty_type[4], 1))
    polygon(x = c(credible_region[1], credible_region[1], credible_region[2], credible_region[2]), 
            y = c(0, rb_line, rb_line, 0), 
            col = rgb(rgb_cr[1]/255, rgb_cr[2]/255, rgb_cr[3]/255, alpha = transparency), 
            border = NA)   
  } else {
    legend("topleft", legend = c("Relative Belief Ratio", "Plausible Region"), lwd = 2, 
           col = c(colour_choice[1], colour_choice[2]), lty = c(lty_type[1], lty_type[2]))
  }
}


# LAST THING I WAS TYRING TO GET DONE:
# realised there isn't an RBcmod the same way as the rest
# ->NEED TO CHANGE THE NAMES OF THE TITLE, not necesasrily using mod!!!!!!!!

# note: see if this needs to be modified or if it is the same as the binormal case.
nonpara_bayes_plots_AUC_copt = function(grid, # used gridcopt
                                        prior = FALSE, 
                                        post = FALSE,
                                        rbr = FALSE,
                                        plausible_region,
                                        credible_region = FALSE,
                                        rb_line = FALSE,
                                        lty_type = c(2, 1, 6, 3, 2, 3),
                                        colour_choice = c("blue", "red", "green",
                                                          "#b3bfff", "royalblue1", "#81ddff"),
                                        transparency = 0.1,
                                        x_title = "cmod"){
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
    # The plausible region
    abline(v = plausible_region[1], col = colour_choice[4], lwd = 2, lty = lty_type[4])
    abline(v = plausible_region[2], col = colour_choice[4], lwd = 2, lty = lty_type[4])
    # Fill & Transparency effect
    rgb_prior = col2rgb(colour_choice[1])
    rgb_post = col2rgb(colour_choice[2])
    polygon(grid, post, col = rgb(rgb_post[1]/255, rgb_post[2]/255, rgb_post[3]/255, 
                                             alpha = transparency), border = NA)
    polygon(grid, force_bounds_0(prior), 
            col = rgb(rgb_prior[1]/255, rgb_prior[2]/255, rgb_prior[3]/255, 
                      alpha = transparency), border = NA)
    # Checking if a credible region exists
    if (typeof(credible_region) == "double") { # need both to run properly
      abline(v = credible_region[1], col = colour_choice[6], lwd = 2, lty = lty_type[6])
      abline(v = credible_region[2], col = colour_choice[6], lwd = 2, lty = lty_type[6])
      legend("topleft", legend = c("Prior", "Posterior", "Plausible Region", "Credible Region"), lwd = 2, 
             col = c(colour_choice[1], colour_choice[2], colour_choice[4], colour_choice[6]), 
             lty = c(lty_type[1], lty_type[2], lty_type[4], lty_type[6]))
    } else {
      legend("topleft", legend = c("Prior", "Posterior", "Plausible Region"), 
             col = c(colour_choice[1], colour_choice[2], colour_choice[4]), lwd = 2,
             lty = c(lty_type[1], lty_type[2], lty_type[4]))
    }
    # TYPE 2: GRAPH OF THE RELATIVE BELIEF RATIO
  } else if(typeof(rbr) == "double"){
    
    # Graph of the relative belief ratio
    title = paste("Plot of the Relative Belief Ratio of", str_to_title(x_title), sep = " ")
    
    plot(grid, rbr, main = title,
         xlab="cmod", ylab=expression("Relative Belief Ratio"), type="l", 
         lty = lty_type[3], lwd = 2, col = colour_choice[3])
    # The plausible region
    abline(v = plausible_region[1], col = colour_choice[4], lwd = 2, lty = lty_type[4])
    abline(v = plausible_region[2], col = colour_choice[4], lwd = 2, lty = lty_type[4])
    # Fill & Transparency effect
    rgb_rbr = col2rgb(colour_choice[3])
    # Colouring in the area between the plausible region and when the RBR > 1
    abline(h = 1, col = colour_choice[4], lwd = 2, lty = lty_type[4])
    l = min(which(grid >= plausible_region[1]))
    h = max(which(grid < plausible_region[2]))
    rgb_rb = col2rgb(colour_choice[3])
    polygon(c(grid[c(l, l:h, h)]),
            c(1, rbr[l:h], 1),
            col = rgb(rgb_rb[1]/255, rgb_rb[2]/255, rgb_rb[3]/255, alpha = transparency), 
            border = NA)
    # Checking if a credible region exists
    if ((typeof(credible_region) == "double") & (typeof(rb_line) == "double")) { 
      abline(v = credible_region[1], col = colour_choice[6], lwd = 2, lty = lty_type[6]) 
      abline(v = credible_region[2], col = colour_choice[6], lwd = 2, lty = lty_type[6])
      abline(h = rb_line, col = colour_choice[6], lwd = 2, lty = lty_type[5])
      rgb_cr = col2rgb(colour_choice[6])
      legend("bottomleft", legend = c("Relative Belief Ratio", "Plausible Region", 
                                      "Credible Region", "Gamma (Area)"), 
             lwd = 2, 
             col = c(colour_choice[3], colour_choice[4], colour_choice[6], 
                     rgb(rgb_cr[1]/255, rgb_cr[2]/255, rgb_cr[3]/255, alpha = transparency)), 
             lty = c(lty_type[3], lty_type[4], lty_type[6], 1))
      polygon(x = c(credible_region[1], credible_region[1], credible_region[2], credible_region[2]), 
              y = c(0, rb_line, rb_line, 0), 
              col = rgb(rgb_cr[1]/255, rgb_cr[2]/255, rgb_cr[3]/255, alpha = transparency), 
              border = NA)   
    } else {
      legend("topleft", legend = c("Relative Belief Ratio", "Plausible Region"), lwd = 2, 
             col = c(colour_choice[3], colour_choice[4]), lty = c(lty_type[3], lty_type[4]))
    }
  }
}





