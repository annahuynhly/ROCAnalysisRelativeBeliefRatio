################################################################
# FUNCTIONS FOR GRAPHS                                         #
################################################################

binormal_diag_prior_post_graph = function(condition,
                                          delta, prior, post, 
                                          plausible_region,
                                          credible_region = FALSE,
                                          colour_choice = c("blue", "green", "#b3bfff", "#81ddff"),
                                          transparency = 0.1){
  # This generates the graph for the prior and the posterior of the AUC (binom val diag case.)
  # colour choice goes in the following order: prior, posterior, plausible region, 
  # and credible region.
  if(condition != "conditional" && condition != "unconditional"){
    return("condition must either be 'conditional' or 'unconditional'.")
  } 
  if (condition == "unconditional"){ # The intervals change depending on the case.
    grid = open_bracket_grid(delta)
    x_interval = c(0, 1)
  } else if (condition == "conditional"){
    L = 1/delta/2
    grid = open_bracket_grid(delta)[(L+1):(2*L)]
    x_interval = c(0.5, 1)
  }
  # Constructs an interval for the y region
  y_interval = c(0, max(c(prior, post)))
  # Plots of the Prior and the Posterior
  plot(grid, prior, type='l', lty = 2, lwd = 2, xlim = x_interval, ylim = y_interval,
       main = "Graph of the Prior and Posterior of the AUC", ylab = "Densities", xlab = "w", 
       col = colour_choice[1])
  lines(grid, post, col=colour_choice[2], type = "l", lty = 2, lwd = 2)
  abline(v=plausible_region[1], col=colour_choice[3], lwd = 2, lty = 3)
  abline(v=plausible_region[2], col=colour_choice[3], lwd = 2, lty = 3)
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
    abline(v = credible_region[1], col = colour_choice[4], lwd = 2, lty = 3) # lty: to change?
    abline(v = credible_region[2], col = colour_choice[4], lwd = 2, lty = 3)
    legend("topleft", legend = c("Prior", "Posterior", "Plausible Region", "Credible Region"), lwd = 2, 
           col = c(colour_choice[1], colour_choice[2], colour_choice[3], colour_choice[4]), 
           lty = c(2, 2, 3, 3))
  } else {
    legend("topleft", legend = c("Prior", "Posterior", "Plausible Region"), lwd = 2, 
           col = c(colour_choice[1], colour_choice[2], colour_choice[3]), 
           lty = c(2, 2, 3))
  }
}

binormal_diag_rbr_graph = function(condition, 
                                   delta, relative_belief_ratio, 
                                   plausible_region,
                                   credible_region = FALSE,
                                   rb_line = FALSE, 
                                   colour_choice = c("red", "#b3bfff", "royalblue1", "#81ddff"), 
                                   transparency = 0.1){
  # This generates the graph for the rbr of the AUC from the binormal valued diagnostic.
  # colour choice goes in the following order: relative belief ratio, plausible region,
  # line of y = 1, credible region.
  if(condition != "conditional" && condition != "unconditional"){
    return("condition must either be 'conditional' or 'unconditional'.")
  } 
  if (condition == "unconditional"){
    grid = open_bracket_grid(delta)
    x_interval = c(0, 1)
  } else if (condition == "conditional"){
    L = 1/delta/2
    grid = open_bracket_grid(delta)[(L+1):(2*L)]
    x_interval = c(0.5, 1)
  }
  # Temporarily set NaNs to 0 for graphing purposes
  relative_belief_ratio[is.na(relative_belief_ratio)] = 0
  # Constructs an interval for the x and y region
  y_interval = c(0, max(relative_belief_ratio))
  # Plotting the graph of the relative belief ratio
  plot(grid, relative_belief_ratio, type='l', lty = 2, lwd = 2, xlim = x_interval, 
       ylim = y_interval,
       main = "Graph of the Relative Belief Ratio of the AUC", 
       ylab = "RBR", xlab = "w", col = colour_choice[1])
  # For the Plausible Region
  abline(h = 1, col = colour_choice[3], lwd = 2, lty = 2)
  abline(v = plausible_region[1], col = colour_choice[2], lwd = 2, lty = 3)
  abline(v = plausible_region[2], col = colour_choice[2], lwd = 2, lty = 3)
  # Colouring in the area between the plausible region and when the RBR > 1
  l = min(which(grid >= plausible_region[1]))
  h = max(which(grid < plausible_region[2]))
  rgb_rb = col2rgb(colour_choice[1])
  polygon(c(grid[c(l, l:h, h)]),
          c(1, relative_belief_ratio[l:h], 1),
          col = rgb(rgb_rb[1]/255, rgb_rb[2]/255, rgb_rb[3]/255, alpha = transparency), 
          border = NA)
  if ((typeof(credible_region) == "double") & (typeof(rb_line) == "double")) { 
    abline(v = credible_region[1], col = colour_choice[4], lwd = 2, lty = 3) 
    abline(v = credible_region[2], col = colour_choice[4], lwd = 2, lty = 3)
    abline(h = rb_line, col = colour_choice[4], lwd = 2, lty = 2)
    rgb_cr = col2rgb(colour_choice[4])
    legend("topleft", legend = c("Relative Belief Ratio", "Plausible Region", 
                                 "Credible Region", "Gamma (Area)"), 
           lwd = 2, 
           col = c(colour_choice[1], colour_choice[2], colour_choice[4], 
                   rgb(rgb_cr[1]/255, rgb_cr[2]/255, rgb_cr[3]/255, alpha = transparency)), 
           lty = c(2, 3, 3, 1))
    polygon(x = c(credible_region[1], credible_region[1], credible_region[2], credible_region[2]), 
            y = c(0, rb_line, rb_line, 0), 
            col = rgb(rgb_cr[1]/255, rgb_cr[2]/255, rgb_cr[3]/255, alpha = transparency), 
            border = NA)   
  } else {
    legend("topleft", legend = c("Relative Belief Ratio", "Plausible Region"), lwd = 2, 
           col = c(colour_choice[1], colour_choice[2]), lty = c(2, 3))
  }
}

binormal_diag_plots_AUC_copt = function(delta, priorcmoddensity = FALSE, 
                                        postcmoddensity = FALSE,
                                        RBcmod = FALSE,
                                        plausible_region,
                                        credible_region = FALSE,
                                        rb_line = FALSE,
                                        prior_lty = 2, post_lty = 1, rbr_lty = 6,
                                        colour_choice = c("blue", "red", "green",
                                                          "#b3bfff", "royalblue1", "#81ddff"),
                                        transparency = 0.1){
  # Colour choice order: prior, posterior, rbr, plausible region, rb line, credible region
  grid = open_bracket_grid(delta)
  # TYPE 1: GRAPH OF PRIOR AND POSTERIOR
  if((typeof(priorcmoddensity) == "double") & (typeof(postcmoddensity) == "double")){
    #Graph of posterior
    plot(grid, postcmoddensity, main = "Plot of the Prior and the Posterior of Cmod", 
         xlab = "cmod", ylab = "Prior and Posterior", type = "l", 
         lty = post_lty, lwd = 2, col = colour_choice[2])
    # Graph of prior 
    lines(grid, priorcmoddensity, type = "l", lty = prior_lty, lwd = 2, col = colour_choice[1])
    # The plausible region
    abline(v = plausible_region[1], col = colour_choice[4], lwd = 2, lty = 3)
    abline(v = plausible_region[2], col = colour_choice[4], lwd = 2, lty = 3)
    # Fill & Transparency effect
    rgb_prior = col2rgb(colour_choice[1])
    rgb_post = col2rgb(colour_choice[2])
    polygon(grid, postcmoddensity, col = rgb(rgb_post[1]/255, rgb_post[2]/255, rgb_post[3]/255, 
                                  alpha = transparency), border = NA)
    polygon(grid, force_bounds_0(priorcmoddensity), 
            col = rgb(rgb_prior[1]/255, rgb_prior[2]/255, rgb_prior[3]/255, 
            alpha = transparency), border = NA)
    # Checking if a credible region exists
    if (typeof(credible_region) == "double") { # need both to run properly
      abline(v = credible_region[1], col = colour_choice[6], lwd = 2, lty = 3)
      abline(v = credible_region[2], col = colour_choice[6], lwd = 2, lty = 3)
      legend("topleft", legend = c("Prior", "Posterior", "Plausible Region", "Credible Region"), lwd = 2, 
             col = c(colour_choice[1], colour_choice[2], colour_choice[4], colour_choice[6]), 
             lty = c(prior_lty, post_lty, 3, 3))
    } else {
      legend("topleft", legend = c("Prior", "Posterior", "Plausible Region"), 
             col = c(colour_choice[1], colour_choice[2], colour_choice[4]), lwd = 2,
             lty = c(prior_lty, post_lty, 3))
    }
    # TYPE 2: GRAPH OF THE RELATIVE BELIEF RATIO
  } else if(typeof(RBcmod) == "double"){
    # Graph of the relative belief ratio
    plot(grid, RBcmod, main = "Plot of the Relative Belief Ratio of Cmod",
         xlab="cmod", ylab=expression("Relative Belief Ratio"), type="l", 
         lty = rbr_lty, lwd = 2, col = colour_choice[3])
    # The plausible region
    abline(v = plausible_region[1], col = colour_choice[4], lwd = 2, lty = 3)
    abline(v = plausible_region[2], col = colour_choice[4], lwd = 2, lty = 3)
    # Fill & Transparency effect
    rgb_rbr = col2rgb(colour_choice[3])
    # Colouring in the area between the plausible region and when the RBR > 1
    abline(h = 1, col = colour_choice[4], lwd = 2, lty = 2)
    l = min(which(grid >= plausible_region[1]))
    h = max(which(grid < plausible_region[2]))
    rgb_rb = col2rgb(colour_choice[3])
    polygon(c(grid[c(l, l:h, h)]),
            c(1, RBcmod[l:h], 1),
            col = rgb(rgb_rb[1]/255, rgb_rb[2]/255, rgb_rb[3]/255, alpha = transparency), 
            border = NA)
    # Checking if a credible region exists
    if ((typeof(credible_region) == "double") & (typeof(rb_line) == "double")) { 
      abline(v = credible_region[1], col = colour_choice[6], lwd = 2, lty = 3) 
      abline(v = credible_region[2], col = colour_choice[6], lwd = 2, lty = 3)
      abline(h = rb_line, col = colour_choice[6], lwd = 2, lty = 2)
      rgb_cr = col2rgb(colour_choice[6])
      legend("bottomleft", legend = c("Relative Belief Ratio", "Plausible Region", 
                                      "Credible Region", "Gamma (Area)"), 
             lwd = 2, 
             col = c(colour_choice[3], colour_choice[4], colour_choice[6], 
                     rgb(rgb_cr[1]/255, rgb_cr[2]/255, rgb_cr[3]/255, alpha = transparency)), 
             lty = c(rbr_lty, 3, 3, 1))
      polygon(x = c(credible_region[1], credible_region[1], credible_region[2], credible_region[2]), 
              y = c(0, rb_line, rb_line, 0), 
              col = rgb(rgb_cr[1]/255, rgb_cr[2]/255, rgb_cr[3]/255, alpha = transparency), 
              border = NA)   
    } else {
      legend("topleft", legend = c("Relative Belief Ratio", "Plausible Region"), lwd = 2, 
             col = c(colour_choice[3], colour_choice[4]), lty = c(rbr_lty, 3))
    }
  }
}

binormal_diag_err_char_plots = function(delta, 
                                        prior_vals = FALSE, post_vals = FALSE, rbr_vals = FALSE,
                                        err_type, 
                                        prior_lty = 2, post_lty = 1, rbr_lty = 6,
                                        colour_choice = c("blue", "red", "green"),
                                        transparency = 0.1){
  # err_type can either be FNR, FPR, Error, FDR, FNDR
  grid = open_bracket_grid(delta)
  if((typeof(prior_vals) == "double") & (typeof(post_vals) == "double")){
    # Graph of the posterior
    plot(grid, post_vals, main = paste("Plot of the Prior and the Posterior of ", err_type, sep = ""),
         xlab = err_type, ylab = "Prior and Posterior", type = "l", lty = post_lty, lwd = 2,
         col = colour_choice[2])
    # Graph of the prior
    lines(grid, prior_vals, xlab = err_type, type = "l", lty = prior_lty, lwd = 2,
          col = colour_choice[1])
    # Fill & Transparent effect
    rgb_prior = col2rgb(colour_choice[1])
    rgb_post = col2rgb(colour_choice[2])
    polygon(grid, post_vals,
            col = rgb(rgb_post[1]/255, rgb_post[2]/255, rgb_post[3]/255, 
            alpha = transparency), border = NA)
    polygon(grid, force_bounds_0(prior_vals),
            col = rgb(rgb_prior[1]/255, rgb_prior[2]/255, rgb_prior[3]/255, 
            alpha = transparency), border = NA)
    legend("topleft", legend = c("Prior", "Posterior"), 
           col = c(colour_choice[1], colour_choice[2]), lwd = 2,
           lty = c(prior_lty, post_lty))
  } else if(typeof(rbr_vals) == "double"){
    # Graph of the relative belief ratio
    plot(grid, rbr_vals, main = paste("Plot of the Relative Belief Ratio of ", err_type, sep = ""),
         xlab = err_type, ylab = "Relative Belief Ratio", type = "l", lty = rbr_lty, lwd = 2, 
         col = colour_choice[3])
    # Fill & Transparent effect
    rgb_rbr = col2rgb(colour_choice[3])
    polygon(grid, force_bounds_0(rbr_vals),
            col = rgb(rgb_rbr[1]/255, rgb_rbr[2]/255, rgb_rbr[3]/255, 
            alpha = transparency), border = NA)
  }
}




