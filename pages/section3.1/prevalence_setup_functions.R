################################################################
# COMPUTATIONS                                                 #
################################################################

RBR_compute_values = function(alpha1w, alpha2w, n, nD, grid){
  # This computes the prior, posterior, and the relative belief ratio of w.
  nND = n - nD # obtaining number not diseased
  
  prior = dbeta(grid, alpha1w, alpha2w)
  post = dbeta(grid, alpha1w + nD, alpha2w + nND) # post is short for posterior
  relative_belief_ratio = post/prior
  
  # Force NA to be 0 for easier computations
  relative_belief_ratio[is.na(relative_belief_ratio)] = 0
  
  # Outputs a plausible region
  # WARNING: the following interval assumes there are no breaks, so it wouldn't work if there's a "peak".
  plausible_region = c()
  for (i in 1:length(grid)){
    if (relative_belief_ratio[[i]] > 1){
      plausible_region = c(plausible_region, as.numeric(grid[i]))
    }
  }
  # Finding maximum w based on the grid points
  RB_estimate_of_prevalence_w = (match(max(relative_belief_ratio), relative_belief_ratio))/length(grid)
  
  # Shortens the plausible region
  plausible_region = c(plausible_region[1], plausible_region[length(plausible_region)])
  
  # finding the prior and posterior content
  prior_content = pbeta(plausible_region[2], alpha1w, alpha2w) - pbeta(plausible_region[1], alpha1w, alpha2w)
  posterior_content = pbeta(plausible_region[2], alpha1w + nD, alpha2w + nND) - pbeta(plausible_region[1], alpha1w + nD, alpha2w + nND)
  
  newlist = list("nND" = nND, "prior" = prior, "post" = post, 
                 "relative_belief_ratio" = relative_belief_ratio, "plausible_region" = plausible_region,
                 "prior_content" = prior_content, "posterior_content" = posterior_content,
                 "RB_estimate_of_prevalence_w" = RB_estimate_of_prevalence_w)
  return(newlist)
}

prior_compute_values = function(alpha1w, alpha2w, grid){
  # Computs values for the prior - assuming we don't know anything about the posterior.
  prior = dbeta(grid, alpha1w, alpha2w)
  return(prior)
}

compute_credible_region = function(alpha1w, alpha2w, n, nD, grid, gamma, delta, relative_belief_ratio, 
                                   posterior_content, plausible_region){
  # Computes the credible region. At first, there's no default input to avoid generating
  # a credible region automatically (it is not necessary.)
  if (check.numeric(gamma) == FALSE){
    err_msg = "Need to put in a valid input for gamma (see graph 1.)"
    return(list("credible_region" = err_msg, "rb_line" = err_msg))
  }
  else{ # This condition runs if the user provides an actual numeric input.
    gamma = as.numeric(gamma)
    if(gamma >= posterior_content){
      err_msg = "Gamma must be less than the posterior content of the plausible region."
      return(list("credible_region" = err_msg, "rb_line" = err_msg))
    } 
    else {
      #half_distance = floor((plausible_region[2]-plausible_region[1])*(1/delta)/2)
      RBR_values = sort(relative_belief_ratio, decreasing = TRUE)
      RBR_values = RBR_values[RBR_values > 1] # sorting for values larger than 1
      for(i in 2:length(RBR_values)){ # doesn't start at the top as the length is 0
        rb_line = RBR_values[i]
        credible_region = c()
        # find the region associated with it
        for(j in 1:length(relative_belief_ratio)){
          if(relative_belief_ratio[j] > rb_line){
            credible_region = c(credible_region, grid[j])
          }
        }
        credible_region = c(min(credible_region), max(credible_region))
        nND = n - nD
        test_area = pbeta(credible_region[2], alpha1w+nD, alpha2w+nND) - pbeta(credible_region[1], alpha1w+nD, alpha2w+nND)
        if(test_area >= gamma){
          break # This means the credible region was actually found
        }
      }
      
      newlist = list("credible_region" = credible_region, "rb_line" = rb_line)
      # Note: rb_line should be the upper dotted line - helps define a valid credible region
      return(newlist)
    }
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
  
  newlist = list("relative_belief_ratio_at_w0" = relative_belief_ratio_at_w0, 
                 "w0_interval" = interval, "strength" = strength)
  return(newlist)
}

################################################################
# PLOTS                                                        #
################################################################

generate_prior_post_graph = function(prior, post, plausible_region, grid, credible_region = FALSE,
                                     colour_choice = c("blue", "green", "#b3bfff", "#81ddff"),
                                     lty_type = c(2, 2, 3, 3),
                                     transparency = 0.1){
  # This generates the graph for the prior and the posterior of the prevalence.
  # colour choice goes in the following order: prior, posterior, plausible region, and credible region.
  
  # Determining what x-axis to show for a better graph
  x_prior = obtain_x_interval(prior, grid, 0.1)
  x_post = obtain_x_interval(post, grid, 0.1)
  
  # Constructs an interval for the x and y region
  x_interval = c(min(x_prior[1], x_post[1]), max(x_prior[length(x_prior)], x_post[length(x_post)]))
  y_interval = c(0, max(c(prior, post)))
  
  # Plots of the Prior and the Posterior
  plot(grid, prior, type='l', lty = lty_type[1], lwd = 2, xlim = x_interval, ylim = y_interval,
       main = "Graph of the Prior and Posterior of w", ylab = "Densities", xlab = "w", 
       col = colour_choice[1])
  lines(grid, post, col = colour_choice[2], type = "l", lty = lty_type[2], lwd = 2)
  abline(v = plausible_region[1], col = colour_choice[3], lwd = 2, lty = lty_type[3])
  abline(v = plausible_region[2], col = colour_choice[3], lwd = 2, lty = lty_type[3])
  
  # Getting inner rgb colour for transparent effect
  rgb_prior = col2rgb(colour_choice[1])
  rgb_post = col2rgb(colour_choice[2])
  
  polygon(grid, post, col = rgb(rgb_post[1]/255, rgb_post[2]/255, rgb_post[3]/255, 
                                alpha = transparency), border = NA)
  polygon(grid, prior, col = rgb(rgb_prior[1]/255, rgb_prior[2]/255, rgb_prior[3]/255, 
                                 alpha = transparency), border = NA)
  #original colours: green rgb(146/255, 255/255, 133/255, alpha = 0.3) 
  # blue rgb(133/255, 198/255, 255/255, alpha = 0.3)
  
  if (typeof(credible_region) == "double") { # need both to run properly
    abline(v=credible_region[1], col=colour_choice[4], lwd = 2, lty = lty_type[4]) 
    abline(v=credible_region[2], col=colour_choice[4], lwd = 2, lty = lty_type[4])
    #abline(h=rb_line, col="#81ddff", lwd = 2, lty = 2)
    legend("topleft", legend = c("Prior", "Posterior", "Plausible Region", "Credible Region"), lwd = 2, 
           col = c(colour_choice[1], colour_choice[2], colour_choice[3], colour_choice[4]), 
           lty = c(lty_type[1], lty_type[2], lty_type[3], lty_type[4]))
  } else {
    legend("topleft", legend = c("Prior", "Posterior", "Plausible Region"), lwd = 2, 
           col = c(colour_choice[1], colour_choice[2], colour_choice[3]), 
           lty = c(lty_type[1], lty_type[2], lty_type[3]))
  }
}

# This is for constructing the prior only
generate_prior_graph = function(prior, grid, colour_choice = "blue", lty_type = 2,
                                transparency = 0.1){
  # Constructs an interval for the x and y region
  x_interval = obtain_x_interval(prior, grid, 0.1)
  y_interval = c(0, max(prior))
  
  # Plots of the Prior and the Posterior
  plot(grid, prior, type='l', lty = lty_type, lwd = 2, xlim = x_interval, ylim = y_interval,
       main = "Graph of the Prior of w", ylab = "Densities", xlab = "w", col = colour_choice)
  
  # Getting inner rgb colour for transparent effect
  rgb_choice = col2rgb(colour_choice)
  
  polygon(grid, prior, col = rgb(rgb_choice[1]/255, 
                                 rgb_choice[2]/255,
                                 rgb_choice[3]/255, alpha = transparency), border = NA)
  # colour originally used: rgb(133/255, 198/255, 255/255, alpha = 0.3)
}

generate_rbr_graph = function(relative_belief_ratio, plausible_region, grid, credible_region = FALSE,
                              rb_line = FALSE, 
                              colour_choice = c("red", "#b3bfff", "royalblue1", "#81ddff"),
                              lty_type = c(2, 3, 2, 3),
                              transparency = 0.1){
  # This generates the graph for the relative belief ratio of the prevalence.
  # colour choice goes in the following order: relative belief ratio, plausible region,
  # line of y = 1, credible region.
  
  # Constructs an interval for the x and y region
  x_interval = obtain_x_interval(relative_belief_ratio, grid, 0.05)
  y_interval = c(0, max(relative_belief_ratio))
  # For the Plausible Region
  lower_bd = plausible_region[1]
  upper_bd = plausible_region[length(plausible_region)]
  
  plot(grid, relative_belief_ratio, type='l', lty = lty_type[1], lwd = 2, 
       xlim = x_interval, ylim = y_interval,
       main = "Graph of the Relative Belief Ratio of w", ylab = "RBR", xlab = "w", 
       col = colour_choice[1])
  abline(h = 1, col=colour_choice[3], lwd = 2, lty = lty_type[3])
  
  abline(v = lower_bd, col = colour_choice[2], lwd = 2, lty = lty_type[2])
  abline(v = upper_bd, col = colour_choice[2], lwd = 2, lty = lty_type[2])
  # Colouring in the area between the plausible region and when the RBR > 1
  l = min(which(grid >= plausible_region[1]))
  h = max(which(grid < plausible_region[2]))
  rgb_rb = col2rgb(colour_choice[1])
  polygon(c(grid[c(l, l:h, h)]),
          c(1, relative_belief_ratio[l:h], 1),
          col = rgb(rgb_rb[1]/255, rgb_rb[2]/255, rgb_rb[3]/255, alpha = transparency), border = NA)
  # original colour (purple): rgb(197/255, 132/255, 255/255, alpha = 0.3)
  
  if ((typeof(credible_region) == "double") & (typeof(rb_line) == "double")) { # need both to run properly
    abline(v=credible_region[1], col=colour_choice[4], lwd = 2, lty = lty_type[4])
    abline(v=credible_region[2], col=colour_choice[4], lwd = 2, lty = lty_type[4])
    abline(h = rb_line, col=colour_choice[4], lwd = 2, lty = lty_type[4])
    rgb_cr = col2rgb(colour_choice[4])
    # Original colour: rgb(148/255, 180/255, 255/255, alpha = 0.2)
    legend("bottomleft", legend = c("Relative Belief Ratio", "Plausible Region", "Credible Region",
                                    "Gamma (Area)"), lwd = 2, 
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


generate_relative_belief_ratio_at_w0_graph = function(relative_belief_ratio, 
                                                      relative_belief_ratio_at_w0, 
                                                      w0_interval, 
                                                      grid,
                                                      colour_choice = c("red", "navy", 
                                                                        "#7c83e8", "#996DEC"),
                                                      lty_type = c(2, 2, 3),
                                                      transparency = 0.1){
  # order of items: rbr, rbr at w0, bounds, area of strength
  
  # note: the following interval assumes there are breaks (for now):
  x_interval = obtain_x_interval(relative_belief_ratio, grid, 0.05)
  y_interval = c(0, max(relative_belief_ratio))
  lower_bd = w0_interval[1]
  upper_bd = w0_interval[length(w0_interval)]
  
  plot(grid, relative_belief_ratio, type='l', lty = lty_type[1], lwd = 2, xlim = x_interval, 
       ylim = y_interval, main = "Graph of the Relative Belief Ratio at w0", 
       ylab = "RBR", xlab = "w", col = colour_choice[1])
  abline(h = relative_belief_ratio_at_w0, lwd = 2, lty = lty_type[2], col = colour_choice[2])
  abline(v = lower_bd, col = colour_choice[3], lwd = 2, lty = lty_type[3])
  abline(v = upper_bd, col = colour_choice[3], lwd = 2, lty = lty_type[3])
  
  str_col = col2rgb(colour_choice[4])
  
  legend("topleft", legend = c("Relative Belief Ratio", "Relative Belief Ratio of w0", 
                               "Interval", "Strength (Area)"), 
         lwd = c(colour_choice[1], colour_choice[2], colour_choice[3], colour_choice[4]), 
         col = c('red', 'navy', '#7c83e8', 
                 rgb(str_col[1]/255, str_col[2]/255, str_col[3]/255, 
                     alpha = transparency)), 
         lty = c(lty_type[1], lty_type[2], lty_type[3], 1))
  
  polygon(c(grid[grid >= upper_bd], upper_bd),
          c(relative_belief_ratio[grid >= upper_bd], 0),
          col = rgb(str_col[1]/255, str_col[2]/255, str_col[3]/255, 
                    alpha = transparency), border = NA)
  polygon(c(grid[grid <= lower_bd ], lower_bd),
          c(relative_belief_ratio[grid <= lower_bd ], 0),
          col = rgb(str_col[1]/255, str_col[2]/255, str_col[3]/255, 
                    alpha = transparency), border = NA)
}

################################################################
# CONSTRUCTING THE DATA FRAME                                  #
################################################################

RB_generate_dataframe = function(grid, prior, post, relative_belief_ratio){
  # This loop is to bring back the NA values to ensure users
  # do not misuse when downloaded.
  for(i in 1:length(prior)){
    if(is.na(post[i]/prior[i])){
      relative_belief_ratio[i] = NA
    }
  }
  RB_df = data.frame(grid, prior, post, relative_belief_ratio)
  colnames(RB_df) = c("Grid Point", "Prior", "Posterior", "Relative Belief Ratio")
  return(RB_df)
}

RB_generate_priorframe = function(grid, prior){
  df = data.frame(grid, prior)
  colnames(df) = c("Grid Point", "Prior")
  return(df)
}

