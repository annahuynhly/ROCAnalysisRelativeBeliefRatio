
RB_distance_that_matters = function(delta){ # MIGHT NEED TO MOVE THIS OUT - USED IN OTHER FUNCTS
  # Creates a grid of values from 0 to 1
  grid = seq(delta/2, (1-delta/2), length= 1/delta)
  return(grid)
}

RBR_compute_values = function(alpha1w, alpha2w, n, nD, grid){
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
  RB_estimate_of_prevalence_w = (match(max(relative_belief_ratio), relative_belief_ratio))/length(grid)
  
  # This finds a plausible region.
  # WARNING: the following interval assumes there are no breaks, so it wouldn't work if there's a "peak".
  plausible_region = c(plausible_region[1], plausible_region[length(plausible_region)])
  
  # finding the prior and posterior content
  prior_content = pbeta(plausible_region[2], alpha1w, alpha2w) - pbeta(plausible_region[1], alpha1w, alpha2w)
  posterior_content = pbeta(plausible_region[2], alpha1w + nD, alpha2w + nND) - pbeta(plausible_region[1], alpha1w + nD, alpha2w + nND)
  
  # WARNING: might be incorrect!
  # finding the area for π(pl(x) | x)... Need to change variable name!
  #sup_gamma = plausible_region[2] - plausible_region[1] # Note that length is 1, so...
  
  newlist = list("nND" = nND, "prior" = prior, "post" = post, 
                 "relative_belief_ratio" = relative_belief_ratio, "plausible_region" = plausible_region,
                 "prior_content" = prior_content, "posterior_content" = posterior_content,
                 "RB_estimate_of_prevalence_w" = RB_estimate_of_prevalence_w)
  return(newlist)
}

prior_compute_values = function(alpha1w, alpha2w, grid){
  prior = dbeta(grid, alpha1w, alpha2w)
  
  return(prior)
}

# TODO: might've computed this wrong
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
      for(i in 2:length(RBR_values)){ # doesnt start at the top as the length is 0
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

generate_prior_post_graph = function(prior, post, plausible_region, grid, credible_region = FALSE){
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
       main = "Graph of the Prior and Posterior of w", ylab = "Densities", xlab = "w", col = "blue")
  lines(grid, post,col="green", type = "l", lty = 2, lwd = 2)
  abline(v=plausible_region[1], col="#b3bfff", lwd = 2, lty = 3)
  abline(v=plausible_region[2], col="#b3bfff", lwd = 2, lty = 3)
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

generate_prior_graph = function(prior, grid){
  
  # Obtaining an x region
  x_region = c()
  for (i in 1:length(grid)){
    if ((prior[[i]] > 0.1)){
      x_region = c(x_region, as.numeric(grid[i]))
    }
  }
  # Constructs an interval for the x and y region
  x_interval = c(x_region[1], x_region[length(x_region)])
  y_interval = c(0, max(prior))
  
  # Plots of the Prior and the Posterior
  plot(grid, prior, type='l', lty = 2, lwd = 2, xlim = x_interval, ylim = y_interval,
       main = "Graph of the Prior of w", ylab = "Densities", xlab = "w", col = "blue")
  polygon(grid, prior, col = rgb(133/255, 198/255, 255/255, alpha = 0.3), border = NA)
  
}

generate_rbr_graph = function(relative_belief_ratio, plausible_region, grid, credible_region = FALSE,
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
  lower_bd = plausible_region[1]
  upper_bd = plausible_region[length(plausible_region)]
  
  plot(grid, relative_belief_ratio, type='l', lty = 2, lwd = 2, xlim = x_interval, ylim = y_interval,
       main = "Graph of the Relative Belief Ratio of w", ylab = "RBR", xlab = "w", col = "red")
  abline(h=1, col="royalblue1", lwd = 2, lty = 2)
  abline(v=lower_bd, col="#b3bfff", lwd = 2, lty = 3)
  abline(v=upper_bd, col="#b3bfff", lwd = 2, lty = 3)
  # Colouring in the area between the plausible region and when the RBR > 1
  l <- min(which(grid >= plausible_region[1]))
  h <- max(which(grid < plausible_region[2]))
  polygon(c(grid[c(l, l:h, h)]),
          c(1, relative_belief_ratio[l:h], 1),
          col = rgb(197/255, 132/255, 255/255, alpha = 0.3), border = NA)
  
  if ((typeof(credible_region) == "double") & (typeof(rb_line) == "double")) { # need both to run properly
    abline(v=credible_region[1], col="#81ddff", lwd = 2, lty = 3) # might change lty?
    abline(v=credible_region[2], col="#81ddff", lwd = 2, lty = 3)
    abline(h=rb_line, col="#81ddff", lwd = 2, lty = 2)
    legend("bottomleft", legend = c("Relative Belief Ratio", "Plausible Region", "Credible Region",
                                    "Gamma (Area)"), lwd = 2, 
           col = c('red', '#b3bfff', '#81f5ff', rgb(148/255, 180/255, 255/255, alpha = 0.2)), 
           lty = c(2, 3, 3, 1))
    polygon(x = c(credible_region[1], credible_region[1], credible_region[2], credible_region[2]), 
            y = c(0, rb_line, rb_line, 0), col = rgb(148/255, 180/255, 255/255, alpha = 0.2), border = NA)   
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
       ylim = y_interval, main = "Graph of the Relative Belief Ratio at w0", 
       ylab = "RBR", xlab = "w", col = "red")
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
  colnames(RB_df) = c("Grid Point", "Prior", "Posterior", "Relative Belief Ratio")
  return(RB_df)
}

RB_generate_priorframe = function(grid, prior){
  df = data.frame(grid, prior)
  colnames(df) = c("Grid Point", "Prior")
  return(df)
}


# TESTING
#alpha1w = 391.72
#alpha2w = 211.39
#n = 100 # total number
#nD = 68 # total number diseased
#nND = n - nD
#w0 = 0.6
#delta = 0.001
#gamma = 0.5

#grid = RB_distance_that_matters(0.001)
#test_1 = RBR_compute_values(alpha1w, alpha2w, n, nD, grid)
#test_2 = compute_credible_region(alpha1w, alpha2w, n, nD, grid, gamma, delta, test_1$relative_belief_ratio, 
#                                 test_1$posterior_content, test_1$plausible_region)
#generate_prior_post_graph(test_1$prior, test_1$post, test_1$plausible_region, grid, test_2$credible_region)
#generate_rbr_graph(test_1$relative_belief_ratio, test_1$plausible_region, grid, test_2$credible_region,
#                  test_2$rb_line)

