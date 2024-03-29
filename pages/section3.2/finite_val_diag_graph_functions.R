################################################################
# FUNCTIONS FOR GRAPHS                                         #
################################################################

convert_hist_to_density_plot = function(hist_density, hist_breaks, num_average_pts = 3, 
                                        showplot = FALSE, colour = "black", lty_ver = 2){
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
      lines(new_grid, hist_density, lty = lty_ver, type = "l", lwd = 3, col = colour)
    }
    return(list("grid" = new_grid, "density" = hist_density))

  } else {
    # when we have more items to average out
    new_density = rep(0, length(hist_density))
    
    num_neighbours = floor(num_average_pts/2)
    for(i in 1:length(hist_density)){
      if(i <= num_neighbours | (length(hist_density) - i) < num_neighbours){
        if(i == 1 | i == length(hist_density)){
          new_density[i] = hist_density[i]
        } else {
          if (i <= num_neighbours){
            pts = i - 1
          } else if ((length(vector) - i) < num_neighbours){
            pts = length(vector) - i
          }
          new_density[i] = sum(hist_density[(i - pts):(i + pts)])/(2*pts + 1)
        }
      } else {
        lower_int = i - num_neighbours
        upper_int = i + num_neighbours
        new_density[i] = sum(hist_density[lower_int:upper_int])/(2*num_neighbours + 1)
      }
    }

    if(showplot == TRUE){
      lines(new_grid, new_density, lty = lty_ver, type = "l", lwd = 3, col = colour)
    }
  }
  return(list("grid" = new_grid, "density" = new_density))
}

density_hist_AUC_prior_post = function(delta, AUC_prior, AUC_post,
                                       densityplot = FALSE, 
                                       showbars = FALSE, 
                                       colour_choice = c("#FF6666", "#6699FF"),
                                       lty_type = c(2, 2),
                                       transparency = 0.2,
                                       legend_position = "topleft"){
  # order: prior, post
  bins = closed_bracket_grid(delta)
  
  if(densityplot == FALSE & showbars == FALSE){ # this is to extract the density values only
    hist_post = hist(AUC_post, breaks = bins, plot = FALSE) 
    hist_prior = hist(AUC_prior, breaks = bins, plot = FALSE)
    prior_vals = convert_hist_to_density_plot(hist_post$density, hist_post$breaks)
    post_vals = convert_hist_to_density_plot(hist_prior$density, hist_prior$breaks)
    
    prior_linearea = grab_density_plot_area(prior_vals$grid, prior_vals$density)
    post_linearea = grab_density_plot_area(post_vals$grid, post_vals$density)
    
    return(list("PriorArea" = prior_linearea, "PostArea" = post_linearea))
  }
  
  # Line colours
  rgb_prior = col2rgb(colour_choice[1])
  rgb_post = col2rgb(colour_choice[2])
  prior_line_col = rgb(rgb_prior[1]/255, rgb_prior[2]/255, rgb_prior[3]/255)
  post_line_col = rgb(rgb_post[1]/255, rgb_post[2]/255, rgb_post[3]/255)
  
  # Histogram colours
  if(showbars == FALSE){ # Forcing it to look white to disappear
    prior_hist_col = "#ffffff"
    post_hist_col = "#ffffff"
  } else { # assuming the user manually put in a list of colours
    prior_hist_col = rgb(rgb_prior[1]/255, rgb_prior[2]/255, rgb_prior[3]/255, 
                         alpha = transparency)
    post_hist_col = rgb(rgb_post[1]/255, rgb_post[2]/255, rgb_post[3]/255, 
                        alpha = transparency)
  }
  
  hist_post = hist(AUC_post, prob = TRUE, breaks = bins, xlab = "AUC", ylab="Density",
                   main = "Density Histograms: The Prior & Posterior of the AUC", 
                   col = post_hist_col, border = "#ffffff") 
  hist_prior = hist(AUC_prior, prob = TRUE, breaks = bins, xlab="AUC", ylab="Density",
                    col = prior_hist_col, border = "#ffffff", add = TRUE)
  
  legend(legend_position, legend = c("Prior", "Posterior"), 
        lwd = 2,
        col = c(prior_line_col, post_line_col),
        lty = c(lty_type[1], lty_type[2]), inset = 0.02, cex = 0.8)
  
  if(densityplot == TRUE){
    convert_hist_to_density_plot(hist_post$density, hist_post$breaks, num_average_pts = 3, 
                                 showplot = TRUE, colour = post_line_col, lty_ver = lty_type[2])
    convert_hist_to_density_plot(hist_prior$density, hist_prior$breaks, num_average_pts = 3, 
                                 showplot = TRUE, colour = prior_line_col, lty_ver = lty_type[1])
  }
}

density_hist_AUC_RBR = function(delta, AUC_RBR,
                                rb_line = FALSE, densityplot = FALSE, showbars = FALSE, 
                                colour_choice = c("#05DEB2", 
                                                  "#3333FF", "#5b10a7"),
                                lty_type = c(2, 2, 3),
                                transparency = 0.2,
                                legend_position = "topleft"){
  # Order of colours: RBR, y = 1 line, credible line
  bins = closed_bracket_grid(delta)
  
  rgb_rbr = col2rgb(colour_choice[1])
  if(showbars == FALSE){
    colours = "#ffffff" # this is referring to the bins, not the rest.
  } else {
    colours = rgb(rgb_rbr[1]/255, rgb_rbr[2]/255, rgb_rbr[3]/255, alpha = transparency)
    #colours = rgb(0/255, 255/255, 204/255, alpha = transparency)
  }
  
  AUC_RBR[is.na(AUC_RBR)] = 0
  
  myhist = list(breaks = bins, counts = AUC_RBR, density = AUC_RBR/delta)
  class(myhist) = "histogram"
  myhist$counts = myhist$counts[-length(myhist$counts)] #removing the last element
  
  plot(myhist, xlab = "AUC", ylab = "Relative Belief Ratio", 
       main = "The Relative Belief Ratio of the AUC",
       col = colours, freq = TRUE, border = "#ffffff")
  abline(h = 1, col = colour_choice[2], lwd = 2, lty = lty_type[2])
  #warning message doesn't seem to be much of an issue
  if(densityplot == TRUE){
    rbr_density_plot = convert_hist_to_density_plot(myhist$counts, myhist$breaks, 
                                                    num_average_pts = 3, showplot = TRUE,
                                                    lty_ver = lty_type[1],
                                                    colour = colour_choice[1])
  }
  
  if(typeof(rb_line) == "double"){
    abline(h = rb_line, col = colour_choice[3], lwd = 2, lty = lty_type[3])
    legend(legend_position, 
           legend = c("Relative Belief Ratio", "Credible Region Line",
                      "RBR = 1"), 
           lwd = 2, 
           col = c(colour_choice[1], colour_choice[3], colour_choice[2]), 
           lty = c(lty_type[1], lty_type[3], lty_type[2]), inset = 0.02, cex = 0.8)
    
  } else {
    legend(legend_position, 
           legend = c("Relative Belief Ratio", "RBR = 1"), 
           lwd = 2, 
           col = c(colour_choice[1], colour_choice[2]), 
           lty = c(lty_type[1], lty_type[2]), inset = 0.02, cex = 0.8)
  }
}

plots_AUC_copt = function(priorc_opt = FALSE, postc_opt = FALSE, RBc_opt = FALSE,
                          prior_label = 3, post_label = 4, rb_label = 8,
                          colour_choice = c("red", "blue", "#16957C"),
                          legend_position = "topright"){
  if((typeof(priorc_opt) == "double") & (typeof(postc_opt) == "double")){
    if(length(priorc_opt) != length(postc_opt)){
      return("Error: length of priorc_opt and postc_opt are not the same.")
    }
    m = length(priorc_opt)
    plot(1:m, priorc_opt, type = "b", xlab = "X",ylab = "probability", pch = prior_label, 
         lwd = 2, col = colour_choice[1], 
         main = "Plot of the Prior and the Posterior of the Cutoff")
    #points(1:m,postc_opt, pch=post_label, lwd = 2, col = colour_choice[2])
    lines(1:m, postc_opt, pch = post_label, col = colour_choice[2], type = "b", lwd = 2)
    
    legend(legend_position, legend = c("Prior", "Posterior"),
           lwd = 2, cex = 1.2, col = c(colour_choice[1], colour_choice[2]), 
           pch = c(prior_label, post_label), lty = c(NA, NA))
    
  } else if(typeof(RBc_opt) == "double"){
    m = length(RBc_opt)
    plot(1:m, RBc_opt, type = "b", xlab="X", ylab="Relative Belief Ratio", pch = rb_label, lwd = 2,
         col = colour_choice[3],
         main = "Plot of the Relative Belief Ratio of the Cutoff")
    legend(legend_position, legend = c("Relative Belief Ratio"),
           lwd = 2, cex = 1.2, col = c(colour_choice[3]), 
           pch = c(rb_label), lty = c(NA))
  }
}