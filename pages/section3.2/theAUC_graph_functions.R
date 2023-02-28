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
    hist_post = hist(AUC_post, breaks = bins, plot = FALSE) 
    hist_prior = hist(AUC_prior, breaks = bins, plot = FALSE)
    prior_vals = convert_hist_to_density_plot(hist_post$density, hist_post$breaks)
    post_vals = convert_hist_to_density_plot(hist_prior$density, hist_prior$breaks)
    
    prior_linearea = grab_density_plot_area(prior_vals$grid, prior_vals$density)
    post_linearea = grab_density_plot_area(post_vals$grid, post_vals$density)
    
    return(list("PriorArea" = prior_linearea, "PostArea" = post_linearea))
  }
  
  if(showbars == FALSE){
    hist_colours = c("#ffffff", "#ffffff") # Forcing it to look white to disappear
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
  bins = theAUC_grid(delta) #bins = c(0, bins, 1)
  
  if(showbars == FALSE){
    colours = "#ffffff"
  } else {
    colours = rgb(0/255, 255/255, 204/255, alpha = 0.5)
  }
  
  AUC_RBR[is.na(AUC_RBR)] = 0
  
  myhist =list(breaks=bins, counts=AUC_RBR, density=AUC_RBR/delta)
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
    l = min(which(rbr_density_plot$grid >= plausible_region[1]))
    h = max(which(rbr_density_plot$grid <= plausible_region[2]))
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

plots_AUC_copt = function(priorc_opt = FALSE, postc_opt = FALSE, RBc_opt = FALSE,
                          prior_label = 3, post_label = 4, rb_label = 8){
  if((typeof(priorc_opt) == "double") & (typeof(postc_opt) == "double")){
    if(length(priorc_opt) != length(postc_opt)){
      return("Error: length of priorc_opt and postc_opt are not the same.")
    }
    m = length(priorc_opt)
    plot(1:m,priorc_opt,xlab="X",ylab="probability", pch=prior_label, lwd = 2, col = "red", 
         main = "Plot of the Prior and the Posterior of Copt")
    points(1:m,postc_opt,pch=post_label, lwd = 2, col = "blue")
    
    legend("topright", legend = c("Prior", "Posterior"),
           lwd = 2, cex = 1.2, col = c("red", "blue"), pch = c(prior_label, post_label), lty = c(NA, NA))
    
  } else if(typeof(RBc_opt) == "double"){
    m = length(RBc_opt)
    plot(1:m, RBc_opt, xlab="X", ylab="Relative Belief Ratio", pch=rb_label, col = "#16957C",
         main = "Plot of the Relative Belief Ratio of Copt")
    legend("topright", legend = c("Relative Belief Ratio"),
           lwd = 2, cex = 1.2, col = c("#16957C"), pch = c(rb_label), lty = c(NA))
  }
}