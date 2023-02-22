
density_hist_AUC_prior_post = function(delta, AUC_prior, AUC_post, plausible_region,
                                       credible_region = FALSE){
  bins = theAUC_grid(delta)
  
  x = hist(AUC_post, prob = TRUE, breaks = bins, xlab="AUC", ylab="Density",
       #ylim = c(0, max(AUC_prior,AUC_post)),
       main="Density Histogram: The Prior & Posterior of the AUC", 
       col = rgb(102/255, 153/255, 255/255, alpha = 0.5), border = "#ffffff")
  y = hist(AUC_prior, prob = TRUE, breaks = bins, xlab="AUC", ylab="Density",
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
  
  # WILL ADD WITH CERTAIN ARGUMENTS
  convert_hist_to_density_plot(x$density, x$breaks, 5, "blue")
  convert_hist_to_density_plot(y$density, y$breaks, 9, "red")
  #return(x)
}

grab_AUC_densities_breaks = function(delta, AUC){
  bins = theAUC_grid(delta)
  x = hist(AUC, breaks = bins, plot = FALSE)
  return(list("density" = x$density, "breaks" = x$breaks))
}

# ERROR HERE: trying to do something new... might just resort to spline
convert_hist_to_density_plot = function(hist_density, hist_breaks, num_average_pts = 3, showplot = FALSE,
                                        colour = "black"){
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
      lines(new_grid, hist_density, lty = 2, type = "l", lwd = 2, col = colour)#, add = TRUE)
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
      lines(new_grid, new_density, lty = 2, type = "l", lwd = 2, col = colour)#, add = TRUE)
    }
  }
  return(list("grid" = new_grid, "density" = new_density))
}

grab_density_plot_area = function(grid, density){
  area = sum(diff(grid) * (head(density,-1)+tail(density,-1)))/2
  return(area)
}




nND = 50
nD = 100
nMonteCarlo = 10000
alpha_ND = c(1, 1, 1, 1, 1) 
alpha_D = c(1, 1, 1, 1, 1)
###m = 5
fND = "29, 7, 4, 5, 5"
fD = "14, 7, 25, 33, 21"
delta = 0.01
gamma = 0.5
w = 0.65

test1 = simulate_AUC_mc_prior(nND, nD, nMonteCarlo, w, alpha_ND, alpha_D)
test2 = simulate_AUC_mc_post(nND, nD, nMonteCarlo, w, alpha_ND, alpha_D, fND, fD)
test3 = compute_AUC_RBR(delta, test1$AUC, test2$AUC, test1$priorc_opt, test2$post_copt)

testprior = grab_AUC_densities_breaks(delta, test1$AUC)
testpost = grab_AUC_densities_breaks(delta, test2$AUC)

par(mfrow = c(1, 1))
density_hist_AUC_prior_post(delta, test1$AUC, test2$AUC, test3$plausible_region)
x4 = convert_hist_to_density_plot(testprior$density, testprior$breaks, num_average_pts = 5)
density_hist_AUC_prior_post(delta, test1$AUC, test2$AUC, test3$plausible_region)
x5 = convert_hist_to_density_plot(testprior$density, testprior$breaks, num_average_pts = 3)
density_hist_AUC_prior_post(delta, test1$AUC, test2$AUC, test3$plausible_region)
x6 = convert_hist_to_density_plot(testprior$density, testprior$breaks, num_average_pts = 1)

density_hist_AUC_prior_post(delta, test1$AUC, test2$AUC, test3$plausible_region)
x1 = convert_hist_to_density_plot(testpost$density, testpost$breaks, num_average_pts = 5)
density_hist_AUC_prior_post(delta, test1$AUC, test2$AUC, test3$plausible_region)
x2 = convert_hist_to_density_plot(testpost$density, testpost$breaks, num_average_pts = 3)
density_hist_AUC_prior_post(delta, test1$AUC, test2$AUC, test3$plausible_region)
x3 = convert_hist_to_density_plot(testpost$density, testpost$breaks, num_average_pts = 1)







# use this: https://stackoverflow.com/questions/4954507/calculate-the-area-under-a-curve

length(testx$breaks)
