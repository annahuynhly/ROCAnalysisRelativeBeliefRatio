
#epsilon = 0.25
#test = dirichlet_process_a(epsilon)
#plot(test$upper_bounds, main = 'convergence of upper bounds')

elicit_beta = function(r, epsilon, s1, s2){
  # elicitation, for beta distribution probabilities
  v1 = min(r + epsilon, 1)
  v2 = max(0, r - epsilon)
  val = pbeta(v1, shape1 = s1, shape2 = s2) - pbeta(v2, shape1 = s1, shape2 = s2)
  return(val)
}

elicit_upper_bds = function(r, a){
  # elicitation, upper values in bounds
  # search, parameter 'a'
  up_vals = c()
  for (i in 1:length(r)){
    up_val = elicit_beta(r[i], epsilon, a*r[i], a*(1-r[i])) 
    up_vals = c(up_vals, up_val)
  }
  return(up_vals)
}

dirichlet_process_a = function(epsilon){
  # Computes the a for the dirichlet process
  it = 1 # iteration
  a = c(1) # initial
  up_bds = c()
  up_bd = c(0.11) #initialization
  
  # start
  while (it >= 1){
    
    final = list()
    r = seq(0.01, 0.99, by = 0.01)
    up_vals = elicit_upper_bds(r, a)
    
    # next iteration
    a = a+0.05
    up_bd = max(1-up_vals)
    #print(up_bd)
    up_bds = c(up_bds, up_bd)
    
    # criteria
    if (up_bd < 0.1){
      final$a = a
      final$up_bd = up_bd
      final$up_bds = up_bds
      #return(final)
      break
    }
    it = it+1
  }
  
  return(list("a" = final$a, "upper_bound" = final$up_bd,
              "upper_bounds" = final$up_bds))
}