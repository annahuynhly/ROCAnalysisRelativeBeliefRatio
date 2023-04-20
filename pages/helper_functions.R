
closed_bracket_grid = function(delta){
  # Creates a grid of values from 0 to 1 based on the distance between two points (delta).
  # Previous names: finite_val_grid & binormal_val_grid_1
  grid = seq(0, 1, length= (1/delta)+1)
  return(grid)
}

open_bracket_grid = function(delta){
  # Creates a grid of values from 0 to 1 based on the distance between two points (delta).
  # Previous names: RB_distance_that_matters & binormal_val_grid_2
  grid = seq(delta/2, 1 - delta/2, length=(1/delta))
  return(grid)
}

obtain_x_interval = function(condition_list, grid, smallest_bound){
  # helper function for constructing an x interval for graph building
  x_region = c()
  for (i in 1:length(grid)){
    if(condition_list[[i]] > smallest_bound){
      x_region = c(x_region, as.numeric(grid[i]))
    }
  }
  return(c(x_region[1], x_region[length(x_region)]))
}

convert_to_hex = function(hex_colour){
  hex_colour = gsub(" ", "", hex_colour)
  first_char = substr(hex_colour, 1, 1)
  if(first_char != "#"){
    return(paste("#", hex_colour, sep = ""))
  } else {
    return(hex_colour)
  }
}

convert_char_to_vector = function(x){
  if (is.character(x) == FALSE){
    return("Invalid input: the input is not a character.")
  }
  # This function turns characters, such as "1, 2, 3", 
  # into a vector: c(1, 2, 3)
  x = str_replace_all(x, fixed(" "), "") # removes all spaces
  x = (strsplit(x, ",")[[1]])
  
  check_numeric_count = 0
  for(i in 1:length(x)){
    if(check.numeric(x[i])){
      check_numeric_count = check_numeric_count + 1
    }
  }
  if (check_numeric_count == length(x)){
    x = as.numeric(x) # converts to vector
    #x = as.integer(strsplit(x, ",")[[1]]) # converts to vector
    x = x[!is.na(x)]
    return(as.double(x))
  } else {
    return("Invalid vector. Not all numbers are numeric.")
  }
}

# NOTE: CHANGED FUNCTION NAME
valid_numeric_vector = function(x){
  # This function double checks to insure that the vector
  # is valid to use for any weird edge-cases that the player
  # might try to initiate.
  for (i in 1:length(x)) {
    if (is.numeric(x[[i]]) == FALSE){
      # This input contains invalid characters
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
}

create_necessary_vector = function(x){
  # Given a string of values, such as "1, 1, 1, 1, 1", converts it to a vector if
  # it isn't already numeric.
  if(is.character(x) == TRUE){
    return(convert_char_to_vector(x))
  } else if (typeof(x) == "double" | valid_numeric_vector(x) == TRUE){
    return(x)
  } else {
    return("Invalid vector.")
  }
}

# changed finite_val_generate_w -> generate_w
generate_w = function(w = FALSE, alpha1w = NA, alpha2w = NA, 
                      nD = NA, nND = NA, version = NA){
  #Generates w based on the inputs.
  if(typeof(w) == "double"){
    return(w)
  } else if (w == FALSE & version == "prior"){ # This is a sanity check
    if(typeof(alpha1w) == "double" & typeof(alpha2w) == "double"){
      return(rbeta(1, alpha1w, alpha2w))
    } else {
      return("Invalid alpha1w, alpha2w.")
    }
  } else if (w == FALSE & (version == "post" | version == "posterior")){
    if(typeof(alpha1w) == "double" & typeof(alpha2w) == "double"){
      return(rbeta(1, alpha1w + nD, alpha2w + nND))
    }
  } else {
    return("Invalid value for w.")
  }
}

RBR_estimate_of_AUC = function(grid, RBR_of_AUC, condition = "unconditional"){
  # Assumption is that length(grid) == length(RBR_of_AUC)
  max_occurs = which.max(RBR_of_AUC)
  df = data.frame(grid[max_occurs], RBR_of_AUC[max_occurs])
  if (condition == "conditional"){
    df[1] = df[1] + 0.5
  }
  colnames(df) = c("Estimate of AUC", "| Relative Belief Ratio of the Estimated AUC")
  return(df)
}

NA_to_0 = function(vector){
  vector[is.na(vector)] = 0
  return(vector)
}

force_bounds_0 = function(v){
  # idea: force the first and last values of a vector to be zero
  # needs to be moved
  v = v[2:(length(v)-1)]
  v = c(0, v, 0)
}

average_vector_values = function(vector, num_average_pts = 3){
  # num_average_pts: the number of density bins closely added to each other to get
  # a smoother density plot. (Reduce peaks.)
  if(num_average_pts %% 2 == 0){
    # Note: the even case is harder to code. For this instance, since the number of average points
    # will be pre-determined for the user (in terms of the plots), I have decided to not add
    # even functionality for now.
    return("Error: num_average_pts must be an odd number.")
  }
  
  if(num_average_pts == 1){
    return(vector)
  } 
  new_vector = rep(0, length(vector))
  
  pts = 0
  num_neighbours = floor(num_average_pts/2)
  for(i in 1:length(vector)){
    if(i <= num_neighbours | (length(vector) - i) < num_neighbours){ # Edge points case
      if(i == 1 | i == length(vector)){
        new_vector[i] = vector[i]
      } else {
        if (i <= num_neighbours){
          pts = i - 1
        } else if ((length(vector) - i) < num_neighbours){
          pts = length(vector) - i
        }
        new_vector[i] = sum(vector[(i-pts):(i+pts)])/(2*pts + 1)
      }
    } else {
      lower_int = i - num_neighbours
      upper_int = i + num_neighbours
      new_vector[i] = sum(vector[lower_int:upper_int])/(2*num_neighbours + 1)
    }
  }
  
  return(new_vector)
}

copt_transform = function(vector){
  first = vector[1]
  last = vector[length(vector)]
  err_msgs = c("Need to put in a valid input for gamma.",
               "Gamma must be less than the posterior content of the plausible region.")

  if(first %in% err_msgs){
    return(err_msgs[match(first, err_msgs)])
  }
  
  if (is.numeric(first) == FALSE){
    first = as.numeric(first)
  }
  if (is.numeric(last) == FALSE){
    last = as.numeric(last)
  }
  # Vector in this case is often used for the plausible or the credible region.
  return(c(tan(pi*first - pi/2), tan(pi*last - pi/2)))
}





