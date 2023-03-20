
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

RBR_estimate_of_AUC = function(grid, RBR_of_AUC){
  # Assumption is that length(grid) == length(RBR_of_AUC)
  max_occurs = which.max(RBR_of_AUC)
  df = data.frame(grid[max_occurs], RBR_of_AUC[max_occurs])
  colnames(df) = c("Estimate of AUC", "| Relative Belief Ratio of the Estimated AUC")
  return(df)
}

