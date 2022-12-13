# This represents all helper functions!

library(stringr)

# Helper Functions for: realdataROC_1_page.R

convert_char_to_vector = function(x){
  # This function turns characters, such as "1, 2, 3", 
  # into a vector: c(1, 2, 3)
  x = str_replace_all(x, fixed(" "), "") # removes all spaces
  x = as.integer(strsplit(x, ",")[[1]]) # converts to vector
  x = x[!is.na(x)]
  return(as.double(x))
}

valid_vector = function(x){
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
