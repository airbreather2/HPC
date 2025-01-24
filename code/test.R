
x <- c(1, 2,3, 7 , 22) 
y <- c(1, 2,3, 7 , 22, 4, 6, 7, 8) 

sum_vect <- function(x, y) {
  # Check if the lengths of x and y are different
  if (length(x) != length(y)) {
    # Calculate the difference in lengths
    diff <- abs(length(x) - length(y))
    
    # Pad the shorter vector with zeros
    if (length(x) > length(y)) {
      y <- c(y, rep(0, diff))  # Pad y with zeros if y is shorter
    } else {
      x <- c(x, rep(0, diff))  # Pad x with zeros if x is shorter
    }
  }
  
  # Return the sum of the two vectors
  return(x + y)
}

sum_vect(x, y)


