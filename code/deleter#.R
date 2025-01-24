for (i in 1:100){
  iter <- i 
  
  if (iter %% 4 == 0) {
    initial_state <- 1  # Store the first simulation condition
  } else if (iter %% 4 == 1) {
    initial_state <- 2  # Store the second simulation condition
  } else if (iter %% 4 == 2) {
    initial_state <- 3  # Store the third simulation condition
  } else if (iter %% 4 == 3) {
    initial_state <- 4   # Store the fourth simulation condition
  }

print(initial_state)
}
