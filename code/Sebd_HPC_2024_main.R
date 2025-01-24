# CMEE 2024 HPC exercises R code main pro forma
# You don't HAVE to use this but it will be very helpful.
# If you opt to write everything yourself from scratch please ensure you use
# EXACTLY the same function and parameter names and beware that you may lose
# marks if it doesn't work properly because of not using the pro-forma.

name <- "Seabstian Dohne"
preferred_name <- "Seb"
email <- "sed24@ic.ac.uk"
username <- "sed24"

# Please remember *not* to clear the work space here, or anywhere in this file.
# If you do, it'll wipe out your username information that you entered just
# above, and when you use this file as a 'toolbox' as intended it'll also wipe
# away everything you're doing outside of the toolbox.  For example, it would
# wipe away any automarking code that may be running and that would be annoying!

# Section One: Stochastic demographic population model

# Question 0

state_initialise_adult <- function(num_stages, initial_size) {
  adult <- rep(0, num_stages) 
  adult[length(adult)] <- initial_size  # Use round brackets, not square brackets
  return(adult)
}


state_initialise_spread <- function(num_stages, initial_size) {
  # Create a vector with the base value distributed evenly
  even_spread <- rep(floor(initial_size / num_stages), num_stages)
  
  # Calculate the remainder to distribute
  if (initial_size %% num_stages != 0) {  # Fix: Use '!=' instead of '=!'
    remainder <- initial_size %% num_stages
    
    # Distribute the remainder across the first few stages
    for (i in 1:remainder) {
      even_spread[i] <- even_spread[i] + 1
    }
  }
  
  return(even_spread)
}


# Question 1
# Modified function to accept parameters
question_1 <- function() {
  # Source the script that performs operations
  source("Demographic.R")
  
  
  num_stages = 4
  simul_length= 24
  initial_adult = 100
  initial_spread = 100
  
  
  # Use the input variables
  adult <- state_initialise_adult(num_stages, initial_adult)
  spread <- state_initialise_spread(num_stages, initial_spread)
  
  growth_matrix <- matrix(
    c(0.1, 0.0, 0.0, 0.0, 
      0.5, 0.4, 0.0, 0.0, 
      0.0, 0.4, 0.7, 0.0, 
      0.0, 0.0, 0.25, 0.4), 
    nrow = num_stages, ncol = num_stages, byrow = TRUE
  )
  
  reproduction_matrix <- matrix(
    c(0.0, 0.0, 0.0, 2.6, 
      0.0, 0.0, 0.0, 0.0, 
      0.0, 0.0, 0.0, 0.0, 
      0.0, 0.0, 0.0, 0.0), 
    nrow = num_stages, ncol = num_stages, byrow = TRUE
  )
  
  projection_matrix <- reproduction_matrix + growth_matrix
  
  line1 <- deterministic_simulation(adult, projection_matrix, simul_length)
  
  line2 <- deterministic_simulation(spread, projection_matrix, simul_length)
  
  matplot(cbind(line1, line2), type="l", col = c("red", "blue"), lwd=2, lty = 1, main="Population growth", xlab="Generations", ylab="Population" )
  # Save output as a PNG file
  png(filename = "question_1", width = 600, height = 400)
  Sys.sleep(0.1)
  dev.off()
  
  return("Explain how the initial distribution of the population in different life stages affects the initial and eventual population growth.")
} # 

# Question 2
#sum vect
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
}

question_2 <- function() {
  # Source the script that defines your functions
  source("Demographic.R")
  
  # Define input variables
  num_stages <- 4
  simul_length <- 24
  initial_adult <- 100
  initial_spread <- 100
  
  # Use the initial state functions (assuming they are defined in your workspace)
  adult <- state_initialise_adult(num_stages, initial_adult)
  spread <- state_initialise_spread(num_stages, initial_spread)
  
  
  # Define the growth and reproduction matrices
  growth_matrix <- matrix(
    c(0.1, 0.0, 0.0, 0.0, 
      0.5, 0.4, 0.0, 0.0, 
      0.0, 0.4, 0.7, 0.0, 
      0.0, 0.0, 0.25, 0.4), 
    nrow = num_stages, ncol = num_stages, byrow = TRUE
  )
  
  reproduction_matrix <- matrix(
    c(0.0, 0.0, 0.0, 2.6, 
      0.0, 0.0, 0.0, 0.0, 
      0.0, 0.0, 0.0, 0.0, 
      0.0, 0.0, 0.0, 0.0), 
    nrow = num_stages, ncol = num_stages, byrow = TRUE
  )
  
  # Define the clutch distribution
  clutch_distribution <- c(0.06, 0.08, 0.13, 0.15, 0.16, 0.18, 0.15, 0.06, 0.03)
  
  # Run the stochastic simulations using the initial states
  line1 <- stochastic_simulation(adult, growth_matrix, reproduction_matrix, clutch_distribution, simul_length)
  line2 <- stochastic_simulation(spread, growth_matrix, reproduction_matrix, clutch_distribution, simul_length)
  
  
  # Plot the results and save as a PNG file
  matplot(
    cbind(line1, line2), type = "l", col = c("red", "blue"), lwd = 2, lty = 1,
    main = "Population Growth", xlab = "Generations", ylab = "Population"
  )
  png(filename = "question_2.png", width = 600, height = 400)
  dev.off()  # Close the graphics device
  
  return("Explain how stochasticity in recruitment and survival affects the population growth.")
}

  # Questions 3 and 4 involve writing code elsewhere to run your simulations on the cluster


# Question 5
question_5 <- function(){

#load in the rda files into a list of results so their indexes can be accessed


#use this convention to specify simulation conditions
  if (iter %% 4 == 0) {
    initial_state <- initial_states[[1]]  # Store the first simulation condition
  } else if (iter %% 4 == 1) {
    initial_state <- initial_states[[2]]  # Store the second simulation condition
  } else if (iter %% 4 == 2) {
    initial_state <- initial_states[[3]]  # Store the third simulation condition
  } else if (iter %% 4 == 3) {
    initial_state <- initial_states[[4]]  # Store the fourth simulation condition
  }
  
  png(filename="question_5", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()
  
  return("type your written answer here")
}

# Question 6
question_6 <- function(){
  
  png(filename="question_6", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()
  
  return("type your written answer here")
}


# Section Two: Individual-based ecological neutral theory simulation 

# Question 7
x <- c(1, 2, 2, 3, 3, 3)
species_richness <- function(community){
  s_richness <- length(unique(community))
  return(s_richness)
}

# Question 8
init_community_max <- function(size){
 max_size <- seq(1:size)
 return(max_size)
}

# Question 9
init_community_min <- function(size){
  min_size <- rep(1, size)
  return(min_size)
}

# Question 10
choose_two <- function(max_value){
  sample_vect <- seq(1:max_value)
  randomint<- sample(sample_vect, 2)
  return(randomint)
}


# Question 11
neutral_step <- function(community){
  random_value <- floor(runif(1, min(community), max(community) + 1))
  random_index <- sample(1:length(community), 1)
  if (random_value != community[random_index]) {
    community[random_index] <- random_value
  }
  return(community)
}

# Question 12
neutral_generation <- function(community){
  # Randomly select either 'floor' or 'ceiling'
  floor_or_ceiling <- sample(c(floor, ceiling), 1)[[1]]
  
  # Apply the chosen function to divide by 2
  generations <- floor_or_ceiling(length(community) / 2)
  
  for (i in 1:generations) {
    community <- neutral_step(community)
  }
  return(community)
}


# Question 13
neutral_time_series <- function(community,duration)  {
  
}

# Question 14
question_8 <- function() {
  
  
  
  png(filename="question_14", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()
  
  return("type your written answer here")
}

# Question 15
neutral_step_speciation <- function(community,speciation_rate)  {
  
}

# Question 16
neutral_generation_speciation <- function(community,speciation_rate)  {
  
}

# Question 17
neutral_time_series_speciation <- function(community,speciation_rate,duration)  {
  
}

# Question 18
question_18 <- function()  {
  
  png(filename="question_18", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()
  
  return("type your written answer here")
}

# Question 19
species_abundance <- function(community)  {
  
}

# Question 20
octaves <- function(abundance_vector) {
  
}

# Question 21
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
  return (x + y)
}

# Question 22
question_22 <- function() {
  
  png(filename="question_22", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()
  
  return("type your written answer here")
}

# Question 23
neutral_cluster_run <- function(speciation_rate, size, wall_time, interval_rich, interval_oct, burn_in_generations, output_file_name) {
    
}

# Questions 24 and 25 involve writing code elsewhere to run your simulations on
# the cluster

# Question 26 
process_neutral_cluster_results <- function() {
  
  
  combined_results <- list() #create your list output here to return
  # save results to an .rda file
  
}

plot_neutral_cluster_results <- function(){

    # load combined_results from your rda file
  
    png(filename="plot_neutral_cluster_results", width = 600, height = 400)
    # plot your graph here
    Sys.sleep(0.1)
    dev.off()
    
    return(combined_results)
}


# Challenge questions - these are substantially harder and worth fewer marks.
# I suggest you only attempt these if you've done all the main questions. 

# Challenge question A
Challenge_A <- function(){
  
  png(filename="Challenge_A", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()
  
}

# Challenge question B
Challenge_B <- function() {
  
  png(filename="Challenge_B", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()
  
}

# Challenge question C
Challenge_B <- function() {
  
  png(filename="Challenge_C", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()

}

# Challenge question D
Challenge_D <- function() {
  
  png(filename="Challenge_D", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()
}

# Challenge question E
Challenge_E <- function() {
  
  png(filename="Challenge_E", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()
  
  return("type your written answer here")
}

