rm(list = ls())
graphics.off()
source("Demographic.R")

  #read in job from job cluster
     
  iter <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX")) # Find out the job number
  #set seed as iter

  #iter <- 4
  
set.seed(iter) # Set this as the random seed so that all runs have a unique seed
  
  
  #cluster variable definitions
  cluster_run <- 100 
  cluster_simulations <- 150 
  
  # Define input variables
  num_stages <- 4
  simul_length <- 120
  large_adult <- 100
  large_spread <- 100
  small_adult <- 10
  small_spread <- 10
  
  
# Use the initial state functions (assuming they are defined in your workspace)
# Create a list with the initialized states
initial_states <- list(
  large_adult = state_initialise_adult(num_stages, large_adult),
  large_spread = state_initialise_spread(num_stages, large_spread),
  small_adult = state_initialise_adult(num_stages, small_adult),
  small_spread = state_initialise_spread(num_stages, small_spread)
)

# Print the list to check the results
print(initial_states)

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
  
  
  #large_adult_simulation <- stochastic_simulation(large_adult, growth_matrix, reproduction_matrix, clutch_distribution, simul_length)
  #small_adult_simulation <- stochastic_simulation(small_adult, growth_matrix, reproduction_matrix, clutch_distribution, simul_length)
  #large_spread_simulation <- stochastic_simulation(large_spread, growth_matrix, reproduction_matrix, clutch_distribution, simul_length)
  #small_spread_simulation <- stochastic_simulation(small_spread, growth_matrix, reproduction_matrix, clutch_distribution, simul_length)
  
  
  # Use the modulo result to select from initial_states based on iter value
  
  if (iter %% 4 == 0) {
    initial_state <- initial_states[[1]]  # Store the first simulation condition
  } else if (iter %% 4 == 1) {
    initial_state <- initial_states[[2]]  # Store the second simulation condition
  } else if (iter %% 4 == 2) {
    initial_state <- initial_states[[3]]  # Store the third simulation condition
  } else if (iter %% 4 == 3) {
    initial_state <- initial_states[[4]]  # Store the fourth simulation condition
  }



  # create list to store results of the simulation
  simulation_results <- vector("list", cluster_simulations)
  
  # run through all simulations
  for (i in 1:cluster_simulations) {
    simulation_results[[i]] <- stochastic_simulation(
      initial_state, 
      growth_matrix, 
      reproduction_matrix, 
      clutch_distribution, 
      simul_length)
  }

print("done!")

#save simulation results
save(simulation_results, file=paste("output_",iter,".rda",sep=""))

