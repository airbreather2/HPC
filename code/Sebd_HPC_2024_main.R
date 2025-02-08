# CMEE 2024 HPC exercises R code main pro forma
# You don't HAVE to use this but it will be very helpful.
# If you opt to write everything yourself from scratch please ensure you use
# EXACTLY the same function and parameter names and beware that you may lose
# marks if it doesn't work properly because of not using the pro-forma.

name <- "Sebastian Dohne"
preferred_name <- "Seb"
email <- "sed24@ic.ac.uk"
username <- "sed24"

# Please remember *not* to clear the work space here, or anywhere in this file.
# If you do, it'll wipe out your username information that you entered just
# above, and when you use this file as a 'toolbox' as intended it'll also wipe
# away everything you're doing outside of the toolbox.  For example, it would
# wipe away any automarking code that may be running and that would be annoying!

# Section One: Stochastic demographic population model


##### Modules
# Load necessary libraries
library(tidyverse)
library(fs)       # For working with files
library(purrr)    # For functional programming
library(dplyr)    # For filtering conditions

# Question 0

state_initialise_adult <- function(num_stages, initial_size) {
  adult <- rep(0, num_stages) #initialise adult vector
  adult[length(adult)] <- initial_size  # Use round brackets, not square brackets (assigns the initial size to the last index of adult vector)
  return(adult)
}


state_initialise_spread <- function(num_stages, initial_size) {
  # Create a vector with the base value distributed evenly
  even_spread <- rep(floor(initial_size / num_stages), num_stages) #assigns size of pop divided by number of stages across a vector length num of stages
  
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
  source("code/Demographic.R")
  
  
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
  
  # Save output as a PNG file
  png(filename = "results/question_1", width = 600, height = 400)
  
  matplot(cbind(line1, line2), type="l", col = c("red", "blue"), lwd=2, lty = 1, main="Deterministic Population growth", xlab="Generations", ylab="Population" )
  
  #add legend
  legend("topright", legend = c("Adult Population", "Spread Population"), 
         col = c("red", "blue"), lty = 1, lwd=2, cex=0.8)
  
  Sys.sleep(0.1)
  dev.off()
  
  return("A higher initial adult count leads to a rapid population increase in early generations due to more mature individuals contributing to reproduction. This effect is driven by the reproduction matrix, where only Stage 4 individuals produce offspring. Over time, the population growth rate depends on survival probabilities and transition rates between life stages.")
} # 
question_1()
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
  source("code/Demographic.R")
  
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
  png(filename = "results/question_2.png", width = 600, height = 400)
  
  matplot(
    cbind(line1, line2), type = "l", col = c("red", "blue"), lwd = 2, lty = 1,
    main = "Stochastic Population Growth", xlab = "Generations", ylab = "Population"
  )
  #add legend
  legend("topright", legend = c("Adult Population", "Spread Population"), 
         col = c("red", "blue"), lty = 1, lwd=2, cex=0.8)
  
  dev.off()  # Close the graphics device
  
  return("Stochasticity in recruitment and survival introduces variation in population growth, unlike deterministic models, which follow a fixed trajectory. In nature, environmental (abiotic) factors such as climate fluctuations and resource availability, as well as biological (biotic) interactions like predation and competition, cause unpredictable changes in survival and reproduction rates. This added variability leads to more realistic population dynamics, where population size fluctuates due to random effects rather than following a fixed pattern. ")
}
  question_2()

  # Questions 3 and 4 involve writing code elsewhere to run your simulations on the cluster

  
# Question 5

#check if rda files are there
  
  
  question_5 <- function() {
    
    rdafiles <- dir_ls("output", regexp = "\\.rda$")
    
    print(rdafiles)  # Debugging: Check if any files are found
    
    if (length(rdafiles) == 0) {
      stop("No .rda files found in output/ directory")
    }
    
    # 1. Define counters for each condition’s total extinctions and total runs
    LA_extinction_counter <- 0
    SA_extinction_counter <- 0
    LS_extinction_counter <- 0
    SS_extinction_counter <- 0
    LA_total_sims <- 0
    SA_total_sims <- 0
    LS_total_sims <- 0
    SS_total_sims <- 0
    
    # 2. Loop over every .rda file
    for (f in seq_along(rdafiles)) {
      file_path <- rdafiles[f]
      # Extract HPC job index from filename, e.g. "output_17.rda" => 17
      file_name <- basename(file_path)  # "output_17.rda"
      iter_str  <- sub("output_(\\d+)\\.rda", "\\1", file_name)
      job_idx   <- as.numeric(iter_str)
      
      # 3. Determine the condition name the same way the cluster script does
      mod_val <- job_idx %% 4
      if (mod_val == 1) {
        cond_name <- "large spread"
      } else if (mod_val == 2) {
        cond_name <- "small adult"
      } else if (mod_val == 3) {
        cond_name <- "small spread"
      } else {
        # mod_val == 0
        cond_name <- "large adult"
      }
      
      # Load the HPC object "simulation_results"
      load(file_path)
      if (!exists("simulation_results")) {
        stop("simulation_results not found in ", file_path)
      }
      
      # Count total sims in this file (should be 150)
      num_simulations <- length(simulation_results)
      # Determine how many ended in extinction (final pop == 0)
      extinct_vec <- sapply(simulation_results, function(x) tail(x, 1) == 0)
      n_extinct   <- sum(extinct_vec)
      
      # 4. Update counters for whichever condition
      if (cond_name == "large adult") {
        LA_extinction_counter <- LA_extinction_counter + n_extinct
        LA_total_sims         <- LA_total_sims + num_simulations
      } else if (cond_name == "small adult") {
        SA_extinction_counter <- SA_extinction_counter + n_extinct
        SA_total_sims         <- SA_total_sims + num_simulations
      } else if (cond_name == "large spread") {
        LS_extinction_counter <- LS_extinction_counter + n_extinct
        LS_total_sims         <- LS_total_sims + num_simulations
      } else if (cond_name == "small spread") {
        SS_extinction_counter <- SS_extinction_counter + n_extinct
        SS_total_sims         <- SS_total_sims + num_simulations
      }
    }
    
    # 5. Compute proportions for each condition
    large_adult_extinction  <- LA_extinction_counter / LA_total_sims
    small_adult_extinction  <- SA_extinction_counter / SA_total_sims
    large_spread_extinction <- LS_extinction_counter / LS_total_sims
    small_spread_extinction <- SS_extinction_counter / SS_total_sims
    
    # Print a summary
    cat("---- Extinction Counts ----\n")
    cat("Large Adult Extinctions:", LA_extinction_counter, "/", LA_total_sims, "\n")
    cat("Small Adult Extinctions:", SA_extinction_counter, "/", SA_total_sims, "\n")
    cat("Large Spread Extinctions:", LS_extinction_counter, "/", LS_total_sims, "\n")
    cat("Small Spread Extinctions:", SS_extinction_counter, "/", SS_total_sims, "\n\n")
    
    # 6. Create a barplot of all 4 proportions
    extinction_rates <- c(
      large_adult_extinction,
      small_adult_extinction,
      large_spread_extinction,
      small_spread_extinction
    )
    
    labels <- c("Large Adult", "Small Adult", "Large Spread", "Small Spread")
    
    cat("---- Extinction Proportions ----\n")
    print(extinction_rates)
    
    # Save a barplot
    png("results/question_5.png", width = 600, height = 400)
    barplot(
      extinction_rates,
      names.arg = labels,
      xlab = "Initial Condition",
      ylab = "Proportion of simulations that ended in extinction",
      main = "Extinction Probabilities by Initial Condition",
      col = "skyblue"
    )
    dev.off()
    
    print("Simulation analysis complete")
    return("Stochastic simulations with small initial population sizes are are more prone to extinction due to a higher vulnerability to variability within the stochastic model. Inital states containing only mature adults reach extinction in less cases due to an initial uptick in new juveniles in early stages reducing vulnerability to variation in population")
  }
  
  question_5()  
  


# Question 6
  question_6 <- function() {
    
    # -----------------------------
    # 1) FIND .RDA FILES
    # -----------------------------
    rda_files <- list.files(
      path = "output",         # directory to look in
      pattern = "\\.rda$",     # only .rda files
      full.names = TRUE        # include full path
    )
    
    if (length(rda_files) == 0) {
      stop("No .rda files found in 'output' directory.")
    }
    
    # -----------------------------
    # 2) INITIAL SETUP & MATRICES
    # -----------------------------
    num_stages   <- 4
    simul_length <- 120
    
    # HPC code sets large_spread=100, small_spread=10
    large_initial_spread <- 100
    small_initial_spread <- 10
    
    # Spread these totals among 4 stages
    large_spread_state <- state_initialise_spread(num_stages, large_initial_spread)
    small_spread_state <- state_initialise_spread(num_stages, small_initial_spread)
    
    growth_matrix <- matrix(
      c(0.1, 0.0, 0.0, 0.0, 
        0.5, 0.4, 0.0, 0.0, 
        0.0, 0.4, 0.7, 0.0, 
        0.0, 0.0, 0.25, 0.4),
      nrow = num_stages, byrow = TRUE
    )
    
    reproduction_matrix <- matrix(
      c(0.0, 0.0, 0.0, 2.6,
        0.0, 0.0, 0.0, 0.0, 
        0.0, 0.0, 0.0, 0.0, 
        0.0, 0.0, 0.0, 0.0),
      nrow = num_stages, byrow = TRUE
    )
    
    projection_matrix <- reproduction_matrix + growth_matrix
    
    # -----------------------------
    # 3) DETERMINISTIC SIMULATIONS
    # -----------------------------
    # deterministic_simulation() returns a length-121 numeric vector (time=0..120)
    
    det_large_spread <- deterministic_simulation(large_spread_state, projection_matrix, simul_length)
    det_small_spread <- deterministic_simulation(small_spread_state, projection_matrix, simul_length)
    
    # -----------------------------
    # 4) ACCUMULATORS FOR STOCHASTIC
    # -----------------------------
    total_sims_sum_large <- rep(0, simul_length + 1)  # length 121
    total_sims_sum_small <- rep(0, simul_length + 1)
    
    count_large <- 0
    count_small <- 0
    
    # -----------------------------
    # 5) LOOP OVER .rda FILES
    # -----------------------------
    for (f in seq_along(rda_files)) {
      
      file_path <- rda_files[f]
      # Load simulation_results (list of numeric vectors, each length ~121)
      load(file_path)  
      
      # Extract HPC job index, e.g. "output_17.rda" => 17
      file_name <- basename(file_path)
      iter_str  <- sub("output_(\\d+)\\.rda", "\\1", file_name)
      job_idx   <- as.numeric(iter_str)
      
      # HPC logic: 0=large_adult, 1=large_spread, 2=small_adult, 3=small_spread
      mod_val <- job_idx %% 4
      if (mod_val == 1) {
        cond_name <- "large spread"
      } else if (mod_val == 3) {
        cond_name <- "small spread"
      } else {
        # skip large_adult(0) & small_adult(2)
        next
      }
      
      # simulation_results is a list of length 150 (by default),
      # each element is a numeric vector of length 121
      for (i in seq_along(simulation_results)) {
        sim_vec <- simulation_results[[i]]
        # keep all 121 points:
        
        if (cond_name == "large spread") {
          total_sims_sum_large <- total_sims_sum_large + sim_vec
          count_large <- count_large + 1
        } else {
          # small spread
          total_sims_sum_small <- total_sims_sum_small + sim_vec
          count_small <- count_small + 1
        }
      }
    }
    
    # -----------------------------
    # 6) AVERAGE STOCHASTIC TRENDS
    # -----------------------------
    mean_large_spread <- total_sims_sum_large / max(count_large, 1)
    mean_small_spread <- total_sims_sum_small / max(count_small, 1)
    
    # -----------------------------
    # 7) COMPUTE DEVIATION
    # -----------------------------
    # Ratio of mean stochastic to deterministic, each length 121
    
    deviation_large <- mean_large_spread / det_large_spread
    deviation_small <- mean_small_spread / det_small_spread
    
    # -----------------------------
    # 8) MAKE THE PLOT
    # -----------------------------
    png("results/question_6.png", width = 600, height = 400)
    
    # Time axis = 0..120
    time_axis <- 0:simul_length
    
    # Set up a 1-row, 2-column layout to produce two separate panels
    par(mfrow = c(1, 2))
    
    # Panel 1: Large spread
    plot(
      time_axis, deviation_large, type = "l", col = "blue",
      ylim = c(0.95, max(deviation_large, na.rm = TRUE) * 1.03),
      xlab = "Time step", ylab = "Deviation (Stochastic / Deterministic)",
      main = "Deviation: Large Spread"
    )
    abline(h = 1, col = "darkgray", lty = 2)  # reference line at 1
    
    # Panel 2: Small spread
    plot(
      time_axis, deviation_small, type = "l", col = "red",
      ylim = c(0.95, max(deviation_small, na.rm = TRUE) * 1.03),
      xlab = "Time step", ylab = "Deviation (Stochastic / Deterministic)",
      main = "Deviation: Small Spread"
    )
    abline(h = 1, col = "darkgray", lty = 2)
    
    dev.off()
    
    # -----------------------------
    # 9) RETURN WRITTEN ANSWER
    # -----------------------------
    return(
      paste(
        "For the large-spread initial condition, the deterministic model",
        "more closely approximates the average stochastic trend. When the",
        "initial population is large, random variation has a smaller relative",
        "impact, so the mean stochastic trajectory remains closer to deterministic in comparison to the small-spread initial condition"
      )
    )
  }
  
  # Call the function
  question_6()
  

# Section Two: Individual-based ecological neutral theory simulation 

# Question 7: Find species richness in vector: 
  #example: 
  #input:species_richness(c(1,4,4,5,1,6,1))
  #output: returns 4 
  
species_richness <- function(community){  #requires vector
  s_richness <- length(unique(community)) #find most common value within vector
  return(s_richness)
}

# Question 8: generate vector of max community size.
# Example:
# Input: 5
# Expected Output: 1 2 3 4 5

init_community_max <- function(size) {
  # This function initializes a community of the specified size
  # with the maximum possible number of distinct species.
  # Each individual in the community is assigned a unique species ID numerically.
  
  # Generate a sequence from 1 up to 'size'
  max_size <- seq(1:size)
  
  # Return this sequence as the community
  return(max_size)
}

# Question 9: initialise community with monodominance of species
# Example usage:
# Input: 5
# Expected Output: 1 1 1 1 1
init_community_min <- function(size) {
  # This function initializes a community of the specified size
  # where all individuals belong to the same species (monodominance).
  
  # Create a vector of length 'size' where all elements are species 1
  min_size <- rep(1, size)
  
  # Return the community
  return(min_size)
}


# Question 10: Choose 2 elements in a vector
# Example usage:
# Possible outputs for choose_two(4): c(3, 4), c(1, 2), c(2, 4), etc.

choose_two <- function(max_value) {
  # This function selects two distinct random integers from 1 to max_value.
  # The numbers are chosen with equal probability.
  
  # Generate a random sample of 2 unique numbers between 1 and max_value
  random_int <- sample(1:max_value, 2) 
  
  # Return the selected numbers as a vector
  return(random_int)
}


# Question 11: Neutral Step: 
# Example usage:
# If community = c(1, 2), neutral_step(c(1,2)) could return c(1,1) or c(2,2)

neutral_step <- function(community) {
  # This function performs one step of a neutral model simulation.
  # It selects one individual to die and another to reproduce.
  
  # Choose two distinct indices in the community
  indices <- choose_two(length(community))
  
  # One individual (first in indices) dies and is replaced by the species of the second
  community[indices[1]] <- community[indices[2]]
  
  # Return the updated community
  return(community)
}


# Question 12
#Example: simulate a generation in a community by performing x/2 neutral steps
# If community = c(1, 2, 3, 4), this will perform 2 neutral steps (either floor(4/2) = 2 or ceiling(4/2) = 2)
neutral_generation <- function(community) {
  # If there's only one individual, nothing changes
  if (length(community) == 1) {
    return(community)
  }
  
  # Determine the number of neutral steps for one generation
  generations <- sample(c(floor(length(community) / 2), ceiling(length(community) / 2)), 1)
  
  # Perform the determined number of neutral steps
  for (i in 1:generations) {
    community <- neutral_step(community)
  }
  
  # Return the updated community after one generation
  return(community)
}


# Question 13: Time Series of Species Richness
# Example usage:
# If community = c(1,2,3,4,5,6), neutral_time_series(community, 10) 
# could return c(6,6,5,5,4,4,4,3,3,2,2)

neutral_time_series <- function(community, duration) {
  # Initialize species richness list
  richness_vector <- numeric(duration + 1)  
  richness_vector[1] <- species_richness(community)  
  
  for (i in 2:(duration + 1)) {  
    community <- neutral_generation(community)  
    richness_vector[i] <- species_richness(community)  
  }
  
  return(richness_vector)  
}


# Question 14: Species Richness Over Time
# Example usage:
# question_14() will save "results/question_14.png" and return final richness vector.

species_richness_over_time <- function() {
  
  # Initialize community with maximal diversity (100 species)
  communityfortimeseries <- init_community_max(100) 
  
  # Run simulation for 200 generations
  simulation_data <- neutral_time_series(communityfortimeseries, 200)
  
  # Generate sequence for x-axis (generations)
  generations <- seq_along(simulation_data)  
  
  # Save plot as PNG
  png(filename = "results/question_14.png", width = 600, height = 400)
  
  plot(generations, simulation_data, type = "o", col = "blue", 
       xlab = "Generations", ylab = "Species Richness", 
       main = "Species Richness Over Time",  
       ylim = c(0, 100))  
  
  dev.off()  # Close the graphics device
  
  # Return the required answer for Question 14
  return("Over time, the system will converge to a single species (monodominance) due to the absence of speciation. 
  This occurs because, in a neutral model without speciation, random extinction events eventually eliminate all but one species.")
}

species_richness_over_time()

# Question 15: Neutral Step with Speciation
# Example usage:
# If community = c(1,1,2), neutral_step_speciation(c(1,1,2), 0.2) 
# behaves like neutral_step 80% of the time, and introduces a new species 20% of the time.

neutral_step_speciation <- function(community, speciation_rate) {
  indices <- choose_two(length(community))
  
  if (runif(1) < speciation_rate) {
    # Generate a unique species ID
    existing_species <- unique(community)
    new_species <- max(existing_species) + 1  # Ensures uniqueness
    community[indices[1]] <- new_species
  } else {
    community[indices[1]] <- community[indices[2]]
  }
  
  return(community)
}

# Question 16: Neutral Generation with Speciation
# Example usage:
# neutral_generation_speciation(c(1,1,2,2,3,3), 0.1)

neutral_generation_speciation <- function(community, speciation_rate) {
  
  # If there's only one individual, nothing changes
  if (length(community) == 1) {
    return(community)
  }
  
  # Determine the number of neutral steps for one generation
  num_steps <- sample(c(floor(length(community) / 2), ceiling(length(community) / 2)), 1)
  
  # Perform the determined number of neutral steps
  for (i in 1:num_steps) {
    community <- neutral_step_speciation(community, speciation_rate)
  }
  
  return(community)
}

# Question 17: Time Series of Species Richness with Speciation
# Example usage:
# neutral_time_series_speciation(c(1,2,3,4,5), 0.1, 10)

neutral_time_series_speciation <- function(community, speciation_rate, duration) {
  # Initialize species richness list
  richness_vector <- numeric(duration + 1)
  richness_vector[1] <- species_richness(community)  # Store initial richness
  
  # Simulate over generations
  for (gen in 2:(duration + 1)) {  
    community <- neutral_generation_speciation(community, speciation_rate)  
    richness_vector[gen] <- species_richness(community)  
  }
  
  return(richness_vector)  
}

# Question 18: Species Richness Over Time with Speciation
# Example usage:
# question_18() saves "results/question_18.png" and returns a written explanation.

question_18 <- function()  {
  
  # Initialize community with maximal diversity (100 species)
  communityfortimeseriesmax <- init_community_max(100) 
  
  # Initialize community with minimal diversity (1 species)
  communityfortimeseriesmin <- init_community_min(100) 
  
  # Run simulation for 200 generations
  simulation_data1 <- neutral_time_series_speciation(communityfortimeseriesmax, 0.1, 200)
  simulation_data2 <- neutral_time_series_speciation(communityfortimeseriesmin, 0.1, 200)
  
  # Generate sequence for x-axis (generations)
  generations <- seq_along(simulation_data1)
  
  # Save plot as PNG
  png(filename = "results/question_18.png", width = 600, height = 400)
  
  plot(generations, simulation_data1, type = "o", col = "blue", 
       xlab = "Generations", ylab = "Species Richness", 
       main = "Species Richness Over Time in Speciation Model",  
       ylim = c(0, 100)) 
  
  lines(generations, simulation_data2, type = "o", col = "red")
  
  # Add a legend
  legend("topright", legend = c("Max community richness", "Min community richness"), 
         col = c("blue", "red"), lty = 1, pch = 1)
  
  dev.off()  # Close the graphics device
  
  return("Both initial conditions converge toward a similar species richness over time due to speciation balancing diversity loss from extinction. 
  The initially diverse community loses species quickly, while the monodominant community gains diversity gradually. 
  This confirms that long-term species richness is determined more by speciation rate than initial diversity.")
}

# Run and save plot
question_18()


# Question 19: Species Abundance Distribution
# Example usage:
# species_abundance(c(1,1,2,2,2,3,3,3,3,4))
# Expected output: A table with species sorted by abundance (most to least)

species_abundance <- function(community)  {
  # Count the number of times each species appears in the community
  species_counts <- table(community)
  
  # Sort species from most to least abundant
  abundance_ordered <- sort(species_counts, decreasing = TRUE)
  
  # Return the sorted species abundance table
  return(abundance_ordered)
}


# Question 20: Convert species abundance into octave bins
# Example usage:
# octaves(c(1, 2, 3, 4, 5, 8, 9, 12, 15, 20))
# Expected output: A vector representing how many species fall into each octave bin.

octaves <- function(species_abundance_vector) {
  if (length(species_abundance_vector) == 0) {
    return(integer(0))  # Return empty integer vector if there are no species
  }
  
  # Compute octave bins using powers of 2
  octave_bins <- floor(log2(species_abundance_vector)) + 1  
  octave_distribution <- tabulate(octave_bins)  # Counts occurrences in each bin
  
  return(octave_distribution)
}


# Question 21: Summing Two Vectors After Padding with Zeros
# Example usage:
# sum_vect(c(1,3), c(1,0,5,2)) 
# Expected output: c(2,3,5,2)

sum_vect <- function(x, y) {
  # Pad the shorter vector with zeros to match the longer vector's length
  len_x <- length(x)
  len_y <- length(y)
  
  if (len_x > len_y) {
    y <- c(y, rep(0, len_x - len_y))  # Pad y with zeros
  } else if (len_y > len_x) {
    x <- c(x, rep(0, len_y - len_x))  # Pad x with zeros
  }
  
  # Return sum of both vectors
  return(x + y)
}


# Question 22: Mean Species Abundance Distribution After Burn-in
# Runs two neutral model simulations, records species abundance octaves, and saves bar plots.  DOESN'T WORK

question_22 <- function() {
  
  # -----------------------------
  # 1. SIMULATION SETUP
  # -----------------------------
  
  # Define parameters
  community_max <- init_community_max(100)  # Max initial richness
  community_min <- init_community_min(100)  # Min initial richness
  speciation_rate <- 0.1
  burn_in_generations <- 200
  total_generations <- 2000
  record_every <- 20  # Record every 20 generations
  
  # Burn-in phase (run for 200 generations)
  for (i in 1:burn_in_generations) {
    community_max <- neutral_generation_speciation(community_max, speciation_rate)
    community_min <- neutral_generation_speciation(community_min, speciation_rate)
  }
  
  # Initialize storage for summed octave distributions
  total_octaves_max <- NULL
  total_octaves_min <- NULL
  num_samples <- 0  # Counter for averaging later
  
  # -----------------------------
  # 2. SIMULATION LOOP (2000 generations, record every 20)
  # -----------------------------
  
  for (gen in seq(1, total_generations, by = record_every)) {
    for (j in 1:record_every) {
      community_max <- neutral_generation_speciation(community_max, speciation_rate)
      community_min <- neutral_generation_speciation(community_min, speciation_rate)
    }
    
    # Record species abundance octaves
    octaves_max <- octaves(species_abundance(community_max))
    octaves_min <- octaves(species_abundance(community_min))
    
    # Sum recorded octaves over multiple samples using sum_vect()
    total_octaves_max <- if (is.null(total_octaves_max)) octaves_max else sum_vect(total_octaves_max, octaves_max)
    total_octaves_min <- if (is.null(total_octaves_min)) octaves_min else sum_vect(total_octaves_min, octaves_min)
    
    num_samples <- num_samples + 1
  }
  
  # -----------------------------
  # 3. COMPUTE MEAN SPECIES ABUNDANCE
  # -----------------------------
  
  mean_octaves_max <- total_octaves_max / num_samples
  mean_octaves_min <- total_octaves_min / num_samples
  
  # -----------------------------
  # 4. GENERATE AND SAVE PLOTS
  # -----------------------------
  
  png(filename = "results/question_22.png", width = 800, height = 400)
  par(mfrow = c(1, 2))  # Two-panel plot
  
  # Panel 1: Max initial richness
  barplot(mean_octaves_max, main = "Mean Species Abundance (Max Initial Richness)",
          xlab = "Octave Bins", ylab = "Mean Abundance", col = "blue", names.arg = seq_along(mean_octaves_max))
  
  # Panel 2: Min initial richness
  barplot(mean_octaves_min, main = "Mean Species Abundance (Min Initial Richness)",
          xlab = "Octave Bins", ylab = "Mean Abundance", col = "red", names.arg = seq_along(mean_octaves_min))
  
  dev.off()
  
  # -----------------------------
  # 5. RETURN WRITTEN ANSWER
  # -----------------------------
  
  return("The initial condition of the system does not strongly affect long-term species abundance distributions. 
  After the burn-in period, the mean species abundance distribution converges to a similar shape regardless of whether the system started with high or low initial species richness. 
  This suggests that speciation and stochastic dynamics dominate in determining species diversity over time, rather than the starting community structure.")
}

# Run function
question_22()




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

