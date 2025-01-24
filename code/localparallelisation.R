#Local paralellisation
#run tasks in parallel for greater speed/efficiency
n <- 10000 #number of observations


data <- data.frame(
  ID = sample(1:10, n, replace = TRUE), # ID column to define 10 groups
  Y = rnorm(n), #creates a vector of 10000 random values from normal standard dist (mean=0,sd=1)
  x = rnorm(n)
)

#split data into a list of datasets. We can use list apply and define out funcvtion for outputting the eoefficients for the corresponding ID group:

library(dplyr)

data_groups <- data %>% group_by(ID) %>% group_split() # Split the data by ID

# Define a function to fit a linear model for each group
fit_model <- function(group_data) {
  # Fit a linear model (lm) to the data in group_data, predicting Y using x
  model <- lm(Y ~ x, data = group_data)
  
  # Extract the coefficients from the model and transpose them to create a one-row data frame
  coef_df <- as.data.frame(t(coef(model)))
  
  # Add a column to the coefficients data frame with the unique ID from the group_data for reference
  coef_df$ID <- unique(group_data$ID)
  
  # Return the data frame containing the model coefficients and the ID
  return(coef_df)
}

# Now, run the parallel code to fit models for each group
library(parallel)

num_cores <- detectCores() - 1  # Use all cores but one

# Use mclapply to run the fit_model function on each subset of the data
results <- mclapply(data_groups, fit_model, mc.cores = num_cores)

# Bind all the results into one data frame
final_results <- bind_rows(results)
print(final_results)

