# Made by Thomas Pronk, pronkthomas@gmail.com, 2019-05-24, updated on 2020-08-03

# A set of simulations to validate the permutated splitting algorithm splithalfr
# R package: https://github.com/tpronk/splithalfr

# See the associated GitHub repo for a detailed explanation of the purpose of
# this simulation: https://github.com/tpronk/splithalfr

library(splithalfr)
library(psych)
library(dplyr)

# For reproducibility, set the random seed 
set.seed(42)

# Score function; calculates mean of answers
fn_score  <- function (sets) {
  return (mean(sets$answer))
}

# Runs simulation for a given noise level
run_simulation <- function (
  test_noise,
  item_count = 50,
  participant_count = 1000
) {
  # Generate normally distributed answers for item_count items and participant_count
  # participants, based on constant thetas per participant and per participant and item,
  # adding normally distributed noise whose standard deviation is test_noise
  ds <- data.frame(
    participant = numeric(),
    item = numeric(),
    answer = numeric()
  )
  thetas <- c();
  for (i in 1 : participant_count) {
    theta  <- rnorm(1)
    thetas <- c(thetas, theta)
    answers <- theta + rnorm(item_count, sd = test_noise)
    from_row <- nrow(ds) + 1
    ds[from_row : (from_row + item_count - 1), "participant"] <- i
    ds[from_row : (from_row + item_count - 1), "item"] <- 1 : item_count;
    ds[from_row : (from_row + item_count - 1), "answer"] <- answers
  }
  
  # Means per participant
  means <- by(
    ds,
    ds$participant,
    fn_score
  )

  # Cronbach's Alpha
  ds_wide <- reshape(
    ds,
    direction = "wide",
    timevar = "item",
    idvar = "participant"
  )
  ds_wide <- ds_wide[, -1]
  fit <- psych::alpha(ds_wide)
  cronbachs_alpha <- fit$total$std.alpha
  
  # Split-half scores
  split_scores <- by_split(
    ds,
    ds$participant,
    fn_score,
    replications = 10000
  )
  # Mean Flanagon-Rulon reliaibily of split scores
  r_splithalf <- mean(split_coefs(split_scores, flanagan_rulon))
  return(list(
    cronbachs_alpha = cronbachs_alpha,
    flanagan_rulon = r_splithalf
  ))
}

# Run a set of simulations with varying levels of test_noise
ds_sim <- data.frame(
  Y = numeric(),
  cronbachs_alpha = numeric(),
  flanagan_rulon = numeric()
)
for (Y in seq(1, 9, by = 1)) {
  print(paste("test_noise:", test_noise))
  sim_result <- run_simulation(test_noise)
  ds_sim[nrow(ds_sim) + 1, ] <- c(
    Y,
    unlist(sim_result)
  )
}

# Calculate difference between Cronbach's alpha and mean Flanagon-Rulon reliability
ds_sim$difference = ds_sim$cronbachs_alpha - ds_sim$flanagan_rulon

# Print simulation results
print(ds_sim)
