# Made by Thomas Pronk, pronkthomas@gmail.com, 2019-05-24

# This simulation generates scores from a single factor model with equal factor loadings
# with different levels of reliability. Next, mean Flanagon-Rulon reliabilies and Cronbach's
# alphas are calculated of these scores, and compared to eachother.
# For more information, see the vignette "about_splithalfr" included in the splithalfr package.

library(splithalfr)
library(psych)
library(dplyr)

# For reproducibility, set the random to The Answer to Life the Universe and Everything
set.seed(42)

# splithalfr sets function; extracts "answer" column from data and returns it as the "answer" element of a list
sim_fn_sets = function (ds) {
  return (list(
    answers = ds$answer
  ))
}
# splithalfr score function; calculates mean of answers
sim_fn_score  <- function (sets) {
  return (mean(sets$answers))
}

# Runs simulation for a given noise level
run_simulation <- function (
  test_noise,
  item_count = 50,
  participant_count = 1000
) {
  # Generates normally distributed answers for item_count items and participant_count
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
  means <- sh_apply(
    ds,
    "participant",
    sim_fn_sets,
    sim_fn_score
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
  splithalfs <- sh_apply(
    ds,
    "participant",
    sim_fn_sets,
    sim_fn_score,
    split_count = 1000
  )
  # Mean Flanagon-Rulon reliaibily of split scores
  r_splithalf <- mean_fr_by_split(splithalfs)
  return(list(
    cronbachs_alpha = cronbachs_alpha,
    r_splithalf = r_splithalf
  ))
}

# Runs a set of simulations with varying levels of test_noise
ds_sim <- data.frame(
  test_noise = numeric(),
  cronbachs_alpha = numeric(),
  r_splithalf = numeric()
)
for (test_noise in seq(1, 9, by = 1)) {
  print(paste("test_noise:", test_noise))
  sim_result <- run_simulation(test_noise)
  ds_sim[nrow(ds_sim) + 1, ] <- c(
    test_noise,
    unlist(sim_result)
  )
}
# Calculates difference between Cronbach's alpha and mean Flanagon-Rulon reliability
ds_sim$difference = ds_sim$cronbachs_alpha - ds_sim$r_splithalf
# Print simulation results
print(ds_sim)
