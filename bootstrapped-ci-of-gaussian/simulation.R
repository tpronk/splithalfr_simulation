# *** Libraries
library(splithalfr)
library(MASS)
library(boot)
library(bcaboot)
library(parallel)

# Run a simulation with given n_participants
run_simulation = function(n_participants, sd, n_bootstraps = 10000) {
  ds = data.frame(
    participant = 1 : n_participants,
    score = rnorm(n = n_participants, sd = sd )
  )
  
  statistic = function (ds) {
    return (mean(ds$score))
  }
  
  # CI estimated from sample via linear model
  sample_mean = mean(ds$score)
  sample_se = sd(ds$score) / sqrt(n_participants)
  sample_t = qt(0.975, df = n_participants - 1)
  
  # CI estimated via bcaboot (only single core available)
  timing_bcaboot <- system.time(
    result_bcaboot <- bcajack(
      x = ds,
      B = n_bootstraps,
      func = statistic,
      alpha = 0.025
    )
  )

  # CI estimated via boot running on a single core
  timing_sc_boot <- system.time(
    result_sc_boot <- boot(
      data = ds,
      statistic =function(ds_original, indexes) {
        return (statistic(ds_original[indexes,]))
      },
      R = n_bootstraps
    )
  )

  # CI estimated via boot running on multiple cores
  timing_mc_boot <- system.time(
    result_mc_boot <- boot(
      data = ds,
      statistic =function(ds_original, indexes) {
        return (statistic(ds_original[indexes,]))
      },
      R = n_bootstraps,
      parallel="snow", 
      ncpus = detectCores()
    )
  )

  # Illustration of variable names using boot_sc_ci_l:
  # boot package (boot), single core (SC), confidence interval (ci), left (l)
  return (data.frame(
    n_participants = n_participants, 
    sd = sd, 
    n_bootstraps = n_bootstraps,
    linear_ci_l  = sample_mean - sample_se * sample_t,
    bcaboot_ci_l = result_bcaboot$lims[1,"bca"],
    boot_sc_ci_l = boot.ci(result_sc_boot, type="bca")$bca[4],
    boot_mc_ci_l = boot.ci(result_mc_boot, type="bca")$bca[4],
    linear_ci_r  = sample_mean + sample_se * sample_t,
    bcaboot_ci_r = result_bcaboot$lims[3,"bca"],
    boot_sc_ci_r = boot.ci(result_sc_boot, type="bca")$bca[5],
    boot_mc_ci_r = boot.ci(result_mc_boot, type="bca")$bca[5],
    bcaboot_time = as.numeric(timing_bcaboot["user.self"]),
    boot_sc_time = as.numeric(timing_sc_boot["user.self"]),
    boot_mc_time = as.numeric(timing_mc_boot["user.self"])
  ))
}

results = NULL
v_n_participants = c(20, 100, 20, 100)
v_sd             = c( 1,   1, 10,  10)

for (i in 1 : length(v_n_participants)) {
  print(paste(
    "Running simulation with n_participants =", 
    v_n_participants[i], 
    "and sd =",
    v_sd[i]
  ))
  result = run_simulation(v_n_participants[i], v_sd[i])
  if (is.null(results)) {
    results = result
  } else {
    results = rbind(results, result)
  }
}

print(results)
