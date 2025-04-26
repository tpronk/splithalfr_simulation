# *** Libraries
library(splithalfr)
library(MASS)
library(boot)
library(bcaboot)
library(parallel)

# Run a simulation with given n_participants
run_simulation = function(n_participants, sd, n_bootstraps = 100) {
  result = NULL
  
  ds = data.frame(
    participant = 1 : n_participants,
    score = rnorm(n = n_participants, sd = sd )
  )
  
  statistic = function (ds) {
    return (mean(ds$score))
  }
  
  # CI estimated from sample via linear model (i.e., analytically)
  sample_mean = mean(ds$score)
  sample_se = sd(ds$score) / sqrt(n_participants)
  sample_t = qt(0.975, df = n_participants - 1)
  
  result = rbind(
    data.frame(
      n_participants = n_participants, 
      sd = sd, 
      n_bootstraps = n_bootstraps,
      method = "analytic",
      metric = "ci_left",
      value = sample_mean - sample_se * sample_t
    ),
    data.frame(
      n_participants = n_participants, 
      sd = sd, 
      n_bootstraps = n_bootstraps,
      method = "analytic",
      metric = "ci_right",
      value = sample_mean + sample_se * sample_t
    )
  )
    
  
  # CI estimated via bcaboot (only single core available)
  timing_bcaboot <- system.time(
    result_bcaboot <- bcajack(
      x = ds,
      B = n_bootstraps,
      func = statistic,
      alpha = 0.025
    )
  )
  result = rbind(result,
    data.frame(
      n_participants = n_participants, 
      sd = sd, 
      n_bootstraps = n_bootstraps,
      method = "bcaboot",
      metric = "ci_left",
      value = result_bcaboot$lims[1,"bca"]
    ),
    data.frame(
      n_participants = n_participants, 
      sd = sd, 
      n_bootstraps = n_bootstraps,
      method = "bcaboot",
      metric = "ci_right",
      value = result_bcaboot$lims[3,"bca"]
    ),
    data.frame(
      n_participants = n_participants, 
      sd = sd, 
      n_bootstraps = n_bootstraps,
      method = "bcaboot",
      metric = "time",
      value = as.numeric(timing_bcaboot["user.self"])
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

  result = rbind(result,
    data.frame(
      n_participants = n_participants, 
      sd = sd, 
      n_bootstraps = n_bootstraps,
      method = "boot_sc",
      metric = "ci_left",
      value = boot.ci(result_sc_boot, type="bca")$bca[4]
    ),
    data.frame(
      n_participants = n_participants, 
      sd = sd, 
      n_bootstraps = n_bootstraps,
      method = "boot_sc",
      metric = "ci_right",
      value = boot.ci(result_sc_boot, type="bca")$bca[5]
    ),
    data.frame(
      n_participants = n_participants, 
      sd = sd, 
      n_bootstraps = n_bootstraps,
      method = "boot_sc",
      metric = "time",
      value = as.numeric(timing_sc_boot["user.self"])
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
  
  result = rbind(result,
    data.frame(
      n_participants = n_participants, 
      sd = sd, 
      n_bootstraps = n_bootstraps,
      method = "boot_mc",
      metric = "ci_left",
      value = boot.ci(result_mc_boot, type="bca")$bca[4]
    ),
    data.frame(
      n_participants = n_participants, 
      sd = sd, 
      n_bootstraps = n_bootstraps,
      method = "boot_mc",
      metric = "ci_right",
      value = boot.ci(result_mc_boot, type="bca")$bca[5]
    ),
    data.frame(
      n_participants = n_participants, 
      sd = sd, 
      n_bootstraps = n_bootstraps,
      method = "boot_mc",
      metric = "time",
      value = as.numeric(timing_mc_boot["user.self"])
    )
  )  
  return (result)
}

# Run simulation
results = NULL
v_n_participants = c(10, 20, 40, 80)
v_sd             = c(10, 20, 40, 80)

for (j in 1 : length(v_sd)) {
  for (i in 1 : length(v_n_participants)) {
    print(paste(
      "Running simulation with n_participants =", 
      v_n_participants[i], 
      "and sd =",
      v_sd[j]
    ))
    result = run_simulation(v_n_participants[i], v_sd[j])
    if (is.null(results)) {
      results = result
    } else {
      results = rbind(results, result)
    }
  }
}

library(ggplot2)

# Plot of confidence intervals
results$sd = as.factor(results$sd)
results$method = as.factor(results$method)
results$metric = as.factor(results$metric)
ggplot(data = results[results$metric %in% c("ci_left", "ci_right"),], 
    aes(x = n_participants, y = value, color = method, linetype = metric)
  ) +
  geom_line(linewidth = 1.2) + 
  facet_grid(rows = vars(sd), scales = "free")

# Plot of performance
ggplot(data = results[results$metric %in% c("time"),], 
       aes(x = n_participants, y = value, color = method)
) +
  geom_line(linewidth = 1.2) + 
  facet_grid(rows = vars(sd), scales = "free")


print(results)
