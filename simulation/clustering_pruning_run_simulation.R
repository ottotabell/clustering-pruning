# Running the simulation

args <- commandArgs(trailingOnly = TRUE)
task_id <- as.integer(args[1])

# If clustering & pruning
setwd("~/simulation")
source("05_simulation.R")

set.seed(20260706 + task_id)

sim_result <- simulate_times(400, clustering = T, prune = T, min_n = 5, max_n = 8, max_r = 2, max_e = 2, max_o = 1, timeout = 0.25,
                             output_file = sprintf("/scratch/jkarvane/clustering-pruning/simulation_results_%03d.rds", task_id),
                             save_every = 10)

saveRDS(sim_result, file = sprintf("/scratch/jkarvane/clustering-pruning/simulation_results_%03d.rds", task_id))

print(paste("Task", task_id, "completed!"))