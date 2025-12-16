# R code for testing the simulations

source("05_simulation.R")

# Smaller simulation with small graphs. Should take a few minutes.
# n = 500; min_n = 5; max_n = 5; max_r = 2; max_e = 2; max_o = 1
# min_sources = 2; max_sources = 4
# prob1 = 0.5; prob2 = 0.35; prob3 = 0.5; prob_sources = c(0.35, 0.125, 0.125, 0.40)

sim_result <- simulate_times(500, indicate = 100)

sim_result8 <- sim_result[sim_result$real_dag_size > 7, ]

plot(sim_result[, 2], sim_result[, 3], col = factor(sim_result[, 1]))
abline(reg = c(0, 1))

plot(sim_result8[, 2], sim_result8[, 3], col = factor(sim_result8[, 1]))
abline(reg = c(0, 1))


# Trying out the timeout (with really small time limit) with some larger graphs. 
# Should take a few minutes.
# n = 20; min_n = 6; max_n = 9; max_r = 2; max_e = 2; max_o = 1
# min_sources = 2; max_sources = 4
# prob1 = 0.5; prob2 = 0.35; prob3 = 0.5; prob_sources = c(0.35, 0.125, 0.125, 0.40)
# timeout = 0.005

sim_result_timeout <- simulate_times(20, min_n = 6, max_n = 9, indicate = 1, timeout = 0.005)

# Bigger simulation with clustered graphs of 5-7 nodes and original graphs of 
# 7-11 nodes. Times out after 15 minutes. MIGHT TAKE 2-3 HOURS
# n = 1000; min_n = 5; max_n = 7; max_r = 2; max_e = 2; max_o = 1
# min_sources = 2; max_sources = 4
# prob1 = 0.5; prob2 = 0.35; prob3 = 0.5; prob_sources = c(0.35, 0.125, 0.125, 0.40)

# sim_result_larger <- simulate_times(1000, min_n = 5, max_n = 7, timeout = 0.25, indicate = 1)

# save(sim_result_larger, file = "sim_result_larger.RData")
load("sim_result_larger.RData")
res <- sim_result_larger[sim_result_larger$setting != "timeout", ]

# whole data
plot(res[, 2], res[, 3], col = factor(res[, 1]))
abline(reg = c(0, 1))

# ~1.5 minutes
plot(res$raw_timediff / 1000, res$clustering_timediff / 1000, col = factor(res$setting), xlim = c(0, 1e+02), ylim = c(0, 1e+02),
     xlab = "Time elapsed (s) for Do-search with the original graph",
     ylab = "Time elapsed (s) for running clustering algorithms and \n Do-search with the clustered graph")
abline(reg = c(0, 1))

cols <- ifelse(res$setting %in% c("id", "non_id_idinv"), "green", "blue")

# Create plot
plot(res$raw_timediff / 1000,
     res$clustering_timediff / 1000,
     col = cols,
     pch = 19,
     xlim = c(0, 1e+02),
     ylim = c(0, 1e+02),
     xlab = "Time elapsed (s) for Do-search with the original graph",
     ylab = "Time elapsed (s) for running clustering algorithms and \n Do-search with the clustered graph")

# Add reference line
abline(a = 0, b = 1)

# Add legend
legend("topleft",
       legend = c("Clustering not possible, \nhaving to use original graph", 
                  "Clustering possible"),
       col = c("green", "blue"),
       border = "black",
       pch = 19,
       bty = "n")

# create a short stable key for color mapping
res$color_key <- ifelse(res$setting %in% c("id", "non_id_idinv"),
                        "no_cluster",
                        "cluster")

# Create ggplot — map to color_key (short values), then label them in scale_color_manual()
ggplot(res, aes(
  x = raw_timediff / 1000,
  y = clustering_timediff / 1000,
  color = factor(color_key)
)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40", size = 0.6) +
  scale_color_manual(
    # named vector ensures correct mapping
    values = c(no_cluster = "#1b9e77",  # green
               cluster = "#0072B2"),    # blue
    # present user-facing legend labels (with line break after comma)
    labels = c(cluster = "Clustering not possible,\nhaving to use original graph",
               no_cluster = "Clustering possible"),
    # keep legend order predictable
    breaks = c("no_cluster", "cluster"),
    guide = guide_legend(override.aes = list(alpha = 1, size = 3))
  ) +
  xlim(0, 1e+02) +
  ylim(0, 1e+02) +
  labs(
    x = "Time elapsed (s) for Do-search with the original graph",
    y = "Time elapsed (s) for running clustering algorithms and\nDo-search with the corresponding graph",
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = c(0.05, 0.95),
    legend.justification = c("left", "top"),
    legend.background = element_rect(fill = "white", color = "gray80", size = 0.3),
    legend.key = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    legend.text = element_text(size = 10)
  )
# 1 second
plot(res[, 2], res[, 3], col = factor(res[, 1]), xlim = c(0, 1e+03), ylim = c(0, 1e+03))
abline(reg = c(0, 1))

# 0.1 seconds
plot(res[, 2], res[, 3], col = factor(res[, 1]), xlim = c(0, 1e+02), ylim = c(0, 1e+02))
abline(reg = c(0, 1))
