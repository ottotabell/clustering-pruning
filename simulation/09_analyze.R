# R codes for analyzing the simulated data

library(tidyverse)

# Get all matching file names (sorted)
# files <- list.files(pattern = "^simulation_results_\\d{3}\\.rds$")

# Read each .rds file and combine into one data frame
# combined_results <- files %>%
#   lapply(readRDS) %>%
#   bind_rows(.id = "source_file")

# saveRDS(combined_results, "combined_simulation_results.rds")

d <- combined_simulation_results[combined_simulation_results$setting != "timeout", ]

# Two observations with unrealistically high running times. To be removed?
d <- d[!(d$real_dag_size == 6 & d$clustering_timediff > 2000), ]
d <- d[!(d$real_dag_size == 10 & d$clustering_timediff > 40000 & d$setting != "non_id_non_idinv"), ]

d$raw_clust_diff <- d$raw_timediff - d$clustering_timediff
d$size_factor <- as.factor(d$real_dag_size)

# Scatter plots of Figure 7

base_plot <- function(df, lim) {
  ggplot(
    data = df,
    aes(
      x = raw_timediff / 1000,
      y = clustering_timediff / 1000,
      col = setting
    )
  ) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0) +
    scale_x_continuous(limits = c(0, lim)) +
    scale_y_continuous(limits = c(0, lim)) +
    scale_color_manual(
      values = c("black", "#E69F00", "#56B4E9"),
      name = "Setting",
      labels = c("1", "2", "3")
    ) +
    labs(
      x = "Unclustered strategy (s)",
      y = "Clustered strategy (s)"
    ) +
    theme_bw() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5))
}

# filter once
d_use <- d[
  d$real_dag_size %in% c(6, 8, 10, 12) &
    d$setting != "non_id_non_idinv",
]

# individual panels
p6  <- base_plot(subset(d_use, size_factor == 6),  lim = 0.15)

p8  <- base_plot(subset(d_use, size_factor == 8),  lim = 5)

p10 <- base_plot(subset(d_use, size_factor == 10), lim = 300)

p12 <- base_plot(subset(d_use, size_factor == 12), lim = 900)

ggsave(
  "../../paper/plots/scatter6.pdf",
  p6,
  width  = 0.55 * 6.5,  # 6.5in ≈ LaTeX textwidth
  height = 0.55 * 6.5,
  units  = "in"
)

ggsave(
  "../../paper/plots/scatter8.pdf",
  p8,
  width  = 0.525 * 6.5,
  height = 0.525 * 6.5,
  units  = "in"
)

ggsave(
  "../../paper/plots/scatter10.pdf",
  p10,
  width  = 0.55 * 6.5,  # 6.5in ≈ LaTeX textwidth
  height = 0.55 * 6.5,
  units  = "in"
)

ggsave(
  "../../paper/plots/scatter12.pdf",
  p12,
  width  = 0.525 * 6.5,
  height = 0.525 * 6.5,
  units  = "in"
)

# Table 1

d %>% mutate(raw_clust_diff = raw_clust_diff / 1000) %>% group_by(size_factor, setting) %>% 
  summarise(median = round(median(raw_clust_diff), 2),
            lower = round(quantile(raw_clust_diff, 0.25), 2),
            upper = round(quantile(raw_clust_diff, 0.75), 2),
            n = n()) |> View()

d %>% mutate(raw_clust_prop = raw_timediff / clustering_timediff) %>% group_by(size_factor, setting) %>% 
  summarise(median = round(median(raw_clust_prop), 2),
            lower = round(quantile(raw_clust_prop, 0.25), 2),
            upper = round(quantile(raw_clust_prop, 0.75), 2),
            n = n()) |> View()

# Box plot of Figure 8

settingC <- ggplot(data = d[d$setting == "non_id_non_idinv", ], mapping = aes(x = size_factor, y = raw_clust_diff / 1000)) +
  geom_boxplot(outliers = FALSE) +
  labs(x = "Original graph size", y = "Difference in running time (s)") +
  theme_bw()

ggsave(
  "plots/settingC.pdf",
  settingC
)


# Takes data sources as a single string (potentially with newlines) and returns a list of observed, intervened
# and conditioned variables
# For example: extract_variables_by_source("p(a|do(b), c)\np(d, e, f)") should
# return a list:
# observed[[1]] = "a"
# observed[[2]] = c("d", "e", "f")
# intervened[[1]] = "b"
# intervened[[2]] = character(0)
# conditioned[[1]] = "c"
# conditioned[[2]] = character(0)
extract_variables_by_source <- function(sources) {
  # Split the input string by newlines to get individual sources
  sources_vec <- strsplit(sources, "\n")[[1]]
  # Remove any empty strings
  sources_vec <- sources_vec[nzchar(trimws(sources_vec))]
  
  n <- length(sources_vec)
  observed_list <- list()
  intervened_list <- list()
  conditioned_list <- list()
  
  for (i in seq_along(sources_vec)) {
    src <- sources_vec[i]
    
    # Removing "p(" from the start and ")" from the end
    content <- gsub("^p\\(|\\)$", "", src)
    # Removing empty spaces
    content <- gsub(" ", "", content)
    
    obs_vars <- character(0)
    int_vars <- character(0)
    cond_vars <- character(0)
    
    # Splitting variables to A and B&C. [[1]] is needed because strsplit
    # returns a list of one element. [[1]] gives us a vector.
    parts <- strsplit(content, "\\|")[[1]]
    
    # Splitting observed variables by comma
    obs_vars <- strsplit(trimws(parts[1]), ",\\s*")[[1]]
    
    if (length(parts) > 1) {
      cond_part <- trimws(parts[2])
      
      do_match <- regexpr("do\\([^)]*\\)", cond_part)
      if (do_match != -1) {
        do_str <- regmatches(cond_part, do_match)
        int_vars <- strsplit(gsub("do\\(|\\)", "", do_str), ",\\s*")[[1]]
        cond_part <- gsub("do\\([^)]*\\),?", "", cond_part)
        cond_part <- trimws(cond_part)
      }
      
      # Only parse conditioned variables if anything is left
      if (nzchar(cond_part)) {
        cond_vars <- strsplit(cond_part, ",\\s*")[[1]]
      }
    }
    
    # Always assign (even if empty)
    observed_list[[i]]    <- obs_vars
    intervened_list[[i]]  <- int_vars
    conditioned_list[[i]] <- cond_vars
  }
  
  list(
    observed = observed_list,
    intervened = intervened_list,
    conditioned = conditioned_list
  )
}

# Checking gid-instances

res <- logical(1000)
  
check_gid <- Vectorize(function(data) {
  
  s <- extract_variables_by_source(data)
  for (i in 1:length(s$conditioned)) {
    if (length(s$conditioned[[i]]) > 0) return(FALSE)
  }
  
  V <- c()
  for (i in 1:length(s$observed)) {
    V <- union(V, s$observed[[i]])
    V <- union(V, s$intervened[[i]])
  }
  
  for (i in 1:length(s$observed)) {
    if (sum(V %in% union(s$observed[[i]], s$intervened[[i]])) != length(V)) return(FALSE)
  }
  
  return(TRUE)
})

res <- check_gid(d$clustered_data)
d$clustered_data[5]
