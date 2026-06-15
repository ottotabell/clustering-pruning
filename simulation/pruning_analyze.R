# R codes for analyzing the simulated data

library(tidyverse)

# Get all matching file names (sorted)
# files <- list.files(pattern = "^simulation_results_\\d{3}\\.rds$")

# Read each .rds file and combine into one data frame
# combined_results <- files %>%
#   lapply(readRDS) %>%
#   bind_rows(.id = "source_file")

# saveRDS(combined_results, "combined_simulation_results.rds")

d <- readRDS("pruning.rds")
d$raw_clust_diff <- d$raw_timediff - d$new_timediff
d$size_factor <- as.factor(d$real_dag_size)

# Turn summary into a LaTeX table

summary_df <- d %>%
  mutate(
    raw_clust_diff  = raw_clust_diff / 1000,
    raw_clust_ratio = raw_timediff / new_timediff,
    setting = new_setting
  ) %>%
  filter(real_dag_size < 13) %>%
  group_by(size_factor, setting) %>%
  summarise(
    median_diff  = round(median(raw_clust_diff), 2),
    lower_diff   = round(quantile(raw_clust_diff, 0.25), 2),
    upper_diff   = round(quantile(raw_clust_diff, 0.75), 2),
    median_ratio = round(median(raw_clust_ratio), 2),
    lower_ratio  = round(quantile(raw_clust_ratio, 0.25), 2),
    upper_ratio  = round(quantile(raw_clust_ratio, 0.75), 2),
    n            = n(),
    .groups      = "drop"
  )

# Helper: format N with a thousands space (e.g. 1 872)
fmt_n <- function(x) {
  formatC(x, format = "d", big.mark = "\\ ", big.interval = 3)
}

# Helper: format a number for the median diff cell (wrap negatives in $...$)
fmt_diff <- function(x) {
  s <- formatC(x, format = "f", digits = 2)
  paste0("$", s, "$")
}

# Helper: format the IQR parenthetical
fmt_iqr_diff <- function(lo, hi) {
  lo_s <- formatC(lo, format = "f", digits = 2)
  hi_s <- formatC(hi, format = "f", digits = 2)
  # add a leading space inside the parenthesis when lower bound is non-negative
  lo_fmt <- if (lo >= 0) paste0(" ", lo_s) else lo_s
  paste0("$(", lo_fmt, ",\\ ", hi_s, ")$")
}

fmt_ratio <- function(x) {
  s <- formatC(x, format = "f", digits = 2)
  paste0("$", s, "$")
}

fmt_iqr_ratio <- function(lo, hi) {
  lo_s <- formatC(lo, format = "f", digits = 2)
  hi_s <- formatC(hi, format = "f", digits = 2)
  paste0("$(", lo_s, ",\\ ", hi_s, ")$")
}

# Build the table body rows
rows <- apply(summary_df, 1, function(r) {
  size        <- r[["size_factor"]]
  setting     <- r[["setting"]]
  med_diff    <- as.numeric(r[["median_diff"]])
  lo_diff     <- as.numeric(r[["lower_diff"]])
  hi_diff     <- as.numeric(r[["upper_diff"]])
  med_ratio   <- as.numeric(r[["median_ratio"]])
  lo_ratio    <- as.numeric(r[["lower_ratio"]])
  hi_ratio    <- as.numeric(r[["upper_ratio"]])
  n_val       <- as.integer(r[["n"]])
  
  paste(
    size, "&", setting, "&",
    fmt_diff(med_diff), "&", fmt_iqr_diff(lo_diff, hi_diff), "&",
    fmt_ratio(med_ratio), "&", fmt_iqr_ratio(lo_ratio, hi_ratio), "&",
    fmt_n(n_val),
    "\\\\"
  )
})

# Assemble the full table
caption <- paste0(
  "Median differences (in seconds) of running times between the unclustered and clustered strategies, ",
  "and median ratios (the running time for the unclustered strategy divided by the running time of the ",
  "clustered strategy). The interquartile ranges (IQR) are included in parentheses. The number of ",
  "instances for each graph size and setting is denoted by $N$ ",
  "(A: identifiable in the clustered graph, B: non-identifiable in the clustered graph but identification ",
  "invariant, C: non-identifiable in the clustered graph and not identification invariant)."
)

latex_table <- c(
  "\\begin{table}[ht]",
  "\\centering",
  "\\begin{tabular}{c c r l r l r}",
  "\\toprule",
  "Graph size & Setting & \\multicolumn{2}{c}{Median difference (IQR)} & \\multicolumn{2}{c}{Median ratio (IQR)} & $N$ \\\\",
  "\\midrule",
  rows,
  "\\bottomrule",
  "\\end{tabular}",
  paste0("\\caption{", caption, "}"),
  "\\label{tab:simresults}",
  "\\end{table}"
)

# Print to console
cat(paste(latex_table, collapse = "\n"))

# Or write to a .tex file
writeLines(latex_table, "table_simresults.tex")
