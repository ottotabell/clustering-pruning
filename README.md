# Clustering and Pruning in Causal Data Fusion

This repository contains the R-codes that were used in obtaining the causal effect identifying formulas in the paper Clustering and Pruning in Causal Data Fusion (Tabell, Tikka & Karvanen, 2025). The R-package do-search (Tikka et al., 2024) was used in the examples. 

## Contents

| File | Description |
|------|-------------|
| `01_pruningexample.R` | R code for the pruning example from **Section 3** of the paper. |
| `02_clusteringexample.R` | R code for the clustering examples from **Section 4** of the paper. |
| `03_tobaccoexample.R` | R code for the **Section 7.1** example involving a causal model from an infant mortality study. |
| `04_atherosclerosis.R` | R code for the **Section 7.2** example involving a causal model from an atherosclerosis study. |
| simulation | Folder containing the codes for the simulation in **Section 6** |
| `01_utils.R` | Helper functions to aid in the simulation. |
| `02_findtrclust.R` | R implementation of the FindTrClust algorithm by Tikka et al. 2023 |
| `03_idinvariant.R` | R implementation of VerifyInputs algorithm by Tabell et al. 2025. |
| `04_simulate_dag_and_data.R` | R implementation of SimulateInstance by Tabell et al. 2025 that generates random causal graph with transit clusters and input distributions |
| `05_simulation.R` | Generates the simulation as explained in **Section 6** |
| `06_test_fucntions.R` | Tests for functions in `02_findtrclust.R`, `03_idinvariant.R` and `04_simulate_dag_and_data.R` |
| `07_simulation.R` | Testing `04_simulation.R` |
| `08_run_simulation.R` | R code for running the simulation in Shell environment |
| `09_analyze.R` | Analyses of the simulation results |
| `simulate.sh` | Shell code for the simulation |


