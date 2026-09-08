# Decoupling from legacy creditors and public social spending

This repository contains the data and code required to construct the datasets and replicate all empirical analyses, figures, and robustness checks in the manuscript.

## Software Requirements
* **R** (version 4.2+ recommended)
* **Stata** (version 16+ recommended, for optional robustness checks)

## Repository Architecture
* `Construct_dataset`: Raw external datasets and scripts for dataset construction.
* `Analysis`: Cleaned dataset files, intermediate outputs, and analysis subfolders.
  * `Analysis/Code`: Primary R estimation scripts.
  * `Analysis/General_figures`: Exported figures for the full sample.
  * `Analysis/LIC_results`: Results for low-income countries.
  * `Analysis/MIC_results`: Results for middle-income countries.

## Execution Order

1. **Dataset Construction (Optional - Recreates `master_data.xlsx`)**
   Set the working directory to the project root and run `Construct_dataset/construct_dataset_main.R`. This processes the raw files and exports `master_data.xlsx` directly into the `Analysis` folder. The secondary scripts in this folder generate alternative definitions of "legacy creditors" used in robustness testing.

2. **Main Empirical Analysis**
   Run `master_execute_main.R` from the project root. This executes all main R scripts in `Analysis/Code`, populates the output folders (`General_figures`, `LIC_results`, `MIC_results`), and automatically exports the dataset needed for the Stata robustness check to `Analysis/`.

3. **Stata Robustness Check (Manual Execution)**
   Open Stata, set the working directory to the project root, and execute `Analysis/Code/iv_reg_frac_probit_cfa.do` manually to produce the instrumental variables and fractional probit robustness checks.
