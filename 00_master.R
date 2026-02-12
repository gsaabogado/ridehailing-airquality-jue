#### ##################################################################### ####
####   Master Script: The Air Quality Effects of Uber                     ####
####   Journal of Urban Economics                                         ####
####   Sarmiento, Kim (2025)                                              ####
#### ##################################################################### ####

#### ===================================================================== ####
####                        USER CONFIGURATION                             ####
#### ===================================================================== ####

#### Set the root directory to the replication_package folder ####
root <- "SET_YOUR_PATH_HERE"  # e.g., "~/replication_package"

#### Control which scripts to run ####
run_data_construction    <- FALSE   # TRUE to rebuild data from raw sources
run_regressions          <- TRUE    # Main text regressions (Tables 3-6, 8)
run_regressions_appendix <- TRUE    # Appendix regressions (Tables G1-J1)
run_exhibits             <- TRUE    # Main text figures and tables (Figs 1-7, Tables 1-2)
run_exhibits_appendix    <- TRUE    # Appendix figures (Figs C1-J4)
install_packages         <- FALSE   # TRUE to install all required packages

#### Parallelization ####
mc_cores <- 4  # Number of cores for parallel operations

#### Reproducibility ####
set.seed(12345)

#### ===================================================================== ####
####                     PACKAGE INSTALLATION                              ####
#### ===================================================================== ####

if (install_packages) {

  cat("=== Installing required packages ===\n")

  pkgs <- c(
    # Core
    "tidyverse", "data.table", "fixest", "etwfe", "synthdid", "broom", "conflicted",
    # Data construction
    "vroom", "readxl", "humidity", "zoo", "con2aqi", "tidycensus", "mgsub",
    # Visualization
    "sf", "ggspatial", "patchwork", "NatParksPalettes", "colorspace", "ggthemes",
    # Tables
    "mmtable2", "kableExtra", "gt", "texreg",
    # Other
    "bacondecomp", "lfe", "haven", "arrow"
  )

  installed <- rownames(installed.packages())
  to_install <- pkgs[!pkgs %in% installed]

  if (length(to_install) > 0) {
    cat(sprintf("Installing %d packages: %s\n", length(to_install), paste(to_install, collapse = ", ")))
    install.packages(to_install)
  } else {
    cat("All packages already installed.\n")
  }
}

#### ===================================================================== ####
####                        DIRECTORY PATHS                                ####
#### ===================================================================== ####

#### Validate root directory ####
if (root == "SET_YOUR_PATH_HERE") {
  stop("Please set the 'root' variable in 00_master.R to the path of the replication_package folder.")
}

#### Define directory paths ####
code_dir    <- file.path(root, "code")
data_raw    <- file.path(root, "02_data")
data_gen    <- file.path(root, "03_gen")
results_dir <- file.path(root, "output", "results")
figures_dir <- file.path(root, "output", "figures")

#### Create output directories if they don't exist ####
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

#### Print configuration ####
cat("\n====================================================\n")
cat("  Replication: The Air Quality Effects of Uber (JUE)  \n")
cat("====================================================\n")
cat(sprintf("  Root:           %s\n", root))
cat(sprintf("  R version:      %s\n", R.version.string))
cat(sprintf("  Platform:       %s\n", R.version$platform))
cat(sprintf("  Date:           %s\n", Sys.Date()))
cat(sprintf("  Cores:          %d\n", mc_cores))
cat("====================================================\n\n")

#### ===================================================================== ####
####                       RUN SCRIPTS                                     ####
#### ===================================================================== ####

#### Step 1: Data Construction (optional -- skip if using pre-built data) ####
if (run_data_construction) {
  cat("\n=== Step 1/5: Data Construction ===\n")
  cat("Building all datasets from raw sources...\n")
  source(file.path(code_dir, "01_data_construction.R"))
  cat("Data construction complete.\n\n")
} else {
  cat("\n=== Step 1/5: Data Construction [SKIPPED] ===\n")
  cat("Using pre-built datasets from data/constructed/\n\n")
}

#### Step 2: Main Regressions ####
if (run_regressions) {
  cat("\n=== Step 2/5: Main Regressions ===\n")
  cat("Estimating Tables 3-6, 8 and dynamic specifications...\n")
  source(file.path(code_dir, "02_regressions.R"))
  cat("Main regressions complete.\n\n")
}

#### Step 3: Appendix Regressions ####
if (run_regressions_appendix) {
  cat("\n=== Step 3/5: Appendix Regressions ===\n")
  cat("Estimating Tables G1-J1 and dynamic specifications...\n")
  source(file.path(code_dir, "03_regressions_appendix.R"))
  cat("Appendix regressions complete.\n\n")
}

#### Step 4: Main Exhibits ####
if (run_exhibits) {
  cat("\n=== Step 4/5: Main Exhibits ===\n")
  cat("Generating Tables 1-2 and Figures 1-7...\n")
  source(file.path(code_dir, "04_exhibits.R"))
  cat("Main exhibits complete.\n\n")
}

#### Step 5: Appendix Exhibits ####
if (run_exhibits_appendix) {
  cat("\n=== Step 5/5: Appendix Exhibits ===\n")
  cat("Generating Figures C1-J4...\n")
  source(file.path(code_dir, "05_exhibits_appendix.R"))
  cat("Appendix exhibits complete.\n\n")
}

#### ===================================================================== ####
####                       COMPLETION                                      ####
#### ===================================================================== ####

cat("\n====================================================\n")
cat("  Replication complete!                              \n")
cat(sprintf("  Results saved to:  %s\n", results_dir))
cat(sprintf("  Figures saved to:  %s\n", figures_dir))
cat("====================================================\n")
