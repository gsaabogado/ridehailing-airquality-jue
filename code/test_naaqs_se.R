#### ===================================================================== ####
#### Test script: NAAQS balance SE recovery                               ####
#### Compares three approaches for NAAQS logit E-TWFE standard errors:    ####
####   A) fe = "none" with analytical SEs (delta method)                  ####
####   B) fe = "vs" with 2000 bootstrap reps (vs current 500)            ####
####   C) fe = "vs" with 500 bootstrap reps (current approach, baseline) ####
#### ===================================================================== ####

set.seed(12345)

#### Threading guards for Juno ####
Sys.setenv(MKL_NUM_THREADS = 1, OMP_NUM_THREADS = 1)

#### Load packages ####
library(fixest)
setFixest_nthreads(1)
library(data.table)
library(etwfe)
library(tidyverse)
library(broom)

cat("etwfe version:", as.character(packageVersion("etwfe")), "\n")
cat("marginaleffects version:", as.character(packageVersion("marginaleffects")), "\n")
cat("fixest version:", as.character(packageVersion("fixest")), "\n")

#### Set root ####
root = "/work/cmcc/ls01122/uber_pol"
setwd(root)

#### Load and prepare data (same as 03_regressions_appendix.R) ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")
data = filter(data, score %in% seq(-5, 0, 1) | treated == 0)
data = select(data, year, enter_year, fips, treated,
              treatment, cbsa_id, NAAQS, forest_fire, ret_pp)

cat("\n=== Data dimensions ===\n")
cat("Rows:", nrow(data), "| Unique CBSAs:", length(unique(data$cbsa_id)), "\n")
cat("NAAQS mean:", mean(data$NAAQS, na.rm = TRUE), "\n\n")

#### ================================================================== ####
#### APPROACH A: fe = "none" with analytical delta-method SEs            ####
#### ================================================================== ####
cat("=== APPROACH A: fe = 'none' (analytical SEs) ===\n")

tryCatch({
  t0 = Sys.time()
  mod_none = etwfe(fml = value ~ 1,
                   tvar = year,
                   gvar = enter_year,
                   data = data %>% rename(value = NAAQS),
                   family = "logit",
                   vcov = ~cbsa_id,
                   fe = "none")

  cat("Model estimated. Computing marginal effects...\n")
  agg_none = emfx(mod_none, type = "simple")
  t1 = Sys.time()

  cat("\nResults (fe = 'none', analytical SEs):\n")
  print(agg_none[, c("term", "estimate", "std.error", "p.value")])
  cat("Time:", round(difftime(t1, t0, units = "mins"), 1), "minutes\n")
}, error = function(e) {
  cat("ERROR in Approach A:", e$message, "\n")
})

#### ================================================================== ####
#### APPROACH B: fe = "vs" with 2000 bootstrap reps                     ####
#### ================================================================== ####
cat("\n=== APPROACH B: fe = 'vs', B = 2000 cluster bootstrap ===\n")

tryCatch({
  t0 = Sys.time()

  # Point estimate first
  mod_vs = etwfe(fml = value ~ 1,
                 tvar = year, gvar = enter_year,
                 data = data %>% rename(value = NAAQS),
                 family = "logit", vcov = ~cbsa_id, fe = "vs")
  agg_vs = suppressWarnings(emfx(mod_vs, vcov = FALSE))
  cat("Point estimate (fe='vs'):", agg_vs$estimate[1], "\n")

  # Bootstrap
  B = 2000
  cbsa_ids = unique(data$cbsa_id)
  n_cbsa = length(cbsa_ids)
  boot_est = rep(NA_real_, B)

  for (b in 1:B) {
    if (b %% 200 == 0) cat("  Bootstrap iteration", b, "of", B, "\n")
    boot_cbsa = sample(cbsa_ids, n_cbsa, replace = TRUE)
    boot_list = lapply(seq_along(boot_cbsa), function(i) {
      d = data[data$cbsa_id == boot_cbsa[i], ]
      d$cbsa_id = i
      d
    })
    boot_data = rbindlist(boot_list)
    boot_data$value = boot_data$NAAQS

    tryCatch({
      m_b = suppressWarnings(
        etwfe(fml = value ~ 1, tvar = year, gvar = enter_year,
              data = boot_data, family = "logit",
              vcov = ~cbsa_id, fe = "vs"))
      a_b = suppressWarnings(emfx(m_b, vcov = FALSE))
      boot_est[b] = a_b$estimate[1]
    }, error = function(e) {})
  }

  t1 = Sys.time()

  n_success = sum(!is.na(boot_est))
  boot_se = sd(boot_est, na.rm = TRUE)
  boot_mean = mean(boot_est, na.rm = TRUE)

  cat("\nResults (fe = 'vs', B = 2000):\n")
  cat("  Successful iterations:", n_success, "of", B, "\n")
  cat("  Point estimate:", agg_vs$estimate[1], "\n")
  cat("  Bootstrap SE:", round(boot_se, 4), "\n")
  cat("  Bootstrap mean:", round(boot_mean, 4), "\n")
  cat("  t-stat:", round(agg_vs$estimate[1] / boot_se, 3), "\n")
  cat("  p-value (normal):", round(2 * pnorm(-abs(agg_vs$estimate[1] / boot_se)), 4), "\n")
  cat("  95% CI (percentile): [",
      round(quantile(boot_est, 0.025, na.rm = TRUE), 4), ",",
      round(quantile(boot_est, 0.975, na.rm = TRUE), 4), "]\n")
  cat("Time:", round(difftime(t1, t0, units = "mins"), 1), "minutes\n")

  # Save bootstrap distribution for diagnostics
  saveRDS(boot_est, file.path(root, "replication_package/03_gen/09_revision/naaqs_boot_2000.rds"))

}, error = function(e) {
  cat("ERROR in Approach B:", e$message, "\n")
})

#### ================================================================== ####
#### APPROACH C: fe = "vs" with 500 bootstrap reps (baseline)           ####
#### ================================================================== ####
cat("\n=== APPROACH C: fe = 'vs', B = 500 cluster bootstrap (baseline) ===\n")

tryCatch({
  t0 = Sys.time()

  set.seed(12345)  # Same seed as production code
  B = 500
  cbsa_ids = unique(data$cbsa_id)
  n_cbsa = length(cbsa_ids)
  boot_est_500 = rep(NA_real_, B)

  for (b in 1:B) {
    if (b %% 100 == 0) cat("  Bootstrap iteration", b, "of", B, "\n")
    boot_cbsa = sample(cbsa_ids, n_cbsa, replace = TRUE)
    boot_list = lapply(seq_along(boot_cbsa), function(i) {
      d = data[data$cbsa_id == boot_cbsa[i], ]
      d$cbsa_id = i
      d
    })
    boot_data = rbindlist(boot_list)
    boot_data$value = boot_data$NAAQS

    tryCatch({
      m_b = suppressWarnings(
        etwfe(fml = value ~ 1, tvar = year, gvar = enter_year,
              data = boot_data, family = "logit",
              vcov = ~cbsa_id, fe = "vs"))
      a_b = suppressWarnings(emfx(m_b, vcov = FALSE))
      boot_est_500[b] = a_b$estimate[1]
    }, error = function(e) {})
  }

  t1 = Sys.time()

  n_success = sum(!is.na(boot_est_500))
  boot_se = sd(boot_est_500, na.rm = TRUE)

  cat("\nResults (fe = 'vs', B = 500, same seed as production):\n")
  cat("  Successful iterations:", n_success, "of", B, "\n")
  cat("  Bootstrap SE:", round(boot_se, 4), "\n")
  cat("  t-stat:", round(agg_vs$estimate[1] / boot_se, 3), "\n")
  cat("  p-value (normal):", round(2 * pnorm(-abs(agg_vs$estimate[1] / boot_se)), 4), "\n")
  cat("  95% CI (percentile): [",
      round(quantile(boot_est_500, 0.025, na.rm = TRUE), 4), ",",
      round(quantile(boot_est_500, 0.975, na.rm = TRUE), 4), "]\n")
  cat("Time:", round(difftime(t1, t0, units = "mins"), 1), "minutes\n")

}, error = function(e) {
  cat("ERROR in Approach C:", e$message, "\n")
})

#### ================================================================== ####
#### Summary comparison                                                  ####
#### ================================================================== ####
cat("\n\n========================================\n")
cat("SUMMARY: NAAQS BALANCE SE COMPARISON\n")
cat("========================================\n")
cat("Original paper SE:  0.013 (analytical, incorrect)\n")
cat("Current replication: 0.172 (bootstrap B=500)\n")
cat("See results above for new approaches.\n")
cat("========================================\n")

cat("\nDone.\n")
