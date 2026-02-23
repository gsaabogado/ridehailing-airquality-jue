#### ===================================================================== ####
#### Test script v2: NAAQS balance SE — jackknife + BCa + diagnostics     ####
#### All approaches use fe = "vs" to preserve the point estimate          ####
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

cat("etwfe version:", as.character(packageVersion("etwfe")), "\n")
cat("marginaleffects version:", as.character(packageVersion("marginaleffects")), "\n")

#### Set root ####
root = "/work/cmcc/ls01122/uber_pol"
setwd(root)

#### Load and prepare data (same as 03_regressions_appendix.R) ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")
data = filter(data, score %in% seq(-5, 0, 1) | treated == 0)
data = select(data, year, enter_year, fips, treated,
              treatment, cbsa_id, NAAQS, forest_fire, ret_pp)

cat("Rows:", nrow(data), "| Unique CBSAs:", length(unique(data$cbsa_id)), "\n")
cat("NAAQS mean:", mean(data$NAAQS, na.rm = TRUE), "\n\n")

#### ================================================================== ####
#### Step 0: Point estimate with fe = "vs" (baseline)                    ####
#### ================================================================== ####
cat("=== POINT ESTIMATE (fe = 'vs') ===\n")
mod_vs = etwfe(fml = NAAQS ~ 1, tvar = year, gvar = enter_year,
               data = data, family = "logit", vcov = ~cbsa_id, fe = "vs")
agg_vs = suppressWarnings(emfx(mod_vs, vcov = FALSE))
theta_hat = agg_vs$estimate[1]
cat("ATT point estimate:", theta_hat, "\n\n")

#### ================================================================== ####
#### APPROACH 1: Delete-one-cluster jackknife                            ####
#### ================================================================== ####
cat("=== APPROACH 1: JACKKNIFE (delete-one-cluster) ===\n")
t0 = Sys.time()

cbsa_ids = sort(unique(data$cbsa_id))
G = length(cbsa_ids)
theta_jack = rep(NA_real_, G)

for (g in 1:G) {
  if (g %% 50 == 0) cat("  Jackknife cluster", g, "of", G, "\n")
  d_g = data[data$cbsa_id != cbsa_ids[g], ]
  tryCatch({
    m_g = suppressWarnings(
      etwfe(fml = NAAQS ~ 1, tvar = year, gvar = enter_year,
            data = d_g, family = "logit", vcov = ~cbsa_id, fe = "vs"))
    a_g = suppressWarnings(emfx(m_g, vcov = FALSE))
    theta_jack[g] = a_g$estimate[1]
  }, error = function(e) {})
}

t1 = Sys.time()

n_success_jack = sum(!is.na(theta_jack))
theta_bar = mean(theta_jack, na.rm = TRUE)
se_jack = sqrt(((G - 1) / G) * sum((theta_jack[!is.na(theta_jack)] - theta_bar)^2))

cat("\nJackknife results:\n")
cat("  Successful clusters:", n_success_jack, "of", G, "\n")
cat("  Point estimate (original):", theta_hat, "\n")
cat("  Jackknife mean:", round(theta_bar, 5), "\n")
cat("  Jackknife SE:", round(se_jack, 4), "\n")
cat("  t-stat:", round(theta_hat / se_jack, 3), "\n")
cat("  p-value (normal):", round(2 * pnorm(-abs(theta_hat / se_jack)), 4), "\n")
cat("  95% CI: [", round(theta_hat - 1.96 * se_jack, 4), ",",
    round(theta_hat + 1.96 * se_jack, 4), "]\n")
cat("Time:", round(difftime(t1, t0, units = "mins"), 1), "minutes\n")

# Save jackknife distribution
saveRDS(theta_jack,
        "replication_package/03_gen/09_revision/naaqs_jackknife.rds")

#### ================================================================== ####
#### APPROACH 2: Bootstrap diagnostics + robust SE measures              ####
#### Reuses the B=500 bootstrap from the production code                 ####
#### ================================================================== ####
cat("\n=== APPROACH 2: BOOTSTRAP B=500 DIAGNOSTICS ===\n")
t0 = Sys.time()

set.seed(12345)  # Same seed as production
B = 500
boot_est = rep(NA_real_, B)

for (b in 1:B) {
  if (b %% 100 == 0) cat("  Bootstrap iteration", b, "of", B, "\n")
  boot_cbsa = sample(cbsa_ids, G, replace = TRUE)
  boot_list = lapply(seq_along(boot_cbsa), function(i) {
    d = data[data$cbsa_id == boot_cbsa[i], ]
    d$cbsa_id = i
    d
  })
  boot_data = rbindlist(boot_list)

  tryCatch({
    m_b = suppressWarnings(
      etwfe(fml = NAAQS ~ 1, tvar = year, gvar = enter_year,
            data = boot_data, family = "logit",
            vcov = ~cbsa_id, fe = "vs"))
    a_b = suppressWarnings(emfx(m_b, vcov = FALSE))
    boot_est[b] = a_b$estimate[1]
  }, error = function(e) {})
}

t1 = Sys.time()

n_success = sum(!is.na(boot_est))
boot_valid = boot_est[!is.na(boot_est)]

cat("\nBootstrap diagnostics (B=500):\n")
cat("  Successful iterations:", n_success, "of", B, "\n")
cat("  Failure rate:", round(100 * (B - n_success) / B, 1), "%\n")

# Standard SE
se_boot = sd(boot_valid)
cat("\n  Standard SE:", round(se_boot, 4), "\n")
cat("  t-stat:", round(theta_hat / se_boot, 3), "\n")
cat("  p-value:", round(2 * pnorm(-abs(theta_hat / se_boot)), 4), "\n")

# Trimmed SE (drop 2.5% tails)
trim_lo = quantile(boot_valid, 0.025)
trim_hi = quantile(boot_valid, 0.975)
boot_trimmed = boot_valid[boot_valid >= trim_lo & boot_valid <= trim_hi]
se_trimmed = sd(boot_trimmed)
cat("\n  Trimmed SE (5% tails removed):", round(se_trimmed, 4), "\n")
cat("  t-stat (trimmed):", round(theta_hat / se_trimmed, 3), "\n")
cat("  p-value (trimmed):", round(2 * pnorm(-abs(theta_hat / se_trimmed)), 4), "\n")

# IQR-based SE (robust to heavy tails)
se_iqr = IQR(boot_valid) / 1.349
cat("\n  IQR-based SE:", round(se_iqr, 4), "\n")
cat("  t-stat (IQR):", round(theta_hat / se_iqr, 3), "\n")
cat("  p-value (IQR):", round(2 * pnorm(-abs(theta_hat / se_iqr)), 4), "\n")

# MAD-based SE
se_mad = mad(boot_valid)
cat("\n  MAD-based SE:", round(se_mad, 4), "\n")
cat("  t-stat (MAD):", round(theta_hat / se_mad, 3), "\n")
cat("  p-value (MAD):", round(2 * pnorm(-abs(theta_hat / se_mad)), 4), "\n")

# Percentile CI
cat("\n  Percentile 95% CI: [",
    round(quantile(boot_valid, 0.025), 4), ",",
    round(quantile(boot_valid, 0.975), 4), "]\n")
cat("  Percentile 90% CI: [",
    round(quantile(boot_valid, 0.05), 4), ",",
    round(quantile(boot_valid, 0.95), 4), "]\n")

# Distribution shape
cat("\n  Distribution shape:\n")
cat("    Mean:", round(mean(boot_valid), 4), "\n")
cat("    Median:", round(median(boot_valid), 4), "\n")
cat("    Skewness:", round(
  mean(((boot_valid - mean(boot_valid)) / sd(boot_valid))^3), 3), "\n")
cat("    Kurtosis:", round(
  mean(((boot_valid - mean(boot_valid)) / sd(boot_valid))^4), 3), "\n")
cat("    Min:", round(min(boot_valid), 4), "\n")
cat("    Max:", round(max(boot_valid), 4), "\n")
cat("    5th pctl:", round(quantile(boot_valid, 0.05), 4), "\n")
cat("    95th pctl:", round(quantile(boot_valid, 0.95), 4), "\n")

# BCa confidence interval
cat("\n  BCa 95% CI:\n")
# Bias correction
z0 = qnorm(mean(boot_valid < theta_hat))
cat("    Bias correction z0:", round(z0, 3), "\n")

# Acceleration (from jackknife influence values)
theta_jack_valid = theta_jack[!is.na(theta_jack)]
theta_dot = mean(theta_jack_valid)
num = sum((theta_dot - theta_jack_valid)^3)
den = 6 * (sum((theta_dot - theta_jack_valid)^2))^1.5
a_hat = num / den
cat("    Acceleration a:", round(a_hat, 4), "\n")

# BCa percentiles
alpha = c(0.025, 0.975)
z_alpha = qnorm(alpha)
adj_alpha = pnorm(z0 + (z0 + z_alpha) / (1 - a_hat * (z0 + z_alpha)))
bca_ci = quantile(boot_valid, adj_alpha)
cat("    BCa 95% CI: [", round(bca_ci[1], 4), ",", round(bca_ci[2], 4), "]\n")

# Check if 0 is outside BCa CI
if (bca_ci[2] < 0) {
  cat("    ** 0 is OUTSIDE the BCa 95% CI => significant at 5% **\n")
} else {
  cat("    0 is inside the BCa 95% CI\n")
}

# BCa 90% CI
alpha90 = c(0.05, 0.95)
z_alpha90 = qnorm(alpha90)
adj_alpha90 = pnorm(z0 + (z0 + z_alpha90) / (1 - a_hat * (z0 + z_alpha90)))
bca_ci90 = quantile(boot_valid, adj_alpha90)
cat("    BCa 90% CI: [", round(bca_ci90[1], 4), ",", round(bca_ci90[2], 4), "]\n")
if (bca_ci90[2] < 0) {
  cat("    ** 0 is OUTSIDE the BCa 90% CI => significant at 10% **\n")
} else {
  cat("    0 is inside the BCa 90% CI\n")
}

cat("Time:", round(difftime(t1, t0, units = "mins"), 1), "minutes (bootstrap only)\n")

# Save bootstrap distribution
saveRDS(boot_est,
        "replication_package/03_gen/09_revision/naaqs_boot_500_diag.rds")

#### ================================================================== ####
#### Summary                                                              ####
#### ================================================================== ####
cat("\n\n========================================\n")
cat("SUMMARY: NAAQS SE COMPARISON\n")
cat("========================================\n")
cat("Point estimate (fe='vs'):", theta_hat, "\n\n")
cat("SE estimates:\n")
cat("  Original paper (analytical, incorrect): 0.013\n")
cat("  Jackknife (delete-one-cluster):        ", round(se_jack, 4), "\n")
cat("  Bootstrap B=500 (standard SD):         ", round(se_boot, 4), "\n")
cat("  Bootstrap B=500 (trimmed 5%):          ", round(se_trimmed, 4), "\n")
cat("  Bootstrap B=500 (IQR-based):           ", round(se_iqr, 4), "\n")
cat("  Bootstrap B=500 (MAD-based):           ", round(se_mad, 4), "\n")
cat("========================================\n")

cat("\nDone.\n")
