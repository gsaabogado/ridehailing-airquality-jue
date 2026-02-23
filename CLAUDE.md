# Uber & Air Quality — Project Architecture

## Overview
Paper: "Air Quality Effects of Uber" (Sarmiento & Kim, 2025, Journal of Urban Economics)
Status: Accepted at JUE. Replication package submitted. Zenodo DOI: 10.5281/zenodo.18708962.

## Directory Structure
- `02_data/` — Raw data (AQI, Uber entry dates, Census, NHTS, shapefiles, EIA, NYC medallion, etc.)
- `03_gen/` — Generated intermediate data (regression datasets, Uber entry data)
  - `03_gen/04_reg/` — Regression-ready datasets (yearly_reg.rds, daily_reg.rds, etc.)
  - `03_gen/05_results/` — Regression output (38 .rds files)
  - `03_gen/09_revision/` — Revision-specific results (hourly NowCast, cohort weights, etc.)
- `04_scripts/` — Analysis scripts (main project versions)
- `05_other/18_JUE/Revision_2nd/` — Covering letter and revision materials
- `replication_jue/` — Replication tracking (replication.xlsx with detailed notes per table)
- `replication_package/` — Self-contained replication package for JUE (14 GB)
  - `code/00_master.R` — Master orchestration script
  - `code/01_data_construction.R` — Builds datasets from raw data (includes Section I: EIA power gen)
  - `code/02_regressions.R` — Main regressions (SDID, TWFE, ETWFE, SADD)
  - `code/03_regressions_appendix.R` — Appendix regressions (bacon decomp, hourly, placebo)
  - `code/04_exhibits.R` — Main figures and tables
  - `code/05_exhibits_appendix.R` — Appendix figures and tables
  - `output/figures/` — Generated figures (33 files: .png and .pdf)
  - `output/results/` — Generated tables
- GitHub: `https://github.com/gsaabogado/ridehailing-airquality-jue`

## Key R Packages
fixest (panel regressions), synthdid (synthetic DID), etwfe (extended TWFE),
bacondecomp (Bacon decomposition), tidyverse, data.table, sf (spatial), modelsummary

## HPC Execution (CMCC Juno)
- Remote path: `/work/cmcc/ls01122/uber_pol/replication_package/`
- Queue: `s_medium` (serial, 1 core, 6h max, up to 442G mem)
- Project code: `-P 0560`
- Critical: Must set `MKL_NUM_THREADS=1`, `OMP_NUM_THREADS=1`, `setFixest_nthreads(1)` to avoid Intel MKL threading crashes
- Memory: Regression jobs need 128G; exhibit jobs need 64G
- Job scripts: `job_02_regressions.sh` through `job_05_exhibits_appendix.sh`

## Known Data Issues
- `hourly_reg_aqi.rds` uses CamelCase columns (NowCast, TmpBin, RainBin, EnterDate)
- `03_regressions_appendix.R` has `year()`/`month()` conflicts between lubridate and data.table — requires `lubridate::` namespace qualification
- Section I (EIA data construction) produces corrupted values on Juno (Intel MKL + data.table). Skip guard added; pre-built xlsx files are included. Works fine on standard R.
- `05_exhibits_appendix.R` references `Uber.rds` (capital U) — needs symlink on case-sensitive Linux filesystems

## Replication Changes from Published Paper
10 of 65 exhibits required minor corrections (documented in covering letter and replication.xlsx):
Table 3 (SDiD copy-paste), Table 5 (jackknife SEs), H4/H5 (EIA data fix), H6 (NAAQS jackknife SEs),
H12 (label swap), I13/I14 (NHTS survey weights), I15 (NYC taxi survey weights), J1 (NowCast hourly).
H7 label swap was reclassified as exact match. All changes are non-substantive; qualitative conclusions unchanged.
- H6 NAAQS SE: original 0.013 was incorrect (etwfe analytical SE bug for logit+fe="vs"). Correct SE is 0.103 (delete-one-cluster jackknife, G=345 CBSAs). Estimate (-0.102) unchanged and insignificant (balance test).

## Covering Letter
- File: `05_other/18_JUE/Revision_2nd/covering_letter.tex`
- Title: "Response to Editorial Comments and Replication Package"
- Format: scrartcl, Palatino (newpxtext/newpxmath), dark slate blue RGB(30,60,90), 1.25 line spacing
- Replication section uses per-section tables with alternating row shading, green checkmarks (exact match) and amber tildes (minor correction)
- Compile: `tectonic covering_letter.tex -Z continue-on-errors`
