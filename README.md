# Replication Package: The Air Quality Effects of Uber

This repository contains the code and instructions to replicate all tables and figures in:

> Sarmiento, L., Kim, Y.J. (2025). "The Air Quality Effects of Uber." *Journal of Urban Economics*.

## Data Access

The data required to run this code is archived on Zenodo:

> <https://doi.org/10.5281/zenodo.18708962>

Download the data archive and extract it into this repository so that the structure matches:

```
replication_package/
├── 02_data/                          # Raw data (from Zenodo)
│   ├── 05_shp/                       #   Shapefiles (states, counties)
│   └── 06_other/                     #   ACS, gasoline, NYC taxi, vehicles, power gen, etc.
├── 03_gen/                           # Constructed data (from Zenodo)
│   ├── 01_aqi/                       #   Daily AQI panels
│   ├── 02_weather/                   #   ERA5 county-level weather
│   ├── 03_uber/                      #   Uber entry dates and market data
│   ├── 04_reg/                       #   Regression-ready panel datasets
│   ├── 05_results/                   #   Saved regression result objects
│   └── 09_revision/                  #   Revision-specific outputs
├── code/
│   ├── 01_data_construction.R
│   ├── 02_regressions.R
│   ├── 03_regressions_appendix.R
│   ├── 04_exhibits.R
│   └── 05_exhibits_appendix.R
├── output/
│   ├── figures/
│   └── results/
├── 00_master.R
└── README.md
```

## Quick Start

If you want to replicate results **without** rebuilding data from scratch:

1. Download the Zenodo archive and extract `02_data/` and `03_gen/` into this folder
2. Open `00_master.R` and set `root` to the path of this folder
3. Set `run_data_construction <- FALSE` (default)
4. Run `00_master.R`

To rebuild everything from raw data, set `run_data_construction <- TRUE`.

## Software Requirements

All analysis was conducted in **R 4.4.x** on macOS. The following packages are required:

### Core
| Package | Version | Purpose |
|---------|---------|---------|
| `tidyverse` | >= 2.0.0 | Data wrangling, visualization |
| `data.table` | >= 1.15.0 | Fast data manipulation |
| `fixest` | >= 0.12.0 | Two-way fixed effects, Sun & Abraham |
| `etwfe` | >= 0.4.0 | Extended TWFE (Wooldridge 2021) |
| `synthdid` | >= 0.0.9 | Synthetic difference-in-differences |
| `broom` | >= 1.0.0 | Tidy model output |
| `conflicted` | >= 1.2.0 | Namespace conflict resolution |

### Data Construction
| Package | Purpose |
|---------|---------|
| `vroom` | Fast file reading |
| `readxl` | Excel file import |
| `humidity` | Humidity conversions for weather data |
| `zoo` | Rolling-window operations |
| `con2aqi` | Pollutant concentration to AQI conversion |
| `tidycensus` | Census API access |
| `mgsub` | Multiple string substitution |

### Visualization
| Package | Purpose |
|---------|---------|
| `sf` | Spatial data and maps |
| `ggspatial` | Map annotations |
| `patchwork` | Plot composition |
| `NatParksPalettes` | Color palettes |
| `colorspace` | Color manipulation |
| `ggthemes` | Additional ggplot themes |

### Tables
| Package | Purpose |
|---------|---------|
| `mmtable2` | Summary tables |
| `kableExtra` | Table formatting |
| `gt` | Publication tables |
| `texreg` | Regression tables |

### Other
| Package | Purpose |
|---------|---------|
| `bacondecomp` | Goodman-Bacon decomposition |
| `lfe` | Linear FE (Bacon decomposition) |
| `haven` | Stata file import |
| `parallel` | Parallel processing (base R) |

To install all packages at once, run from the R console:

```r
source("00_master.R")  # with install_packages <- TRUE
```

## Table and Figure Mapping

### Main Text

| Exhibit | Script | Description |
|---------|--------|-------------|
| Table 1 | `04_exhibits.R` | Key variables across treatment groups |
| Table 2 | `04_exhibits.R` | Environmental conditions across treatment groups |
| Table 3 | `02_regressions.R` | Effects of Uber on AQI |
| Table 4 | `02_regressions.R` | Effects on air quality alerts |
| Table 5 | `02_regressions.R` | Effects on light-duty vehicles |
| Table 6 | `02_regressions.R` | Effects on commuting behavior |
| Table 8 | `02_regressions.R` | Effects on gasoline sales |
| Figure 1 | `04_exhibits.R` | Expansion of Uber (2010--2016) |
| Figure 2 | `04_exhibits.R` | Pre-treatment AQI by cohort |
| Figure 3 | `02_regressions.R` + `04_exhibits.R` | Dynamic effects on AQI |
| Figure 5 | `04_exhibits.R` | TNCs in New York City |
| Figure 6 | `04_exhibits.R` | Fuel efficiency and gasoline costs |
| Figure 7 | `04_exhibits.R` | Gasoline and clean vehicles in California |

### Appendix

| Exhibit | Script | Description |
|---------|--------|-------------|
| Figure C1 | `05_exhibits_appendix.R` | Diffusion of Uber |
| Figure C2--C3 | `05_exhibits_appendix.R` | Air quality maps |
| Figure C4 | `05_exhibits_appendix.R` | Entry week of Uber |
| Figure E1 | `05_exhibits_appendix.R` | Goodman-Bacon decomposition |
| Figure E2 | `03_regressions_appendix.R` + `05_exhibits_appendix.R` | Cohort weights |
| Figure G1 | `05_exhibits_appendix.R` | Sample restriction effects |
| Figure G2 | `05_exhibits_appendix.R` | Pre-treatment trends (SDiD) |
| Figure G3--G4 | `05_exhibits_appendix.R` | AQI distribution |
| Figure G5 | `05_exhibits_appendix.R` | Alert effects by stringency |
| Figure G6 | `03_regressions_appendix.R` | Dynamic effects by estimator |
| Figure H7--H8 | `05_exhibits_appendix.R` | PM2.5 maps |
| Figure H11 | `05_exhibits_appendix.R` | Placebo tests |
| Figures I11--I16 | `05_exhibits_appendix.R` | Commuting, taxi, and gasoline trends |
| Figure J1--J4 | `03_regressions_appendix.R` | Heterogeneous effects |
| Table G1 | `03_regressions_appendix.R` | Heterogeneous clustering |
| Table H5 | `03_regressions_appendix.R` | Gasoline and coal robustness |
| Table H6 | `03_regressions_appendix.R` | Balancing regressions |
| Table H7 | `03_regressions_appendix.R` | Macro-covariates balance |
| Table H8 | `03_regressions_appendix.R` | Pre-trends testing |
| Table H9 | `03_regressions_appendix.R` | Effects on PM2.5 |
| Table H10 | `03_regressions_appendix.R` | Restricted cohorts |
| Table H11 | `03_regressions_appendix.R` | Alternative specifications |
| Table H12 | `03_regressions_appendix.R` | Granularity and stacked panel |
| Tables I13--I16 | `05_exhibits_appendix.R` + `03_regressions_appendix.R` | NYC taxi regressions |
| Table I19 | `03_regressions_appendix.R` | California vehicle shares |
| Table J1 | `03_regressions_appendix.R` | NowCast AQI effects |

## Computational Notes

- **Runtime**: Full replication from raw data takes approximately 4--6 hours on a modern workstation. Starting from constructed regression datasets, the analysis scripts complete in approximately 1--2 hours.
- **Memory**: The ERA5 weather data processing requires at least 16 GB of RAM. All other scripts run with 8 GB.
- **Parallelization**: Some synthetic DiD specifications use `parallel::mclapply`. Set `mc.cores` in the master script to match your machine.
- **Reproducibility**: All random operations use `set.seed(12345)`.

## Acknowledgments

This replication package was assembled with the assistance of [Claude Code](https://docs.anthropic.com/en/docs/claude-code), an AI-powered coding tool developed by [Anthropic](https://www.anthropic.com).

## License

MIT License. See `LICENSE` for details.

## Contact

Luis Sarmiento -- luis.sarmiento@eiee.org
Yeong Jae Kim -- kyj@kdischool.ac.kr
