# Data Documentation

## The Air Quality Effects of Uber
### Sarmiento, Kim (2025) -- Journal of Urban Economics

This archive contains all data required to replicate the paper. The code is available at:

> <https://github.com/gsaabogado/ridehailing-airquality-jue>

---

## Directory Structure

```
02_data/                                  # Raw / source data
├── 05_shp/                               #   Shapefiles
│   ├── 02_states/                        #     State boundaries (Census TIGER)
│   └── 03_counties/                      #     County boundaries (Census TIGER)
└── 06_other/                             #   Miscellaneous source data
    ├── 01_pti/                           #     Public transit index (ACS)
    ├── 2_NHTS/                           #     National Household Travel Survey
    ├── 4_Uber_entry_dates/               #     Additional Uber launch dates
    ├── 6_Gasoline_price_data_states/     #     State gasoline prices (EIA)
    ├── 8_Power_generation_states/        #     Power generation by fuel (EIA-923)
    ├── 9_TRI/                            #     Toxics Release Inventory (EPA)
    ├── 10_vehicle_california/            #     California vehicle registration (CEC)
    ├── 13_NYC_taxi_2010_2017/            #     NYC taxi daily trip counts (TLC)
    ├── 15_california_gas_consumption/    #     California gasoline consumption (CEC)
    └── 20_NYC_time_series/               #     NYC taxi extended time series (TLC)

03_gen/                                   # Constructed / generated data
├── 01_aqi/                               #   Daily AQI panels
│   ├── daily_aqi.rds
│   └── daily_events.rds
├── 02_weather/                           #   ERA5 county-level weather
│   └── full_era5.rds                     #     (~1.4 GB)
├── 03_uber/                              #   Uber entry dates and market data
│   ├── uber.rds
│   ├── uber_x.rds
│   └── macro.rds
├── 04_reg/                               #   Regression-ready panel datasets
│   ├── yearly_reg.rds
│   ├── daily_reg.rds
│   ├── yearly_rolling_reg.rds
│   ├── year_monthly_reg.rds
│   ├── yearly_weekday_reg.rds
│   └── sat_reg_test.rds
├── 05_results/                           #   Saved regression result objects
│   ├── twfe.rds, twfe_dynamic.rds, ...
│   ├── sadd.rds, sadd_dynamic.rds, ...
│   ├── etwfe.rds, etwfe_dynamic.rds, ...
│   ├── sdid.rds, sdid_alerts.rds, ...
│   └── (24 .rds files total)
└── 09_revision/                          #   Revision-specific outputs
    ├── etwfe_cohort_weights.rds
    ├── sadd_cohort_weights.rds
    └── twfe_cohort_weights.rds
```

---

## Raw Data Sources (`02_data/`)

### Shapefiles (`05_shp/`)
- **Source**: U.S. Census Bureau TIGER/Line Shapefiles (2018, 1:20m)
- **Files**: `cb_2018_us_state_20m.*` (states), `cb_2018_us_county_20m.*` (counties)
- **Access**: https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html

### Other Sources (`06_other/`)

| Subfolder | Source | Description |
|-----------|--------|-------------|
| `01_pti/` | ACS 5-year estimates | Public transit use index, commuting mode shares |
| `2_NHTS/` | FHWA | National Household Travel Survey |
| `4_Uber_entry_dates/` | Authors | Additional Uber launch dates (`uber_dates_addition.xlsx`) |
| `6_Gasoline_price_data_states/` | EIA | State-level gasoline prices |
| `8_Power_generation_states/` | EIA-923 | Power generation shares by fuel type (coal, natural gas) |
| `9_TRI/` | EPA | Toxics Release Inventory average total releases |
| `10_vehicle_california/` | California Energy Commission | Vehicle registration, ZEV sales shares |
| `13_NYC_taxi_2010_2017/` | NYC TLC | NYC taxi daily trip counts (2010--2017) |
| `15_california_gas_consumption/` | California Energy Commission | Gasoline consumption by city |
| `20_NYC_time_series/` | NYC TLC | NYC taxi daily time series (2009--2017) |

---

## Constructed Datasets (`03_gen/`)

### AQI Data (`01_aqi/`)

| File | Description |
|------|-------------|
| `daily_aqi.rds` | Daily county-level AQI panel |
| `daily_events.rds` | Daily AQI with event flags (forest fires, exceptional events) |

### Weather Data (`02_weather/`)

| File | Size | Description |
|------|------|-------------|
| `full_era5.rds` | ~1.4 GB | ERA5 reanalysis: daily county-level temperature, precipitation, humidity, wind, pressure |

- **Source**: ECMWF ERA5 reanalysis
- **Access**: https://cds.climate.copernicus.eu/
- **Note**: County-level aggregation uses inverse-distance weighting from ERA5 grid

### Uber Data (`03_uber/`)

| File | Description |
|------|-------------|
| `uber.rds` | Uber entry dates matched to FIPS codes |
| `uber_x.rds` | Extended Uber data with additional markets |
| `macro.rds` | County-level macro covariates (income, population, employment) |

### Regression Datasets (`04_reg/`)

| File | Size | Description |
|------|------|-------------|
| `yearly_reg.rds` | ~1.2 MB | Main panel: county-year, all AQI measures |
| `daily_reg.rds` | ~181 MB | Daily panel: county-day with weather controls |
| `yearly_rolling_reg.rds` | ~53 MB | Stacked/rolling panels for robustness |
| `year_monthly_reg.rds` | ~1.5 MB | County-year-month panel |
| `yearly_weekday_reg.rds` | ~20 MB | County-year-weekday panel |
| `sat_reg_test.rds` | ~4.8 MB | Satellite PM2.5 panel |

### Saved Results (`05_results/`)

Pre-estimated regression objects (24 `.rds` files) from `02_regressions.R` and `03_regressions_appendix.R`. These allow regenerating all figures and tables without re-running the regressions (~1--2 hours).

### Revision Outputs (`09_revision/`)

| File | Description |
|------|-------------|
| `etwfe_cohort_weights.rds` | ETWFE cohort weight estimates |
| `sadd_cohort_weights.rds` | Sun & Abraham cohort weight estimates |
| `twfe_cohort_weights.rds` | TWFE cohort weight estimates |

---

## Key Variables in `yearly_reg.rds`

| Variable | Description |
|----------|-------------|
| `fips` | County FIPS code |
| `cbsa_id` | Core-Based Statistical Area identifier |
| `year` | Calendar year |
| `enter_year` | Year Uber entered the CBSA |
| `treated` | 1 if county is in a treated CBSA |
| `treatment` | 1 if post-treatment period |
| `score` | Event time (year - enter_year) |
| `avg` | Average daily AQI |
| `max` | Maximum daily AQI |
| `p25`, `p50`, `p75` | AQI percentiles |
| `above100` | Share of days with AQI > 100 |
| `pop` | County population |
| Weather controls | Temperature, precipitation, humidity, wind, pressure polynomials |

---

## File Formats

- `.rds`: R serialized data (read with `readRDS()` or `readr::read_rds()`)
- `.csv`: Comma-separated values
- `.xlsx`: Microsoft Excel
- `.dta`: Stata data format

## License

The code is released under the MIT License. The data are subject to the terms of their original sources as cited above.
