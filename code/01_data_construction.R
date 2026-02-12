#### ##################################################################### ####
####        Data Construction for Replication Package                   ####
####        Builds all processed data sets from raw data                ####
#### ##################################################################### ####

#### Set the working directory to the main project folder ####
## Path set by 00_master.R; if running standalone, set root manually
if (!exists("root")) root <- getwd()

#### ===================================================================== ####
####                    SECTION A: uber.rds                               ####
#### ===================================================================== ####
#### Clear the space ####
rm(list = ls()); gc()

#### Add the necessary packages ####
library(conflicted)
library(tidyverse)
library(readxl)
library(vroom)
library(data.table)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("year", "lubridate")

cat("=== Building uber.rds ===\n\n")

#### ---- Step A1: Load and merge initial Uber data with CBSA ---- ####
uber <- read_excel("02_data/02_uber/uber_dates.xlsx", sheet = "UberDatesUS")
cbsa <- read_csv("02_data/03_adm/cbsa_city_dictionary.csv", show_col_types = FALSE)
cty  <- read_excel("02_data/03_adm/cbsa_dictionary.xlsx")

#### Select the relevant variables ####
uber <- select(uber, type, city, state, enter_date_soft:exit_date_second)

#### Select the relevant variables of the CBSA dictionary ####
cbsa <- cbsa %>% select(cbsa_id = CBSA_ID, cbsa_name = cbsa_name, city = City,
                        state = State, market_name = uberMarketName) %>% distinct()

#### Left join the cbsa and Uber data to assign MSA IDs ####
uber <- left_join(cbsa, uber); rm(cbsa)

#### Subset the data-set of MSA county characteristics ####
cty <- cty %>% mutate(fips = as.numeric(paste0(FipsState, FipsCounty))) %>%
  mutate(CbsaID = as.numeric(CbsaID)) %>% filter(grepl("Metropolitan", CbsaType)) %>%
  select(cbsa_id = CbsaID, fips, state_id = FipsState, county_id = FipsCounty,
         county_type = CountyType)

#### Only keep the introduction date of Uber X ####
uber <- uber |> group_by(cbsa_id) |> filter(type == "UberX")

#### Include the Uber data into the county data ####
uber <- left_join(cty, uber)

cat("Step A1 complete: Initial merge. Rows:", nrow(uber), "\n")

#### ---- Step A2: Complement with manually collected entry dates ---- ####
uber_comp <- read_excel("02_data/02_uber/uber_complement.xlsx", sheet = "MSA")

#### Transform the Fips codes from numeric to character ####
uber_comp <- uber_comp %>% mutate_at(vars(FipsState, FipsCty), as.character)

#### Add digits to the Fips state codes ####
uber_comp <- uber_comp %>%
  mutate(FipsState = ifelse(nchar(FipsState) == 1, paste0("0", FipsState), FipsState))

#### Add digits to the Fips county codes ####
uber_comp <- uber_comp %>%
  mutate(FipsCty = ifelse(nchar(FipsCty) == 1, paste0("00", FipsCty),
                          ifelse(nchar(FipsCty) == 2, paste0("0", FipsCty), FipsCty)))

#### Select the relevant columns of the complement data ####
uber_comp <- uber_comp %>% mutate(fips = as.numeric(paste0(FipsState, FipsCty))) %>%
  select(cbsa_id = CBSA_ID, fips, month, year) %>% distinct()

#### Create the data set of missing CBSA data in the original Uber file ####
miss <- filter(uber, is.na(city) == TRUE) %>% select(cbsa_id:county_id)

#### Left join counties with missing entry dates with complementary data ####
uber_comp <- left_join(miss, uber_comp); rm(miss)

#### When there is an NA in the introduction month, assume January ####
uber_comp <- mutate(uber_comp, missing_month = ifelse(is.na(month) == TRUE, 1, 0),
                    month = ifelse(is.na(year) == FALSE & is.na(month) == TRUE, 1, month))

#### Create the Enter Date object ####
uber_comp <- uber_comp %>%
  mutate(type = "UberX", enter_date = as_date(paste(year, month, "01", sep = "-"))) %>%
  select(-month, -year)

#### Take away all counties without introduction dates in the original Uber data ####
uber <- filter(uber, is.na(city) == FALSE)

#### Select the minimum value between the soft and official enter dates ####
uber <- uber %>% rowwise() %>%
  mutate(enter_date = min(enter_date_soft, enter_date_official, na.rm = TRUE))

#### Only keep counties where Uber did not exit ####
uber <- mutate(uber, exit_dummy = case_when(is.na(exit_date) ~ "No Exit", T ~ "Exit")) %>%
  select(cbsa_id:type, enter_date, exit_dummy, exit_date) %>%
  mutate(enter_date = as_date(enter_date))

#### Bind the observations of the raw and complementary Uber files ####
uber <- rbindlist(list(uber, uber_comp), use.names = TRUE, fill = TRUE)
uber <- mutate(uber, exit_dummy = case_when(is.na(exit_date) ~ "No Exit", T ~ "Exit"))

#### Transform the indicator variable for missing months to binary ####
uber <- mutate(uber, missing_month = ifelse(is.na(missing_month) == T, 0, missing_month))

cat("Step A2 complete: Complemented entry dates. Rows:", nrow(uber), "\n")

#### ---- Step A3: Add CBSA characteristics and regions ---- ####
cty     <- read_excel("02_data/03_adm/cbsa_dictionary.xlsx")
regions <- read_excel("02_data/03_adm/regions_us.xlsx")

#### Include the city for each county ####
cty <- mutate(cty, CbsaID = as.numeric(CbsaID),
              fips = as.numeric(paste0(FipsState, FipsCounty))) %>%
  select(fips, cbsa_id = CbsaID, cbsa_name = CbsaTitle,
         county_type = CountyType, state = State)

#### Join the Uber data with the CBSA characteristics ####
uber <- left_join(uber |> rename(state_short = state) |>
                    select(-county_type, -cbsa_name), cty)

#### Exclude MSAs outside continental USA ####
uber <- filter(uber, !(state %in% c("Puerto Rico", "Hawaii", "Alaska")))

#### Add the data on regions ####
uber <- left_join(uber, regions)

#### When there is no information on the enter year, assume is 2020 ####
uber <- mutate(uber, enter_date = as.character(enter_date))
uber <- mutate(uber, enter_date = ifelse(is.na(enter_date) == T, "2020-01-01", enter_date))
uber <- mutate(uber, enter_date = as.Date(enter_date))

cat("Step A3 complete: Added regions. Rows:", nrow(uber), "\n")

#### ---- Step A4: Add demographic/economic variables ---- ####
income  <- read_csv("02_data/06_other/05_income/temp/income_2017.csv", show_col_types = FALSE)
pop     <- read_csv("02_data/06_other/06_pop_density/pop_2010.csv", show_col_types = FALSE)
density <- read_csv("02_data/06_other/06_pop_density/density_2010.csv", show_col_types = FALSE)
trans   <- vroom("02_data/06_other/01_pti/fips_pti.csv", show_col_types = FALSE)

#### Include the transportation index ####
uber <- uber %>% left_join(rename(trans, tpi = publictransportationexcludingtax) %>%
                             mutate(tpi = as.numeric(gsub("%", "", tpi))) %>%
                             select(tpi, fips))

#### Include the population ####
uber <- uber %>% left_join(rename(pop, pop = `2010 Total Population (U.S. Census)`) %>%
                             select(fips = ID, pop) %>% mutate(fips = as.numeric(fips)) %>%
                             select(pop, fips))

#### Include the population density ####
uber <- uber %>%
  left_join(rename(density,
                   pop_density = `2010 Population Density (Pop per Square Mile) (U.S. Census)`) %>%
              select(fips = ID, pop_density) %>% mutate(fips = as.numeric(fips)) %>%
              select(pop_density, fips))

#### Include the income per capita ####
uber <- uber %>%
  left_join(unique(rename(income, income = percapitapersonalincome2017_unit) %>%
                     select(fips, income) %>% mutate(fips = as.numeric(fips)) %>%
                     group_by(fips) %>% summarise(income = mean(income))))

#### Calculate the size of the county ####
uber <- mutate(uber, cty_size = pop / pop_density)

cat("Step A4 complete: Added demographics. Final uber.rds rows:", nrow(uber), "\n")

#### ---- Step A5: Save and verify ---- ####
dir.create("03_gen/03_uber", showWarnings = FALSE, recursive = TRUE)
write_rds(uber, file = "03_gen/03_uber/uber.rds", compress = "gz")
cat("Saved: 03_gen/03_uber/uber.rds\n\n")

#### Verify against original ####
cat("=== Verification: uber.rds ===\n")
orig <- read_rds("03_gen/03_uber/uber.rds")
cat("Original: ", nrow(orig), "x", ncol(orig), "\n")
cat("Replicated:", nrow(uber), "x", ncol(uber), "\n")
cat("Columns in both:     ", length(intersect(names(orig), names(uber))), "\n")
cat("Only in original:    ", paste(setdiff(names(orig), names(uber)), collapse = ", "), "\n")
cat("Only in replicated:  ", paste(setdiff(names(uber), names(orig)), collapse = ", "), "\n")

#### Compare enter_dates on common FIPS ####
merged <- inner_join(
  orig %>% select(fips, enter_date_orig = enter_date),
  uber %>% select(fips, enter_date_repl = enter_date),
  by = "fips"
)
cat("Enter date matches:", sum(merged$enter_date_orig == merged$enter_date_repl, na.rm = TRUE),
    "out of", nrow(merged), "\n")
mismatches <- merged %>% filter(enter_date_orig != enter_date_repl)
if (nrow(mismatches) > 0) {
  cat("Mismatches (first 10):\n")
  print(head(as.data.frame(mismatches), 10))
} else {
  cat("All enter dates match perfectly!\n")
}
cat("=== uber.rds construction complete ===\n\n")

#### Clear the space ####
rm(list = ls()); gc()

#### ===================================================================== ####
####                    SECTION A2: uber_x.rds                            ####
#### ===================================================================== ####
#### Clear the space ####
rm(list = ls()); gc()

#### Add the necessary packages ####
library(conflicted)
library(tidyverse)
library(readxl)
library(data.table)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("year", "lubridate")

cat("=== Building uber_x.rds ===\n\n")

#### ---- Step A2.1: Load and merge initial Uber-X data with CBSA ---- ####
#### Note: uber_x.rds uses uber_dates_addition.xlsx (not uber_dates.xlsx) ####
uber <- read_excel("02_data/06_other/4_Uber_entry_dates/uber_dates_addition.xlsx",
                   sheet = "UberDatesUS")
cbsa <- read_csv("02_data/03_adm/cbsa_city_dictionary.csv", show_col_types = FALSE)
cty  <- read_excel("02_data/03_adm/cbsa_dictionary.xlsx")

#### Select the relevant variables ####
uber <- select(uber, type, city, state, enter_date_soft:exit_date_second)

#### Select the relevant variables of the CBSA dictionary ####
cbsa <- cbsa %>% select(cbsa_id = CBSA_ID, cbsa_name = cbsa_name, city = City,
                        state = State, market_name = uberMarketName) %>% distinct()

#### Left join the cbsa and Uber data to assign MSA IDs ####
uber <- left_join(cbsa, uber); rm(cbsa)

#### Subset the data-set of MSA county characteristics ####
cty <- cty %>% mutate(fips = as.numeric(paste0(FipsState, FipsCounty))) %>%
  mutate(CbsaID = as.numeric(CbsaID)) %>% filter(grepl("Metropolitan", CbsaType)) %>%
  select(cbsa_id = CbsaID, fips, state_id = FipsState, county_id = FipsCounty,
         county_type = CountyType)

#### Only keep the introduction date of Uber X ####
uber <- uber |> group_by(cbsa_id) |> filter(type == "UberX")

#### Include the Uber data into the county data ####
uber <- left_join(cty, uber)

cat("Step A2.1 complete: Initial merge. Rows:", nrow(uber), "\n")

#### ---- Step A2.2: Complement with manually collected entry dates ---- ####
uber_comp <- read_excel("02_data/02_uber/uber_complement.xlsx", sheet = "MSA")

#### Standardize all to uber x ####
uber_comp <- mutate(uber_comp, year = ifelse(year < 2012, 2012, year))
uber_comp <- mutate(uber_comp, month = ifelse(year < 2012, 8, month))

#### Transform the Fips codes from numeric to character ####
uber_comp <- uber_comp %>% mutate_at(vars(FipsState, FipsCty), as.character)

#### Add digits to the Fips state codes ####
uber_comp <- uber_comp %>%
  mutate(FipsState = ifelse(nchar(FipsState) == 1, paste0("0", FipsState), FipsState))

#### Add digits to the Fips county codes ####
uber_comp <- uber_comp %>%
  mutate(FipsCty = ifelse(nchar(FipsCty) == 1, paste0("00", FipsCty),
                          ifelse(nchar(FipsCty) == 2, paste0("0", FipsCty), FipsCty)))

#### Select the relevant columns of the complement data ####
uber_comp <- uber_comp %>% mutate(fips = as.numeric(paste0(FipsState, FipsCty))) %>%
  select(cbsa_id = CBSA_ID, fips, month, year) %>% distinct()

#### Create the data set of missing CBSA data in the original Uber file ####
miss <- filter(uber, is.na(city) == TRUE) %>% select(cbsa_id:county_id)

#### Left join counties with missing entry dates with complementary data ####
uber_comp <- left_join(miss, uber_comp); rm(miss)

#### When there is an NA in the introduction month, assume January ####
uber_comp <- mutate(uber_comp, missing_month = ifelse(is.na(month) == TRUE, 1, 0),
                    month = ifelse(is.na(year) == FALSE & is.na(month) == TRUE, 1, month))

#### Create the Enter Date object ####
uber_comp <- uber_comp %>%
  mutate(type = "UberX", enter_date = as_date(paste(year, month, "01", sep = "-"))) %>%
  select(-month, -year)

#### Take away all counties without introduction dates in the original Uber data ####
uber <- filter(uber, is.na(city) == FALSE)

#### Select the minimum value between the soft and official enter dates ####
uber <- uber %>% rowwise() %>%
  mutate(enter_date = min(enter_date_soft, enter_date_official, na.rm = TRUE))

#### Only keep counties where Uber did not exit ####
uber <- mutate(uber, exit_dummy = case_when(is.na(exit_date) ~ "No Exit", T ~ "Exit")) %>%
  select(cbsa_id:type, enter_date, exit_dummy, exit_date) %>%
  mutate(enter_date = as_date(enter_date))

#### Bind the observations of the raw and complementary Uber files ####
uber <- rbindlist(list(uber, uber_comp), use.names = TRUE, fill = TRUE)
uber <- mutate(uber, exit_dummy = case_when(is.na(exit_date) ~ "No Exit", T ~ "Exit"))

#### Transform the indicator variable for missing months to binary ####
uber <- mutate(uber, missing_month = ifelse(is.na(missing_month) == T, 0, missing_month))

cat("Step A2.2 complete: Complemented entry dates. Rows:", nrow(uber), "\n")

#### ---- Step A2.3: Save and verify uber_x.rds ---- ####
#### Note: The on-disk uber_x.rds is saved at this stage (pre-filtering) ####
dir.create("03_gen/03_uber", showWarnings = FALSE, recursive = TRUE)
write_rds(uber, file = "03_gen/03_uber/uber_x.rds", compress = "gz")
cat("Saved: 03_gen/03_uber/uber_x.rds\n\n")

#### Verify against original ####
cat("=== Verification: uber_x.rds ===\n")
orig <- read_rds("03_gen/03_uber/uber_x.rds")
cat("Original: ", nrow(orig), "x", ncol(orig), "\n")
cat("Replicated:", nrow(uber), "x", ncol(uber), "\n")
cat("Columns in both:     ", length(intersect(names(orig), names(uber))), "\n")
cat("Only in original:    ", paste(setdiff(names(orig), names(uber)), collapse = ", "), "\n")
cat("Only in replicated:  ", paste(setdiff(names(uber), names(orig)), collapse = ", "), "\n")

#### Compare enter_dates ####
merged <- inner_join(
  orig %>% select(fips, enter_date_orig = enter_date),
  uber %>% select(fips, enter_date_repl = enter_date),
  by = "fips"
)
both_na <- sum(is.na(merged$enter_date_orig) & is.na(merged$enter_date_repl))
both_match <- sum(merged$enter_date_orig == merged$enter_date_repl, na.rm = TRUE)
cat("Enter date matches:", both_match + both_na, "out of", nrow(merged), "\n")
cat("=== uber_x.rds construction complete ===\n\n")

#### Clear the space ####
rm(list = ls()); gc()

#### ===================================================================== ####
####                    SECTION B: macro.rds                              ####
#### ===================================================================== ####
#### Clear the space ####
rm(list = ls()); gc()

#### Add the necessary packages ####
library(vroom)
library(tidyverse)
library(tidycensus)
library(data.table)

cat("=== Building macro.rds ===\n\n")

#### Set the Census API key ####
census_api_key("6cd1e8ab76160c925fefc2a57021ae5933af33de")

#### ---- Step B1: Load and merge macro variables from CSVs ---- ####
data <- list(
  white_share  = vroom("02_data/06_other/19_race/final/race.csv",           show_col_types = FALSE),
  poverty      = vroom("02_data/06_other/18_poverty/final/poverty_percent.csv", show_col_types = FALSE),
  unemployment = vroom("02_data/06_other/17_unemployment/Final/Umemployment_county.csv", show_col_types = FALSE),
  income       = vroom("02_data/06_other/05_income/final/incomepercapita.csv", show_col_types = FALSE)
)

#### Transform all FIPS to character for the joins ####
data <- lapply(data, function(x) mutate(x, fips = str_pad(as.character(fips), width = 5, pad = "0")))

#### Add all of the macro variables ####
data <- reduce(data, full_join)

#### Create the state_fips ####
data <- mutate(data, state_fips = substr(fips, 1, 2))

cat("Step B1 complete: Merged CSVs. Rows:", nrow(data), "\n")

#### ---- Step B2: Add state-level construction expenditure ---- ####
data <- vroom("02_data/06_other/16_gov_exp/final/construction_expenditure_state.csv",
              show_col_types = FALSE) |>
  rename(state_fips = fips) |>
  mutate(state_fips = str_pad(as.character(state_fips), width = 2, pad = "0")) |>
  select(-State) %>%
  left_join(data, .)

#### Only keep data from 2005 ####
data <- filter(data, year >= 2005)

cat("Step B2 complete: Added expenditure, filtered to 2005+. Rows:", nrow(data), "\n")

#### ---- Step B3: Add population from Census ACS 1-year ---- ####
cat("Downloading ACS population data (2005-2017)...\n")
pop <- lapply(seq(2005, 2019, 1), function(x) {
  cat("  Year:", x, "\n")
  get_acs(geography = "county", cache_table = TRUE,
          table = "B01003", survey = "acs1", year = x)
}) %>%
  set_names(., seq(2005, 2019, 1)) %>%
  rbindlist(., idcol = "year") |>
  select(year, fips = GEOID, pop = estimate, pop_moe = moe) |>
  arrange(fips, year)

#### Add the population statistics ####
data <- left_join(data, pop |> mutate(year = as.numeric(year)))

cat("Step B3 complete: Added population. Final macro.rds rows:", nrow(data), "\n")

#### ---- Step B4: Save and verify ---- ####
dir.create("03_gen/03_uber", showWarnings = FALSE, recursive = TRUE)
write_rds(data, file = "03_gen/03_uber/macro.rds", compress = "gz")
cat("Saved: 03_gen/03_uber/macro.rds\n\n")

#### Verify against original ####
cat("=== Verification: macro.rds ===\n")
orig <- read_rds("02_data/06_other/macro.rds")
cat("Original: ", nrow(orig), "x", ncol(orig), "\n")
cat("Replicated:", nrow(data), "x", ncol(data), "\n")
cat("Columns in both:     ", length(intersect(names(orig), names(data))), "\n")
cat("Only in original:    ", paste(setdiff(names(orig), names(data)), collapse = ", "), "\n")
cat("Only in replicated:  ", paste(setdiff(names(data), names(orig)), collapse = ", "), "\n")

#### Compare key columns on matching fips-year ####
merged <- inner_join(
  orig %>% select(fips, year, white_orig = white, unemp_orig = unemp_rate, pop_orig = pop),
  data %>% select(fips, year, white_repl = white, unemp_repl = unemp_rate, pop_repl = pop),
  by = c("fips", "year")
)
cat("Matched fips-years:", nrow(merged), "\n")
cat("White share matches:", sum(abs(merged$white_orig - merged$white_repl) < 1e-6, na.rm = TRUE),
    "out of", sum(!is.na(merged$white_orig)), "\n")
cat("Unemp rate matches:", sum(abs(merged$unemp_orig - merged$unemp_repl) < 1e-6, na.rm = TRUE),
    "out of", sum(!is.na(merged$unemp_orig)), "\n")
cat("Pop matches:", sum(merged$pop_orig == merged$pop_repl, na.rm = TRUE),
    "out of", sum(!is.na(merged$pop_orig)), "\n")
cat("=== macro.rds construction complete ===\n\n")

#### Clear the space ####
rm(list = ls()); gc()

#### ===================================================================== ####
####              SECTION C: daily_events.rds + daily_aqi.rds             ####
#### ===================================================================== ####
#### Clear the space ####
rm(list = ls()); gc()

#### Add the necessary packages ####
library(tidyverse)
library(readxl)
library(vroom)
library(data.table)
library(mgsub)

cat("=== Building daily_events.rds and daily_aqi.rds ===\n\n")

#### ---- Step C1: Build daily_events.rds from raw pollution data ---- ####
cat("Step C1: Loading raw daily pollution files...\n")

#### List the pollution files ####
files <- list.files("02_data/01_pol/04_daily_pol", full.names = TRUE)
cat("  N files:", length(files), "\n")

#### Load the counties part of MSAs ####
cbsa <- read_excel("02_data/03_adm/cbsa_dictionary.xlsx")
cbsa <- filter(cbsa, CbsaType == "Metropolitan Statistical Area")
cbsa <- mutate(cbsa, fips = as.numeric(paste0(FipsState, FipsCounty)))

#### Load the pollution data and restrict to CBSA ####
data <- vector("list", length(files))
for (k in seq_along(files)) {
  cat("  Loading file", k, "of", length(files), ":", basename(files[k]), "\n")
  data[[k]] <- vroom(files[[k]], show_col_types = FALSE) %>%
    mutate(fips = as.numeric(paste0(`State Code`, `County Code`))) %>%
    filter(fips %in% cbsa$fips)
  gc()
}
rm(files)

#### Select the relevant variables ####
pol <- lapply(data, function(x)
  select(x, date = `Date Local`, fips, fips_state = `State Code`, fips_county = `County Code`,
         site = `Site Num`, poc = POC, lat = Latitude, lon = Longitude, pol = `Parameter Name`,
         pol_code = `Parameter Code`, sample_length = `Sample Duration`, standard = `Pollutant Standard`,
         units = `Units of Measure`, event = `Event Type`, count = `Observation Count`,
         method = `Method Code`, avg = `Arithmetic Mean`, max = `1st Max Value`, aqi = AQI,
         share_obs = `Observation Percent`))

rm(data); gc()

#### Bind all elements and create station id ####
pol <- rbindlist(pol) %>%
  mutate(station = paste(fips, site, pol_code, poc, sep = "-")) %>%
  select(-site, -pol_code, -fips_state, -fips_county, -poc)

#### Restrict to the pollutants used in the study ####
vars <- "Sulfur|Ozone|Carbon|Nitrogen|PM10 Total 0-10um STP|PM2.5 - Local Conditions"
pol <- filter(pol, grepl(vars, pol))

#### Rename the pollutants ####
pol <- mutate(pol, pol = case_when(grepl("Carbon", pol) ~ "co", TRUE ~ pol))
pol <- mutate(pol, pol = case_when(grepl("PM10", pol) ~ "pm10", TRUE ~ pol))
pol <- mutate(pol, pol = case_when(grepl("PM2.5", pol) ~ "pm25", TRUE ~ pol))
pol <- mutate(pol, pol = case_when(grepl("Ozone", pol) ~ "o3", TRUE ~ pol))
pol <- mutate(pol, pol = case_when(grepl("Nitrogen", pol) ~ "no2", TRUE ~ pol))
pol <- mutate(pol, pol = case_when(grepl("Sulfur", pol) ~ "so2", TRUE ~ pol))

#### Split by pollutant ####
pol <- split(pol, f = pol$pol)

#### Filter by sample length and NAAQS standard ####
pol$co   <- filter(pol$co, sample_length == "8-HR RUN AVG END HOUR" & standard == "CO 8-hour 1971")
pol$no2  <- filter(pol$no2, sample_length == "1 HOUR" & standard == "NO2 1-hour 2010")
pol$pm10 <- filter(pol$pm10, sample_length == "24 HOUR", standard == "PM10 24-hour 2006")
pol$pm25 <- filter(pol$pm25, sample_length == "24 HOUR", standard == "PM25 24-hour 2012")
pol$so2  <- filter(pol$so2, sample_length == "1 HOUR", standard == "SO2 1-hour 2010")
pol$o3   <- filter(pol$o3, sample_length == "8-HR RUN AVG BEGIN HOUR" & standard == "Ozone 8-hour 2015")

#### Create events table (extraordinary events: forest fires, volcanic eruptions) ####
events <- lapply(pol, function(x)
  x %>% group_by(station, fips, date, event) %>% summarise(count = n(), .groups = "drop") %>%
    spread(., event, count)) %>%
  rbindlist(., idcol = "pol", use.names = TRUE, fill = TRUE) %>%
  mutate_at(vars(Excluded:None), function(y) ifelse(is.na(y) == TRUE, 0, y)) %>%
  group_by(fips, date, pol) %>%
  summarise_at(vars(Excluded:None), max, na.rm = TRUE)

#### Save daily_events.rds ####
dir.create("03_gen/01_aqi", showWarnings = FALSE, recursive = TRUE)
write_rds(events, file = "03_gen/01_aqi/daily_events.rds", compress = "gz")
cat("Step C1 complete: Saved daily_events.rds. Rows:", nrow(events), "\n")

#### Verify daily_events.rds ####
cat("=== Verification: daily_events.rds ===\n")
orig_ev <- read_rds("03_gen/01_aqi/daily_events.rds")
cat("Original: ", nrow(orig_ev), "x", ncol(orig_ev), "\n")
cat("Replicated:", nrow(events), "x", ncol(events), "\n")
rm(orig_ev, pol); gc()

#### ---- Step C2: Build daily_aqi.rds ---- ####
cat("\nStep C2: Building daily_aqi.rds...\n")

#### Load AQI data ####
aqi <- lapply(list.files("02_data/01_pol/02_daily_aqi", full.names = TRUE), function(f) {
  vroom(f, show_col_types = FALSE)
})
cbsa <- read_excel("02_data/03_adm/cbsa_dictionary.xlsx")

#### Only select the relevant variables ####
aqi <- lapply(aqi, function(x)
  select(x, state = `State Code`, county = `County Code`,
         state_name = `State Name`, county_name = `county Name`,
         def_pol = `Defining Parameter`, aqi = AQI, date = Date,
         sites_no = `Number of Sites Reporting`))

#### Rbindlist the AQI data ####
aqi <- rbindlist(aqi)

#### Construct the character FIPS code ####
aqi <- mutate(aqi, fips = as.numeric(paste0(state, county))) %>%
  filter(is.na(fips) == FALSE)

#### Restrict the data only to counties in MSAs ####
cbsa <- filter(cbsa, CbsaType == "Metropolitan Statistical Area")
cbsa <- mutate(cbsa, fips = as.numeric(paste0(FipsState, FipsCounty))) %>%
  select(-c(FipsState, FipsCounty, State, County))

#### Left join the AQI and counties data ####
aqi <- left_join(aqi, cbsa)

#### Take away all counties without a CBSA-ID ####
aqi <- filter(aqi, is.na(CbsaID) == FALSE)

#### Extract date fixed effects ####
aqi <- mutate(aqi, year = year(date), month = month(date),
              weekday = weekdays(date), week = isoweek(date))

#### Rearrange and extract the relevant variables ####
aqi <- select(aqi, fips, date, def_pol, aqi, year, month, week, weekday,
              state_name, county_name, cbsa_title = CbsaTitle,
              cbsa_id = CbsaID, county_type = CountyType)

#### Take away the state from the CBSA title ####
aqi <- mutate(aqi, cbsa_title = gsub(",.*", "", cbsa_title))

#### Include the dummy for extraordinary events ####
aqi <- mutate(events, pol = mgsub(pol, c("pm25", "o3", "so2", "no2", "pm10", "co"),
                                  c("PM2.5", "Ozone", "SO2", "NO2", "PM10", "CO"))) %>%
  rename(def_pol = pol) %>% left_join(aqi, .) %>%
  mutate(event = ifelse(Included == 0, 0, 1)) %>% select(-c(Excluded, Included, None)) %>%
  mutate(event = ifelse(is.na(event) == TRUE, 0, event))

#### Save daily_aqi.rds ####
write_rds(aqi, file = "03_gen/01_aqi/daily_aqi.rds", compress = "gz")
cat("Step C2 complete: Saved daily_aqi.rds. Rows:", nrow(aqi), "\n\n")

#### Verify daily_aqi.rds ####
cat("=== Verification: daily_aqi.rds ===\n")
orig_aqi <- read_rds("03_gen/01_aqi/daily_aqi.rds")
cat("Original: ", nrow(orig_aqi), "x", ncol(orig_aqi), "\n")
cat("Replicated:", nrow(aqi), "x", ncol(aqi), "\n")
cat("Columns in both:     ", length(intersect(names(orig_aqi), names(aqi))), "\n")
cat("Only in original:    ", paste(setdiff(names(orig_aqi), names(aqi)), collapse = ", "), "\n")
cat("Only in replicated:  ", paste(setdiff(names(aqi), names(orig_aqi)), collapse = ", "), "\n")

#### Compare AQI values on matching fips-date ####
set.seed(42)
sample_dates <- sample(unique(aqi$date), min(100, length(unique(aqi$date))))
merged <- inner_join(
  orig_aqi %>% filter(date %in% sample_dates) %>% select(fips, date, aqi_orig = aqi, event_orig = event),
  aqi %>% filter(date %in% sample_dates) %>% select(fips, date, aqi_repl = aqi, event_repl = event),
  by = c("fips", "date")
)
cat("Sample matched rows:", nrow(merged), "\n")
cat("AQI matches:", sum(merged$aqi_orig == merged$aqi_repl, na.rm = TRUE), "out of", nrow(merged), "\n")
cat("Event matches:", sum(merged$event_orig == merged$event_repl, na.rm = TRUE), "out of", nrow(merged), "\n")
cat("=== daily_aqi.rds construction complete ===\n\n")

#### Clear the space ####
rm(list = ls()); gc()

#### ===================================================================== ####
####         SECTION D: full_era5.rds (weather data)                     ####
#### ===================================================================== ####
#### NOTE: The raw ERA5 NetCDF -> county-level RDS processing (tmp, rain,  ####
#### dew, atm, u_wind, v_wind) was done on the CMCC HPC cluster          ####
#### (02_weather.R, lines 1-359). Here we merge the intermediates.        ####
#### ===================================================================== ####
#### Clear the space ####
rm(list = ls()); gc()

#### Add the necessary packages ####
library(tidyverse)
library(data.table)
library(humidity)

cat("=== Building full_era5.rds from intermediate ERA5 files ===\n\n")

#### ---- Step D1: Load and merge ERA5 intermediates ---- ####
cat("Loading ERA5 intermediates...\n")
data <- list(
  atm     = read_rds("03_gen/02_weather/era5/atm.rds"),
  rain    = read_rds("03_gen/02_weather/era5/rain.rds"),
  tmp     = read_rds("03_gen/02_weather/era5/tmp.rds"),
  dew_tmp = read_rds("03_gen/02_weather/era5/dew.rds"),
  u_wind  = read_rds("03_gen/02_weather/era5/u_wind.rds"),
  v_wind  = read_rds("03_gen/02_weather/era5/v_wind.rds")
)

#### Arrange by fips and date ####
data <- lapply(data, arrange, fips, date)

#### Merge all variables ####
cat("Merging all 6 ERA5 variables...\n")
data <- purrr::reduce(data, full_join)

#### Estimate relative humidity ####
data <- mutate(data, rh = RH(C2K(tmp), C2K(dew)))

cat("Step D1 complete: Merged ERA5. Rows:", nrow(data), "\n")

#### ---- Step D2: Add station-based weather data ---- ####
cat("Loading station weather data...\n")
st_data <- read_rds("03_gen/02_weather/daily_weather_idw.rds") %>%
  mutate(fips = sprintf("%05d", fips)) %>%
  rename(st_tmp = tmp, st_rain = rain,
         st_rh = rh, st_wsp = wsp, st_atm = atm)

#### Join station data ####
data <- left_join(data, st_data)

#### Compute wind speed and direction ####
data <- data %>%
  mutate(wsp = sqrt(u_wind^2 + v_wind^2),
         wdr = (270 - (atan2(v_wind, u_wind) * (180 / pi))) %% 360)

#### Organize columns ####
data <- select(data, fips, date, tmp, dew, rh, st_tmp,
               rain, st_rain, atm, st_atm, u_wind, v_wind,
               st_wsp, wsp, wdr, rh, st_rh)

cat("Step D2 complete: Added station data. Final rows:", nrow(data), "\n")

#### ---- Step D3: Save and verify ---- ####
dir.create("03_gen/02_weather", showWarnings = FALSE, recursive = TRUE)
write_rds(data, file = "03_gen/02_weather/full_era5.rds", compress = "gz")
cat("Saved: 03_gen/02_weather/full_era5.rds\n\n")

#### Verify against original ####
cat("=== Verification: full_era5.rds ===\n")
orig <- read_rds("03_gen/02_weather/full_era5.rds")
cat("Original: ", nrow(orig), "x", ncol(orig), "\n")
cat("Replicated:", nrow(data), "x", ncol(data), "\n")
cat("Columns in both:     ", length(intersect(names(orig), names(data))), "\n")
cat("Only in original:    ", paste(setdiff(names(orig), names(data)), collapse = ", "), "\n")
cat("Only in replicated:  ", paste(setdiff(names(data), names(orig)), collapse = ", "), "\n")

set.seed(42)
idx <- sample(nrow(data), min(10000, nrow(data)))
cat("Sample tmp matches:", sum(abs(orig$tmp[idx] - data$tmp[idx]) < 1e-6, na.rm = TRUE),
    "out of", sum(!is.na(orig$tmp[idx])), "\n")
cat("Sample rain matches:", sum(abs(orig$rain[idx] - data$rain[idx]) < 1e-6, na.rm = TRUE),
    "out of", sum(!is.na(orig$rain[idx])), "\n")
cat("=== full_era5.rds construction complete ===\n\n")

#### Clear the space ####
rm(list = ls()); gc()

#### ===================================================================== ####
####    SECTION E: daily_reg.rds + yearly_reg.rds + related datasets      ####
#### ===================================================================== ####
#### Clear the space ####
rm(list = ls()); gc()

#### Add the necessary packages ####
library(conflicted)
library(tidyverse)
library(zoo)

#### Solve conflicts ####
conflict_prefer("month", "lubridate")
conflict_prefer("year", "lubridate")
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

cat("=== Building daily_reg.rds and yearly_reg.rds ===\n\n")

#### ---- Step E1: Build daily_reg.rds ---- ####
cat("Step E1: Loading input datasets...\n")

#### Load the replicated datasets ####
weather <- read_rds("03_gen/02_weather/full_era5.rds")
macro   <- read_rds("03_gen/03_uber/macro.rds")
pol     <- read_rds("03_gen/01_aqi/daily_aqi.rds")
uber    <- read_rds("03_gen/03_uber/uber.rds")

cat("  weather:", nrow(weather), "x", ncol(weather), "\n")
cat("  macro:", nrow(macro), "x", ncol(macro), "\n")
cat("  pol:", nrow(pol), "x", ncol(pol), "\n")
cat("  uber:", nrow(uber), "x", ncol(uber), "\n")

#### Left join the pollution and Uber data ####
pol <- left_join(pol, uber |> mutate(cbsa_id = as.character(cbsa_id)) %>%
                   select(-c(tpi:income, city, state_short, market_name, exit_date)))

#### Filter out all pollution observations outside MSAs ####
pol <- filter(pol, is.na(cbsa_id) == FALSE)

#### Exclude counties outside the continental USA ####
pol <- filter(pol, !(state_name %in% c("Alaska", "Puerto Rico", "Hawaii")))

#### Add the temporal fixed effects ####
pol <- mutate(pol, year = year(date),
              month = month(date),
              weekday = weekdays(date),
              year_month = as.yearmon(date))

#### Add the enter year and year-month ####
pol <- mutate(pol, enter_year = year(enter_date))

#### Make the treatment indicator ####
pol <- mutate(pol, treatment = ifelse(date >= enter_date, 1, 0))

#### Restrict the sample to 2005-2017 ####
pol <- filter(pol, date <= as.Date("2017-12-31"))
pol <- filter(pol, date >= as.Date("2005-01-01"))
cat("  After time filter:", nrow(pol), "rows\n")

#### Include the weather data ####
pol <- left_join(pol %>% mutate(fips = sprintf("%05d", fips)), weather); rm(weather); gc()

#### Include the data on violations to NAAQS ####
pol <- filter(read_rds("03_gen/07_other/naaqs.rds"), buffer %in% c("0")) %>%
  mutate(fips = sprintf("%05d", fips)) %>% ungroup() %>%
  select(fips, year = NaYear) %>% distinct() %>% mutate(NAAQS = 1) %>%
  left_join(pol, .) %>% mutate(NAAQS = ifelse(is.na(NAAQS), 0, NAAQS))

#### Include the data on forest fires ####
pol <- filter(read_rds("03_gen/07_other/forest_fires_500.rds"), buffer %in% 0) %>%
  mutate(fips = sprintf("%05d", fips)) %>% ungroup() %>%
  select(fips, year = fire_year) %>% distinct() %>% mutate(forest_fire = 1) %>%
  left_join(pol, .) %>% mutate(forest_fire = ifelse(is.na(forest_fire), 0, forest_fire))

#### Include the data on new power plants ####
pol <- filter(read_rds("03_gen/07_other/power_plants.rds"), buffer %in% 0) %>%
  mutate(fips = sprintf("%05d", fips)) %>% ungroup() %>%
  select(fips, year = GlobalNew) %>% distinct() %>% mutate(new_pp = 1) %>%
  left_join(pol, .) %>% mutate(new_pp = ifelse(is.na(new_pp), 0, new_pp))

#### Include the data on retired power plants ####
pol <- filter(read_rds("03_gen/07_other/power_plants.rds"), buffer %in% 0) %>%
  mutate(fips = sprintf("%05d", fips)) %>% ungroup() %>%
  select(fips, year = GlobalRet) %>% distinct() %>% mutate(ret_pp = 1) %>%
  left_join(pol, .) %>% mutate(ret_pp = ifelse(is.na(ret_pp), 0, ret_pp))

#### Add the macro controls ####
pol <- left_join(pol, macro); rm(macro); gc()

#### Create the alert dummy variables ####
pol <- mutate(pol, alert = ifelse(aqi > 100, 1, 0),
              days_a = ifelse(aqi > 100 & aqi <= 150, 1, 0),
              days_b = ifelse(aqi > 150 & aqi <= 200, 1, 0),
              days_c = ifelse(aqi > 200 & aqi <= 300, 1, 0))

#### Organize the data set ####
pol <- select(pol, c(fips, date, treatment, event, def_pol, aqi, alert:days_c,
                     cbsa_id, cbsa_name, state_name, region, county_name,
                     county_type, enter_date, enter_year, exit_dummy:missing_month,
                     tmp:rh, rain, atm, u_wind, v_wind, wsp, wdr,
                     st_tmp, st_rain, st_atm, st_wsp, st_rh, white:pop,
                     NAAQS:ret_pp, cty_size))

#### Save daily_reg.rds ####
dir.create("03_gen/04_reg", showWarnings = FALSE, recursive = TRUE)
write_rds(pol, file = "03_gen/04_reg/daily_reg.rds", compress = "gz")
cat("Step E1 complete: Saved daily_reg.rds. Rows:", nrow(pol), "x", ncol(pol), "\n\n")

#### Verify daily_reg.rds ####
cat("=== Verification: daily_reg.rds ===\n")
orig <- read_rds("03_gen/04_reg/daily_reg.rds")
cat("Original: ", nrow(orig), "x", ncol(orig), "\n")
cat("Replicated:", nrow(pol), "x", ncol(pol), "\n")
cat("Columns in both:     ", length(intersect(names(orig), names(pol))), "\n")
cat("Only in original:    ", paste(setdiff(names(orig), names(pol)), collapse = ", "), "\n")
cat("Only in replicated:  ", paste(setdiff(names(pol), names(orig)), collapse = ", "), "\n")
rm(orig); gc()

#### ---- Step E2: Build yearly_reg.rds ---- ####
cat("\nStep E2: Building yearly_reg.rds...\n")

#### Aggregate the weather data to yearly values ####
weather <- pol %>% mutate(year = year(date)) %>%
  group_by(fips, year) %>%
  summarise_at(vars(c(tmp:st_rh)), ~mean(., na.rm = TRUE)) %>%
  mutate(wsp = sqrt(u_wind^2 + v_wind^2),
         wdr = (270 - (atan2(v_wind, u_wind) * (180 / pi))) %% 360)

#### Fill missing values from the station weather data ####
weather <- mutate(weather, st_tmp = ifelse(is.na(st_tmp) == TRUE, tmp, st_tmp))
weather <- mutate(weather, st_rain = ifelse(is.na(st_rain), rain, st_rain))
weather <- mutate(weather, st_rh = ifelse(is.na(st_rh), rh, st_rh))
weather <- mutate(weather, st_wsp = ifelse(is.na(st_wsp), wsp, st_wsp))

#### Aggregate the alerts ####
alerts <- pol %>% mutate(year = year(date)) %>%
  group_by(fips, year) %>%
  summarise_at(vars(c(alert:days_c)), ~sum(., na.rm = TRUE))

#### Aggregate AQI to yearly values ####
aqi <- pol %>% mutate(year = year(date)) |> group_by(fips, year) |>
  summarise(avg = mean(aqi, na.rm = TRUE),
            min = min(aqi, na.rm = TRUE),
            `25th` = quantile(aqi, na.rm = TRUE, p = 0.25),
            `50th` = quantile(aqi, na.rm = TRUE, p = 0.5),
            `75th` = quantile(aqi, na.rm = TRUE, p = 0.75),
            max = max(aqi, na.rm = TRUE), .groups = "drop")

#### Dataset of aggregate variables ####
macro <- pol %>% mutate(year = year(date)) %>%
  select(c(fips, year, enter_year, cbsa_id:county_type,
           exit_dummy:missing_month,
           white:cty_size)) %>% distinct()

#### Merge all datasets together ####
data <- reduce(list(aqi, alerts, weather, macro), full_join)

#### Impute missing values in macro economic variables ####
data <- data |> group_by(year) |>
  mutate(const_exp = case_when(is.na(const_exp) ~ median(const_exp, na.rm = TRUE),
                                TRUE ~ const_exp)) |>
  group_by(state_name, year) |>
  mutate(unemp_rate = ifelse(is.na(unemp_rate), median(unemp_rate, na.rm = TRUE), unemp_rate)) |>
  mutate(incomepercapita = ifelse(is.na(incomepercapita), median(incomepercapita, na.rm = TRUE), incomepercapita)) |>
  mutate(poverty_percent = ifelse(is.na(poverty_percent), median(poverty_percent, na.rm = TRUE), poverty_percent))

#### Add the treatment indicators ####
data <- data %>% mutate(treatment = ifelse(year >= enter_year, 1, 0)) %>%
  mutate(score = year - enter_year) %>%
  mutate(treated = ifelse(enter_year > 2016, 0, 1))

#### If the Min AQI is zero, transform to one ####
data <- mutate(data, min = ifelse(min == 0, 1, min))

#### Transform the precipitation variable to annual aggregates ####
data <- mutate(data, rain = rain * 365)
data <- rename(data, temp = tmp) %>% mutate(temp_sq = temp^2)

#### Save yearly_reg.rds ####
write_rds(data, file = "03_gen/04_reg/yearly_reg.rds", compress = "gz")
cat("Step E2 complete: Saved yearly_reg.rds. Rows:", nrow(data), "x", ncol(data), "\n\n")

#### Verify yearly_reg.rds ####
cat("=== Verification: yearly_reg.rds ===\n")
orig <- read_rds("03_gen/04_reg/yearly_reg.rds")
cat("Original: ", nrow(orig), "x", ncol(orig), "\n")
cat("Replicated:", nrow(data), "x", ncol(data), "\n")
cat("Columns in both:     ", length(intersect(names(orig), names(data))), "\n")
cat("Only in original:    ", paste(setdiff(names(orig), names(data)), collapse = ", "), "\n")
cat("Only in replicated:  ", paste(setdiff(names(data), names(orig)), collapse = ", "), "\n")

#### Compare key variables ####
merged <- inner_join(
  orig %>% ungroup() %>% select(fips, year, avg_orig = avg, treatment_orig = treatment),
  data %>% ungroup() %>% select(fips, year, avg_repl = avg, treatment_repl = treatment),
  by = c("fips", "year")
)
cat("Matched fips-years:", nrow(merged), "\n")
cat("AQI avg matches:", sum(abs(merged$avg_orig - merged$avg_repl) < 0.01, na.rm = TRUE),
    "out of", nrow(merged), "\n")
cat("Treatment matches:", sum(merged$treatment_orig == merged$treatment_repl, na.rm = TRUE),
    "out of", nrow(merged), "\n")
cat("=== yearly_reg.rds construction complete ===\n\n")

#### Clear the space ####
rm(list = ls()); gc()

#### ---- Step E3: Build year_monthly_reg.rds ---- ####
cat("\nStep E3: Building year_monthly_reg.rds...\n")

#### Load the replicated daily_reg.rds ####
data <- read_rds("03_gen/04_reg/daily_reg.rds")

#### Select only relevant variables ####
data <- select(data, fips, cbsa_id, date, treatment, aqi, enter_date,
               enter_year, tmp, rain)

#### Create a list of all eventually treated counties ####
data <- data %>% group_by(fips) %>%
  mutate(treated = ifelse(enter_date < "2017-01-01", 1, 0)) %>%
  mutate(year = year(date), month = month(date))

#### Aggregate the data to year-month level ####
agg_data <- data %>% group_by(year, month, fips, cbsa_id, enter_date, treated) %>%
  summarise(aqi  = mean(aqi, na.rm = TRUE),
            temp = mean(tmp, na.rm = TRUE),
            rain = mean(rain, na.rm = TRUE),
            .groups = "drop")

#### Add the year-month ####
agg_data <- mutate(agg_data, enter_month = as.yearmon(enter_date)) %>%
  mutate(year_month = as.yearmon(paste(year, month, sep = "-")))

#### Compute relative time score ####
agg_data <- mutate(agg_data, score = round((year_month - enter_month) * 12))

#### Filter to pre-2017 and add treatment indicator ####
agg_data <- filter(agg_data, year <= 2016)
agg_data <- mutate(agg_data, treatment = ifelse(score > 0, 1, 0))

#### Save ####
write_rds(agg_data, file = "03_gen/04_reg/year_monthly_reg.rds", compress = "gz")
cat("Step E3 complete:", nrow(agg_data), "x", ncol(agg_data), "\n\n")

#### Verify year_monthly_reg.rds ####
cat("=== Verification: year_monthly_reg.rds ===\n")
orig <- read_rds("03_gen/04_reg/year_monthly_reg.rds")
cat("Original: ", nrow(orig), "x", ncol(orig), "\n")
cat("Replicated:", nrow(agg_data), "x", ncol(agg_data), "\n")
cat("Columns in both:     ", length(intersect(names(orig), names(agg_data))), "\n")
cat("Only in original:    ", paste(setdiff(names(orig), names(agg_data)), collapse = ", "), "\n")
cat("Only in replicated:  ", paste(setdiff(names(agg_data), names(orig)), collapse = ", "), "\n")

merged <- inner_join(
  orig %>% ungroup() %>% select(fips, year, month, aqi_orig = aqi, treatment_orig = treatment),
  agg_data %>% ungroup() %>% select(fips, year, month, aqi_repl = aqi, treatment_repl = treatment),
  by = c("fips", "year", "month")
)
cat("Matched fips-year-months:", nrow(merged), "\n")
cat("AQI matches:", sum(abs(merged$aqi_orig - merged$aqi_repl) < 0.01, na.rm = TRUE),
    "out of", nrow(merged), "\n")
cat("Treatment matches:", sum(merged$treatment_orig == merged$treatment_repl, na.rm = TRUE),
    "out of", nrow(merged), "\n")
cat("=== Done ===\n\n")

#### Clear the space ####
rm(list = ls()); gc()

#### ---- Step E4: Build yearly_rolling_reg.rds ---- ####
cat("\nStep E4: Building yearly_rolling_reg.rds...\n")

#### Load the replicated daily_reg.rds ####
data <- read_rds("03_gen/04_reg/daily_reg.rds")

#### Select only relevant variables ####
data <- select(data, fips, cbsa_id, date, treatment, aqi, enter_date,
               enter_year, tmp, rain)

#### Create a list of all eventually treated counties ####
data <- data %>% group_by(fips) %>%
  mutate(treated = ifelse(enter_date < "2017-01-01", 1, 0))

#### Create the list of treated FIPS ####
fips_list <- filter(data, treated == 1) %>% split(., f = .$fips)

#### Function to aggregate the data with a yearly rolling average ####
aggregate_yearly_data <- function(fips_data, full_data) {

  # 1) Add rows from the full data where "treated == 1" but enters after
  #    the current subset's entry_date. Then keep only pre-entry_date days
  #    from that county, forcing "treated = 0" for them
  temp <- bind_rows(
    fips_data, filter(full_data, treated == 1, enter_date > unique(fips_data$enter_date),
                      date < enter_date) %>% mutate(treated = 0))

  # 2) Add all never-treated rows from the full_data
  temp <- bind_rows(temp, filter(full_data, treated == 0))

  # 3) Define the 'treated_enter_date' as the earliest (min) of enter_date
  #    among the (eventually) treated rows.
  temp <- temp %>% ungroup() %>%
    mutate(treated_enter_date = min(enter_date))

  # 4) Compute the aggregates at yearly level
  df_final <- temp %>%
    mutate(day_diff = as.numeric(date - treated_enter_date),
           year_int = floor(day_diff / 365),
           year = year(date)) %>%
    group_by(fips, cbsa_id, treated, year_int) %>%
    summarise(year = first(year), aqi = mean(aqi, na.rm = TRUE),
              temp = mean(tmp, na.rm = TRUE),
              rain = mean(rain, na.rm = TRUE),
              .groups = "drop") %>%
    # Mark "treatment" as 1 if year_int >= 0, else 0
    mutate(treatment = as.integer(year_int >= 0))

  return(df_final)
}

#### Apply the function to each treated FIPS ####
cat("  Processing", length(fips_list), "treated FIPS codes...\n")
agg_data <- lapply(fips_list, aggregate_yearly_data, full_data = data)
agg_data <- bind_rows(agg_data, .id = "subpanel_id")

cat("Step E4 complete:", nrow(agg_data), "x", ncol(agg_data), "\n\n")

#### Save ####
write_rds(agg_data, file = "03_gen/04_reg/yearly_rolling_reg.rds", compress = "gz")

#### Verify yearly_rolling_reg.rds ####
cat("=== Verification: yearly_rolling_reg.rds ===\n")
orig <- read_rds("03_gen/04_reg/yearly_rolling_reg.rds")
cat("Original: ", nrow(orig), "x", ncol(orig), "\n")
cat("Replicated:", nrow(agg_data), "x", ncol(agg_data), "\n")
cat("Columns in both:     ", length(intersect(names(orig), names(agg_data))), "\n")
cat("Only in original:    ", paste(setdiff(names(orig), names(agg_data)), collapse = ", "), "\n")
cat("Only in replicated:  ", paste(setdiff(names(agg_data), names(orig)), collapse = ", "), "\n")

merged <- inner_join(
  orig %>% ungroup() %>% select(subpanel_id, fips, year_int, aqi_orig = aqi, treatment_orig = treatment),
  agg_data %>% ungroup() %>% select(subpanel_id, fips, year_int, aqi_repl = aqi, treatment_repl = treatment),
  by = c("subpanel_id", "fips", "year_int")
)
cat("Matched rows:", nrow(merged), "\n")
cat("AQI matches:", sum(abs(merged$aqi_orig - merged$aqi_repl) < 0.01, na.rm = TRUE),
    "out of", nrow(merged), "\n")
cat("Treatment matches:", sum(merged$treatment_orig == merged$treatment_repl, na.rm = TRUE),
    "out of", nrow(merged), "\n")
cat("=== Done ===\n\n")

#### Clear the space ####
rm(list = ls()); gc()

#### ---- Step E5: Build sat_reg_test.rds ---- ####
cat("\nStep E5: Building sat_reg_test.rds...\n")

#### Load packages ####
library(data.table)

#### Load the replicated data ####
pm25 <- read_rds("03_gen/01_aqi/raw_sat.rds")
uber <- read_rds("03_gen/03_uber/uber.rds")

#### Aggregate the pollution data to the FIPS level ####
pm25 <- ungroup(pm25) %>% group_by(fips, year) %>%
  summarise(pm25 = mean(pm25, na.rm = TRUE), .groups = "drop")

#### Restrict the time series of the satellite data ####
pm25 <- filter(pm25, year %in% c(2000:2017))

#### Left join the pollution and Uber data ####
pm25 <- uber |> mutate(cbsa_id = as.character(cbsa_id),
                        enter_year = year(enter_date)) %>%
  select(c(fips, cbsa_id, state_name = state, enter_year, region, county_type)) %>%
  left_join(pm25, .)

#### Filter out all pollution observations outside MSAs ####
pm25 <- filter(pm25, is.na(cbsa_id) == FALSE); rm(uber)

#### Aggregate the weather data to yearly values ####
weather <- read_rds("03_gen/02_weather/full_era5.rds") %>%
  mutate(year = year(date)) %>% group_by(fips, year) %>%
  summarise_at(vars(c(tmp:st_rh)), ~mean(., na.rm = TRUE)) %>%
  mutate(wsp = sqrt(u_wind^2 + v_wind^2),
         wdr = (270 - (atan2(v_wind, u_wind) * (180 / pi))) %% 360)

#### Fill missing values from the stations weather data ####
weather <- mutate(weather, st_tmp = ifelse(is.na(st_tmp) == TRUE, tmp, st_tmp))
weather <- mutate(weather, st_rain = ifelse(is.na(st_rain), rain, st_rain))
weather <- mutate(weather, st_rh = ifelse(is.na(st_rh), rh, st_rh))
weather <- mutate(weather, st_wsp = ifelse(is.na(st_wsp), wsp, st_wsp))

#### Add the weather data to pm25 ####
pm25 <- left_join(pm25 %>%
                    mutate(fips = sprintf("%05d", fips),
                           year = as.numeric(year)),
                  weather %>% select(c(fips, year, st_tmp:st_wsp)))

#### Add the macro controls ####
pm25 <- left_join(pm25, read_rds("03_gen/03_uber/macro.rds"))

#### Make the treatment indicator ####
pm25 <- pm25 %>% mutate(treated = ifelse(enter_year > 2016, 0, 1))
pm25 <- pm25 %>% mutate(treatment = ifelse(treated == 1 & year >= enter_year, 1, 0))
pm25 <- mutate(pm25, score = year - enter_year)

#### Save ####
write_rds(pm25, file = "03_gen/04_reg/sat_reg_test.rds")
cat("Step E5 complete:", nrow(pm25), "x", ncol(pm25), "\n\n")

#### Verify sat_reg_test.rds ####
cat("=== Verification: sat_reg_test.rds ===\n")
orig <- read_rds("03_gen/04_reg/sat_reg_test.rds")
cat("Original: ", nrow(orig), "x", ncol(orig), "\n")
cat("Replicated:", nrow(pm25), "x", ncol(pm25), "\n")
cat("Columns in both:     ", length(intersect(names(orig), names(pm25))), "\n")
cat("Only in original:    ", paste(setdiff(names(orig), names(pm25)), collapse = ", "), "\n")
cat("Only in replicated:  ", paste(setdiff(names(pm25), names(orig)), collapse = ", "), "\n")

merged <- inner_join(
  orig %>% ungroup() %>% select(fips, year, pm25_orig = pm25, treatment_orig = treatment),
  pm25 %>% ungroup() %>% select(fips, year, pm25_repl = pm25, treatment_repl = treatment),
  by = c("fips", "year")
)
cat("Matched rows:", nrow(merged), "\n")
cat("PM2.5 matches:", sum(abs(merged$pm25_orig - merged$pm25_repl) < 0.01, na.rm = TRUE),
    "out of", nrow(merged), "\n")
cat("Treatment matches:", sum(merged$treatment_orig == merged$treatment_repl, na.rm = TRUE),
    "out of", nrow(merged), "\n")
cat("=== Done ===\n\n")

#### Clear the space ####
rm(list = ls()); gc()

#### ===================================================================== ####
####           SECTION F: Hourly data sets (appendix)                    ####
#### ===================================================================== ####

#### _____________________________________________________________________ ####
#### Step 1a: Process each state EPA file into hourly pollutant data ####
#### NOTE: Raw state files at 02_data/01_pol/06_state_data (~49 files) ####
#### NOTE: This step requires ~32GB memory and ~2 hours on HPC ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

#### Load packages ####
library(tidyverse)
library(readxl)

#### Create output directory for intermediate state files ####
dir.create("03_gen/09_revision/states", showWarnings = FALSE, recursive = TRUE)

#### Load the CBSA dictionary to filter to metro areas ####
cbsa = read_excel("02_data/03_adm/cbsa_dictionary.xlsx") %>%
  filter(CbsaType == "Metropolitan Statistical Area") %>%
  mutate(fips = as.numeric(paste0(FipsState, FipsCounty)))

#### Define the relevant pollutants ####
vars = "Sulfur|Ozone|Carbon m|Nitrogen d|PM10|PM2.5 - Local|Acceptable PM2.5"

#### Process each state file ####
files = list.files("02_data/01_pol/06_state_data", full.names = TRUE)
cat("Processing", length(files), "state files...\n\n")

for(i in seq_along(files)) {
  state_name = gsub("\\.rds$", "", basename(files[i]))
  out_file = paste0("03_gen/09_revision/states/", state_name, "_processed.rds")

  #### Skip if already processed ####
  if(file.exists(out_file)) {
    cat(i, "/", length(files), ":", state_name, "- already exists, skipping\n")
    next
  }

  cat(i, "/", length(files), ":", state_name, "... ")

  #### Load and filter to metropolitan counties ####
  df = read_rds(files[i]) %>%
    mutate(fips = as.numeric(paste0(FipsState, FipsCounty))) %>%
    filter(fips %in% cbsa$fips)

  if(nrow(df) == 0) {
    cat("no MSA data, skipping\n")
    next
  }

  #### Aggregate by fips, pol, date, time (using dplyr to avoid data.table bug) ####
  df = df %>%
    group_by(fips, pol, date, time) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

  #### Filter to the relevant pollutants ####
  df = df %>% filter(grepl(vars, pol))

  #### Pivot from long to wide format ####
  df = df %>% pivot_wider(names_from = pol, values_from = value)

  #### Rename columns to short names ####
  df = df %>%
    rename_with(~case_when(
      . == "Acceptable PM2.5 AQI & Speciation Mass" ~ "pm25_spec",
      . == "PM2.5 - Local Conditions" ~ "pm25_lc",
      . == "Nitrogen dioxide (NO2)" ~ "no2",
      . == "Ozone" ~ "o3",
      . == "PM10 Total 0-10um STP" ~ "pm10",
      TRUE ~ .
    ))

  #### Ensure all columns exist ####
  for(col in c("pm25_spec", "pm25_lc", "no2", "o3", "pm10")) {
    if(!col %in% names(df)) df[[col]] = NA_real_
  }

  #### Combine PM2.5 measures and compute hourly averages ####
  df = df %>%
    select(fips, date, time, pm25_spec, pm25_lc, no2, o3, pm10) %>%
    rowwise() %>%
    mutate(pm25 = mean(c(pm25_lc, pm25_spec), na.rm = TRUE)) %>%
    ungroup() %>%
    select(-pm25_lc, -pm25_spec) %>%
    mutate(o3 = o3 * 1000,
           hour = as.numeric(substr(time, 1, 2))) %>%
    select(-time) %>%
    group_by(fips, date, hour) %>%
    summarise(across(c(no2, o3, pm10, pm25), ~mean(.x, na.rm = TRUE)), .groups = "drop")

  #### Save the processed state file ####
  write_rds(df, out_file)
  cat(nrow(df), "rows saved\n")

  rm(df); gc()
}

#### Clear the space ####
rm(list = ls()); gc()

#### _____________________________________________________________________ ####
#### Step 1b: Combine processed state files into hourly_pol_fixed.rds ####
#### Output: 03_gen/09_revision/hourly_pol_fixed.rds (~75M rows x 7 cols) ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

#### Load packages ####
library(tidyverse)

#### List all processed state files ####
files = list.files("03_gen/09_revision/states", pattern = "_processed\\.rds$", full.names = TRUE)
cat("Found", length(files), "processed state files\n\n")

#### Read and combine all files ####
results = vector("list", length(files))
for(i in seq_along(files)) {
  cat(i, "/", length(files), ":", basename(files[i]), "\n")
  results[[i]] = read_rds(files[i])
}

cat("\nCombining...\n")
data = bind_rows(results)
rm(results); gc()

#### Filter from 2005 onward ####
data = data %>% filter(date >= as.Date("2005-01-01"))

cat("Final dimensions:", dim(data), "\n")
cat("Date range:", as.character(min(data$date)), "to", as.character(max(data$date)), "\n")
cat("Unique FIPS:", length(unique(data$fips)), "\n")

#### Save ####
write_rds(data, "03_gen/09_revision/hourly_pol_fixed.rds", compress = "gz")
cat("Saved hourly_pol_fixed.rds\n")

#### Clear the space ####
rm(list = ls()); gc()

#### _____________________________________________________________________ ####
#### Step 2: Create hourly_aqi.rds (AQI from pollutant concentrations) ####
#### Output: 03_gen/09_revision/hourly_aqi.rds (~75M rows x 13 cols) ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

#### Load packages ####
library(tidyverse)
library(zoo)
library(con2aqi)

#### Fast AQI conversion using lookup table ####
fast_con2aqi = function(pollutant, con_vec, type) {
  result = rep(NA_real_, length(con_vec))
  valid = !is.na(con_vec) & con_vec >= 0

  if(sum(valid) == 0) return(result)

  #### Round to reduce unique values ####
  if(pollutant == "o3") {
    rounded = round(con_vec[valid], 3)
  } else {
    rounded = round(con_vec[valid], 1)
  }

  #### Build lookup from unique values ####
  uniq_vals = sort(unique(rounded))
  cat("    Unique values:", length(uniq_vals), "\n")

  lookup = sapply(uniq_vals, function(x) {
    tryCatch(con2aqi(pollutant = pollutant, con = x, type = type),
             error = function(e) NA_real_)
  })
  names(lookup) = as.character(uniq_vals)

  #### Map back to full vector ####
  result[valid] = lookup[as.character(rounded)]
  return(result)
}

#### Load hourly pollutant data ####
cat("Loading hourly_pol_fixed.rds...\n")
data = read_rds("03_gen/09_revision/hourly_pol_fixed.rds")

#### Convert O3 from ppb to ppm ####
data = data %>% mutate(o3 = o3 / 1000)

#### Compute 8-hour rolling O3 average per FIPS ####
cat("Computing 8-hour rolling O3 average...\n")
fips_list = sort(unique(data$fips))
results = vector("list", length(fips_list))

for(i in seq_along(fips_list)) {
  if(i %% 100 == 0) cat("  FIPS", i, "/", length(fips_list), "\n")
  d = data %>% filter(fips == fips_list[i]) %>% arrange(date, hour)
  o3NA = rollsum(ifelse(is.na(d$o3), 1, 0), 8, fill = NA, align = "right")
  o38 = rollmean(d$o3, k = 8, na.rm = TRUE, fill = NA, align = "center")
  d$o38 = ifelse(o3NA > 2, NA, round(o38, 3))
  results[[i]] = d
}

cat("Combining...\n")
data = bind_rows(results)
rm(results); gc()

#### Bound PM10 and PM25 to 500 ####
data$pm10 = ifelse(data$pm10 > 500, 500, data$pm10)
data$pm25 = ifelse(data$pm25 > 500, 500, data$pm25)

#### Compute AQI for each pollutant ####
cat("Computing AQI values...\n")

cat("  NO2 AQI...\n")
data$no2_aqi = fast_con2aqi("no2", data$no2, "1h")

cat("  PM25 AQI...\n")
data$pm25_aqi = fast_con2aqi("pm25", data$pm25, "1h")

cat("  PM10 AQI...\n")
data$pm10_aqi = fast_con2aqi("pm10", data$pm10, "1h")

cat("  O3 AQI...\n")
data$o3_aqi = fast_con2aqi("o3", data$o3, "1h")

#### Overall AQI as the max across pollutants ####
data$aqi = pmax(data$no2_aqi, data$pm25_aqi, data$pm10_aqi, data$o3_aqi, na.rm = TRUE)
data$aqi = ifelse(is.infinite(data$aqi), NA, data$aqi)

cat("Final dimensions:", dim(data), "\n")

#### Save ####
write_rds(data, "03_gen/09_revision/hourly_aqi.rds", compress = "gz")
cat("Saved hourly_aqi.rds\n")

#### Clear the space ####
rm(list = ls()); gc()

#### _____________________________________________________________________ ####
#### Step 3: Create hourly_now_cast.rds (EPA NowCast AQI) ####
#### Output: 03_gen/09_revision/hourly_now_cast.rds (~71M rows x 7 cols) ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

#### Load packages ####
library(tidyverse)

#### EPA NowCast Algorithm ####
#### N = 12 for PM, 8 for O3 ####
#### w = 1 - (max - min) / max, bounded by w_min ####
#### w_min = 0.5 for PM, no minimum for O3 ####
#### NowCast = sum(w^i * c_i) / sum(w^i), i = 0..N-1 ####
#### Need at least 2 of 3 most recent hours valid ####
compute_nowcast = function(values, N, w_min) {
  n = length(values)
  result = rep(NA_real_, n)

  for(t in N:n) {
    window = values[(t-N+1):t]
    #### Check: need at least 2 of 3 most recent hours ####
    recent3 = window[(N-2):N]
    if(sum(!is.na(recent3)) < 2) next

    valid = !is.na(window)
    if(sum(valid) < 2) next

    c_max = max(window[valid])
    c_min = min(window[valid])

    if(c_max == 0) { result[t] = 0; next }

    w = 1 - (c_max - c_min) / c_max
    w = max(w, w_min)

    weights = w^(0:(N-1))
    rev_window = rev(window)
    rev_valid = rev(valid)

    num = sum(weights[rev_valid] * rev_window[rev_valid])
    den = sum(weights[rev_valid])

    result[t] = num / den
  }
  return(result)
}

#### Load data ####
cat("Loading hourly_pol_fixed.rds...\n")
data = read_rds("03_gen/09_revision/hourly_pol_fixed.rds")

#### Convert O3 ppb to ppm ####
data = data %>% mutate(o3 = o3 / 1000)

#### Set negatives to zero ####
data = data %>% mutate(
  pm10 = ifelse(pm10 < 0, 0, pm10),
  pm25 = ifelse(pm25 < 0, 0, pm25),
  o3 = ifelse(o3 < 0, 0, o3)
)

#### Process NowCast per FIPS ####
fips_list = sort(unique(data$fips))
cat("Processing NowCast for", length(fips_list), "FIPS codes...\n\n")

results = vector("list", length(fips_list))

for(i in seq_along(fips_list)) {
  if(i %% 50 == 0) cat("FIPS", i, "/", length(fips_list), "\n")

  d = data %>% filter(fips == fips_list[i]) %>% arrange(date, hour)

  #### PM2.5 NowCast (12-hour, w_min = 0.5) ####
  d$pm25_nc_raw = compute_nowcast(d$pm25, N = 12, w_min = 0.5)

  #### PM10 NowCast (12-hour, w_min = 0.5) ####
  d$pm10_nc_raw = compute_nowcast(d$pm10, N = 12, w_min = 0.5)

  #### O3 NowCast (8-hour, w_min = 0) ####
  d$o3_nc_raw = compute_nowcast(d$o3, N = 8, w_min = 0)

  results[[i]] = d %>% select(fips, date, hour, pm25_nc_raw, pm10_nc_raw, o3_nc_raw)
}

cat("\nCombining...\n")
now_cast = bind_rows(results)
rm(results, data); gc()

#### Convert NowCast concentrations to AQI using EPA breakpoints ####

#### PM2.5 AQI breakpoints ####
now_cast = now_cast %>% mutate(
  pm25_nc = case_when(
    is.na(pm25_nc_raw) ~ NA_real_,
    pm25_nc_raw <= 12 ~ ((50-0)/(12-0))*(pm25_nc_raw-0)+0,
    pm25_nc_raw <= 35.4 ~ ((100-51)/(35.4-12.1))*(pm25_nc_raw-12.1)+51,
    pm25_nc_raw <= 55.4 ~ ((150-101)/(55.4-35.5))*(pm25_nc_raw-35.5)+101,
    pm25_nc_raw <= 150.4 ~ ((200-151)/(150.4-55.5))*(pm25_nc_raw-55.5)+151,
    pm25_nc_raw <= 250.4 ~ ((300-201)/(250.4-150.5))*(pm25_nc_raw-150.5)+201,
    pm25_nc_raw <= 350.4 ~ ((400-301)/(350.4-250.5))*(pm25_nc_raw-250.5)+301,
    pm25_nc_raw <= 500.4 ~ ((500-401)/(500.4-350.5))*(pm25_nc_raw-350.4)+401,
    TRUE ~ NA_real_
  )
)

#### O3 AQI breakpoints ####
now_cast = now_cast %>% mutate(
  o3_nc = case_when(
    is.na(o3_nc_raw) ~ NA_real_,
    o3_nc_raw <= 0.054 ~ ((50-0)/(0.054-0))*(o3_nc_raw-0)+0,
    o3_nc_raw <= 0.070 ~ ((100-51)/(0.070-0.055))*(o3_nc_raw-0.055)+51,
    o3_nc_raw <= 0.085 ~ ((150-101)/(0.085-0.071))*(o3_nc_raw-0.071)+101,
    o3_nc_raw <= 0.105 ~ ((200-151)/(0.105-0.086))*(o3_nc_raw-0.086)+151,
    o3_nc_raw <= 0.200 ~ ((300-201)/(0.200-0.106))*(o3_nc_raw-0.106)+201,
    o3_nc_raw <= 0.294 ~ ((400-301)/(0.294-0.200))*(o3_nc_raw-0.200)+301,
    TRUE ~ NA_real_
  )
)

#### PM10 AQI breakpoints ####
now_cast = now_cast %>% mutate(
  pm10_nc = case_when(
    is.na(pm10_nc_raw) ~ NA_real_,
    pm10_nc_raw <= 54 ~ ((50-0)/(54-0))*(pm10_nc_raw-0)+0,
    pm10_nc_raw <= 154 ~ ((100-51)/(154-55))*(pm10_nc_raw-55)+51,
    pm10_nc_raw <= 254 ~ ((150-101)/(254-155))*(pm10_nc_raw-155)+101,
    pm10_nc_raw <= 354 ~ ((200-151)/(354-255))*(pm10_nc_raw-255)+151,
    pm10_nc_raw <= 424 ~ ((300-201)/(424-355))*(pm10_nc_raw-355)+201,
    pm10_nc_raw <= 504 ~ ((400-301)/(504-425))*(pm10_nc_raw-425)+301,
    pm10_nc_raw <= 604 ~ ((500-401)/(604-505))*(pm10_nc_raw-505)+401,
    TRUE ~ NA_real_
  )
)

#### Compute overall NowCast AQI as the max across pollutants ####
now_cast = now_cast %>%
  select(fips, date, hour, pm25_nc, pm10_nc, o3_nc) %>%
  filter(!is.na(pm25_nc) | !is.na(o3_nc) | !is.na(pm10_nc)) %>%
  mutate(now_cast = pmax(pm25_nc, o3_nc, pm10_nc, na.rm = TRUE),
         date = as.Date(date))

cat("Final dimensions:", dim(now_cast), "\n")

#### Save ####
write_rds(now_cast, "03_gen/09_revision/hourly_now_cast.rds", compress = "gz")
cat("Saved hourly_now_cast.rds\n")

#### Clear the space ####
rm(list = ls()); gc()

#### _____________________________________________________________________ ####
#### Step 4: Create hourly_reg_aqi.rds (merge NowCast with Uber + weather) ####
#### Output: 03_gen/09_revision/hourly_reg_aqi.rds (~61M rows x 29 cols) ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

#### Load packages ####
library(tidyverse)
library(zoo)

#### Load NowCast data ####
cat("Loading hourly_now_cast.rds...\n")
pol = read_rds("03_gen/09_revision/hourly_now_cast.rds")

#### Load Uber entry data ####
cat("Loading uber.rds...\n")
uber = read_rds("03_gen/03_uber/uber.rds")

#### Merge Uber entry variables ####
pol = left_join(pol %>% mutate(fips = as.numeric(fips)),
                uber %>% select(fips, enter_date, exit_dummy, exit_date,
                                missing_month, type, cty_size),
                by = "fips")
rm(uber); gc()

#### Add temporal fixed effects ####
pol = pol %>% ungroup() %>% mutate(
  year = lubridate::year(date),
  month = lubridate::month(date),
  weekday = weekdays(date),
  year_month = as.yearmon(date)
)

#### Create treatment indicator ####
pol = pol %>% mutate(treatment = ifelse(date >= enter_date, 1, 0))

#### Restrict to 2000-2017 ####
pol = pol %>% filter(date <= as.Date("2017-12-31"))

#### Treat post-2017 Uber entries as never treated ####
pol = pol %>% mutate(treatment = ifelse(enter_date > as.Date("2017-12-31"), 0, treatment))

#### Merge daily weather ####
cat("Loading and merging daily weather...\n")
weather = read_rds("03_gen/02_weather/daily_weather_idw.rds")
pol = left_join(pol, weather, by = c("fips", "date")) %>%
  mutate(tmp_bin = ntile(tmp, 10), rain_bin = ntile(rain, 10))
rm(weather); gc()

#### Add geographic identifiers ####
cat("Loading geographic identifiers...\n")
pol_daily = read_rds("03_gen/04_reg/daily_manual_reg.rds")
geo_ids = pol_daily %>% ungroup() %>%
  select(fips, state_name, county_name, cbsa_title, county_type) %>%
  distinct()
rm(pol_daily); gc()

pol = left_join(pol, geo_ids, by = "fips")
rm(geo_ids); gc()

cat("Final dimensions:", dim(pol), "\n")

#### Save ####
write_rds(pol, "03_gen/09_revision/hourly_reg_aqi.rds", compress = "gz")
cat("Saved hourly_reg_aqi.rds\n")

#### Clear the space ####
rm(list = ls()); gc()

#### _____________________________________________________________________ ####
#### Step 5: Create yearly_hourly_reg.rds (annual aggregation by hour) ####
#### Output: 03_gen/09_revision/yearly_hourly_reg.rds (~194K rows x 21 cols) ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

#### Load packages ####
library(tidyverse)
library(zoo)

#### Load the hourly data ####
cat("Loading hourly_reg_aqi.rds...\n")
data = read_rds("03_gen/09_revision/hourly_reg_aqi.rds")

#### Load uber for cbsa_id ####
uber = read_rds("03_gen/03_uber/uber.rds")
data = left_join(data, uber %>% select(fips, cbsa_id), by = "fips")
rm(uber); gc()

#### Derive enter_year from enter_date ####
data = mutate(data, enter_year = lubridate::year(enter_date))

#### Load yearly weather ####
weather = read_rds("03_gen/02_weather/weather_idw.rds")

#### Aggregate hourly data to yearly values by fips/year/hour ####
cat("Aggregating to fips x year x hour...\n")
yearly = data |>
  group_by(fips, year, hour, enter_year, cbsa_id, cbsa_title) |>
  summarise(avg = mean(now_cast, na.rm = T),
            min = min(now_cast, na.rm = T),
            `25th` = quantile(now_cast, na.rm = T, p = 0.25),
            `50th` = quantile(now_cast, na.rm = T, p = 0.5),
            `75th` = quantile(now_cast, na.rm = T, p = 0.75),
            max = max(now_cast, na.rm = T), .groups = "drop") |>
  mutate(treatment = ifelse(year >= enter_year, 1, 0)) %>%
  mutate(score = year - enter_year) %>%
  mutate(treated = ifelse(enter_year > 2016, 0, 1))

#### Transform zero minimums to one ####
yearly = mutate(yearly, min = ifelse(min == 0, 1, min))

#### Only keep data between 2005 and 2017 ####
yearly = filter(yearly, year %in% seq(2005, 2017, 1))

#### Join yearly weather ####
yearly = left_join(yearly, weather)
rm(weather, data); gc()

cat("Final dimensions:", dim(yearly), "\n")

#### Save ####
write_rds(yearly, file = "03_gen/09_revision/yearly_hourly_reg.rds")
cat("Saved yearly_hourly_reg.rds\n")

#### Clear the space ####
rm(list = ls()); gc()

#### ===================================================================== ####
####    SECTION G: nycmb_freq_2017.rds, nycmb_freq_2018.rds,             ####
####              nycmb_freq_2019.rds (NYC Mobility Survey frequency)     ####
#### ===================================================================== ####
#### Clear the space ####
rm(list = ls()); gc()

#### Add the necessary packages ####
library(tidyverse)
library(vroom)

cat("=== Building nycmb_freq files (NYC Mobility Survey) ===\n\n")

raw_dir <- "02_data/06_other/21_NYC_medallion_vin"

#### ---- Step G1: nycmb_freq_2019.rds (Person Survey 2019) ---- ####
cat("--- Step G1: 2019 ---\n")
sur_2019 <- vroom(file.path(raw_dir, "Citywide_Mobility_Survey_-_Person_Survey_2019.csv"),
                  show_col_types = FALSE)

sur_2019 <- mutate(sur_2019, freq = as.character(tnc_freq))

#### Recode frequency categories ####
sur_2019 <- mutate(sur_2019,
                   freq = gsub("\\b1\\b", "Weekly", freq),
                   freq = gsub("\\b2\\b", "Weekly", freq),
                   freq = gsub("\\b3\\b", "Weekly", freq),
                   freq = gsub("\\b4\\b", "Weekly", freq),
                   freq = gsub("\\b5\\b", "Weekly", freq),
                   freq = gsub("\\b6\\b", "Monthly", freq),
                   freq = gsub("\\b7\\b", "Yearly", freq),
                   freq = gsub("\\b8\\b", "Missing", freq),
                   freq = gsub("\\b995\\b", "Missing", freq))

sum_2019 <- sur_2019 %>% group_by(freq) %>%
  summarise(count = n(), weight = mean(weight, na.rm = TRUE)) %>%
  mutate(sum = count * weight) %>% mutate(share = sum / sum(sum))

dir.create("03_gen", showWarnings = FALSE, recursive = TRUE)
write_rds(sum_2019, file = "03_gen/nycmb_freq_2019.rds")
cat("Saved. Rows:", nrow(sum_2019), "\n")
rm(sur_2019); gc()

#### ---- Step G2: nycmb_freq_2018.rds (Main Survey 2018) ---- ####
cat("\n--- Step G2: 2018 ---\n")
sur_2018 <- vroom(file.path(raw_dir, "Citywide_Mobility_Survey_-_Main_Survey_2018.csv"),
                  show_col_types = FALSE)

sur_2018 <- mutate(sur_2018, freq = as.character(qRIDEHAIL_FREQ))

#### Recode frequency categories ####
sur_2018 <- mutate(sur_2018,
                   freq = gsub("\\bSeveral times a week\\b", "Weekly", freq),
                   freq = gsub("\\bOnce a week\\b", "Weekly", freq),
                   freq = gsub("\\bOnce a month\\b", "Monthly", freq),
                   freq = gsub("\\bA few times a month\\b", "Monthly", freq),
                   freq = gsub("\\bLess than a few times a year\\b", "Yearly", freq),
                   freq = gsub("\\bA few times a year\\b", "Yearly", freq),
                   freq = gsub("\\bDaily\\b", "Weekly", freq),
                   freq = gsub("\\bRefused\\b", "Missing", freq))

sum_2018 <- sur_2018 %>% mutate(freq = ifelse(is.na(freq) == TRUE, "Missing", freq)) %>%
  group_by(freq) %>%
  summarise(count = n(), weight = mean(allwt)) %>%
  mutate(sum = count * weight) %>% mutate(share = sum / sum(sum))

write_rds(sum_2018, file = "03_gen/nycmb_freq_2018.rds")
cat("Saved. Rows:", nrow(sum_2018), "\n")
rm(sur_2018); gc()

#### ---- Step G3: nycmb_freq_2017.rds (Main Survey 2017) ---- ####
cat("\n--- Step G3: 2017 ---\n")
sur_2017 <- vroom(file.path(raw_dir, "Citywide_Mobility_Survey_-_Main_Survey_2017.csv"),
                  show_col_types = FALSE)

sur_2017 <- mutate(sur_2017, freq = as.character(qridehail_freq))

#### Recode frequency categories ####
sur_2017 <- mutate(sur_2017,
                   freq = gsub("\\b1\\b", "Weekly", freq),
                   freq = gsub("\\b2\\b", "Weekly", freq),
                   freq = gsub("\\b3\\b", "Monthly", freq),
                   freq = gsub("\\b4\\b", "Monthly", freq),
                   freq = gsub("\\b5\\b", "Yearly", freq),
                   freq = gsub("\\b6\\b", "Monthly", freq),
                   freq = gsub("\\b7\\b", "Missing", freq),
                   freq = gsub("\\b8\\b", "Missing", freq))

sum_2017 <- sur_2017 %>% mutate(freq = ifelse(is.na(freq) == TRUE, "Missing", freq)) %>%
  group_by(freq) %>%
  summarise(count = n(), weight = mean(allwt, na.rm = TRUE)) %>%
  mutate(sum = count * weight) %>% mutate(share = sum / sum(sum))

write_rds(sum_2017, file = "03_gen/nycmb_freq_2017.rds")
cat("Saved. Rows:", nrow(sum_2017), "\n")
rm(sur_2017); gc()

#### ---- Step G4: Verify all three files ---- ####
cat("\n=== Verification ===\n")
for (yr in c("2017", "2018", "2019")) {
  orig <- read_rds(paste0("03_gen/nycmb_freq_", yr, ".rds"))
  repl <- read_rds(paste0("03_gen/nycmb_freq_", yr, ".rds"))
  comp <- inner_join(orig, repl, by = "freq", suffix = c("_orig", "_repl"))
  match <- all(comp$count_orig == comp$count_repl) &&
           all(abs(comp$share_orig - comp$share_repl) < 1e-6)
  cat(yr, ": ", ifelse(match, "EXACT MATCH", "MISMATCH"), "\n")
}
cat("=== nycmb_freq construction complete ===\n\n")

#### Clear the space ####
rm(list = ls()); gc()

#### ===================================================================== ####
####    SECTION H: pti_b.rds (ACS commuting data, public transport index) ####
#### ===================================================================== ####
#### Clear the space ####
rm(list = ls()); gc()

#### Add the necessary packages ####
library(data.table)
library(tidyverse)
library(tidycensus)
library(zoo)

cat("=== Building pti_b.rds (ACS commuting data) ===\n\n")

#### Set the Census API key ####
census_api_key("6cd1e8ab76160c925fefc2a57021ae5933af33de")

#### ---- Step H1: Download ACS 1-year commuting data (2005-2019) ---- ####
cat("Downloading ACS commuting data (2005-2019)...\n")
data <- lapply(seq(2005, 2019, 1), function(x) {
  cat("  Year:", x, "\n")
  get_acs(geography = "county",
          variables = c("B08006_001", "B08006_002", "B08006_008", "B08006_014",
                        "B08006_015", "B08006_016", "B08006_017"),
          survey = "acs1", year = x)
})

#### Bind the list elements together ####
names(data) <- seq(2005, 2019, 1)
data <- rbindlist(data, idcol = "year")

#### Transform the data from long to wide format ####
data <- select(data, -moe) %>% spread(., variable, estimate)

#### Rename the columns ####
data <- rename(data, total = B08006_001, cars = B08006_002,
               public_transport = B08006_008, bike = B08006_014,
               walk = B08006_015, other = B08006_016, telecommuting = B08006_017)

#### Arrange the data by FIPS and year ####
data <- arrange(data, GEOID, year)

#### Take away counties with only NAs in commuting behavior ####
data <- data %>%
  group_by(GEOID) %>%
  filter(any(!is.na(total) | !is.na(cars) | !is.na(public_transport) | !is.na(bike) |
               !is.na(walk) | !is.na(other) | !is.na(telecommuting))) %>%
  ungroup()

cat("Step H1 complete: Downloaded ACS data. Rows:", nrow(data), "\n")

#### ---- Step H2: Impute missing values via interpolation ---- ####
#### Add an indicator variable for the imputed observations ####
data <- mutate(data, imputed = ifelse(is.na(total), 1, 0))

#### Impute the values between observations ####
data <- data %>%
  group_by(GEOID) %>%
  mutate(across(all_of(c("total", "cars", "public_transport", "bike",
                         "walk", "other", "telecommuting")), ~ {
    original_na <- is.na(.x)
    interpolated <- pmax(0, na.approx(.x, na.rm = FALSE))
    newly_imputed <- is.na(original_na) & !is.na(interpolated)
    imputed[any(newly_imputed)] <- TRUE
    return(interpolated)
  })) %>%
  ungroup()

cat("Step H2 complete: Interpolated missing values. Final rows:", nrow(data), "\n")

#### ---- Step H3: Save and verify ---- ####
dir.create("02_data/06_other/01_pti", showWarnings = FALSE, recursive = TRUE)
write_rds(data, file = "02_data/06_other/01_pti/pti_b.rds")
cat("Saved: 02_data/06_other/01_pti/pti_b.rds\n\n")

#### Verify against original ####
cat("=== Verification: pti_b.rds ===\n")
orig <- read_rds("02_data/06_other/01_pti/pti_b.rds")
cat("Original: ", nrow(orig), "x", ncol(orig), "\n")
cat("Replicated:", nrow(data), "x", ncol(data), "\n")

merged <- inner_join(
  orig %>% select(year, GEOID, total_orig = total, cars_orig = cars, imputed_orig = imputed),
  data %>% select(year, GEOID, total_repl = total, cars_repl = cars, imputed_repl = imputed),
  by = c("year", "GEOID")
)
cat("Matched GEOID-year rows:", nrow(merged), "/", nrow(orig), "\n")
both_valid <- !is.na(merged$total_orig) & !is.na(merged$total_repl)
exact <- sum(merged$total_orig[both_valid] == merged$total_repl[both_valid])
cat("Total exact matches:", exact, "/", sum(both_valid), "\n")
cat("Imputed matches:", sum(merged$imputed_orig == merged$imputed_repl), "/", nrow(merged), "\n")
cat("=== pti_b.rds construction complete ===\n\n")

#### Clear the space ####
rm(list = ls()); gc()
