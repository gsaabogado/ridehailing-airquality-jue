#### ##################################################################### ####
####              Constructions of estimates for the appendix              ####
####              Part of the replication package                          ####
#### ##################################################################### ####
#### Set the working directory to the replication folder ####
## Path set by 00_master.R; if running standalone, set root manually
if (!exists("root")) root <- getwd()
setwd(root)

#### _____________________________________________________________________ ####
#### Figure E2 (TWFE): Estimate the weights of each cohort with the GB-Decomposition ####
#### _____________________________________________________________________ ####

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Load the packages ####
library(conflicted)
library(data.table)
library(tidyverse)
library(fixest)
library(broom)
library(bacondecomp)

#### Clear the space ####
conflict_prefer("filter", "dplyr")

#### Load the regressions data ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")

#### Filter the data to the preferred specification ####
data = data %>% filter(score %in% seq(-5, 0, 1)| treated == 0 )

#### Make a balanced panel for the Goodman bacon decomposition ####
data = data %>% group_by(fips) %>% mutate(count = n())
data = filter(data, count == 6)
data %>% group_by(count) %>% summarise(n())

#### Estimates of the effect ####
est = feols(avg ~ treatment | year + fips,
            data =  data,
            cluster = "cbsa_id")

#### Only keep a balanced panel ####
bacon_decomp <- bacon(
  formula = avg ~ treatment,
  data    = data,
  id_var  = "fips",
  time_var= "year"
)

#### Aggregate the bacon decomposition by weight ####
bcw <- bacon_decomp %>%
  mutate(
    # let's call g1 = the cohort that is truly "treated earlier"
    # g2 = the cohort that is "treated later" or "never"
    # Often the package calls them 'treated' and 'untreated', but
    # we rename them for clarity:
    cohort_treated = treated,
    cohort_comparison = untreated
  ) %>%
  group_by(cohort_treated, type) %>%
  summarise(
    sum_of_weights = sum(weight),
    # Weighted average of the 2x2 estimates that involve 'cohort_treated'
    # (But be cautious interpreting this as a direct aggregator.)
    # For a "share" of how much the final coefficient is driven by each cohort:
    total_contribution = sum(estimate * weight)
  ) %>%
  ungroup()

#### Save the data set with the GB-weights ####
write_rds(bcw, file = "03_gen/09_revision/twfe_cohort_weights.rds")

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### _____________________________________________________________________ ####
#### Figure E2 (ETWFE): Estimate the weights of each cohort for the E-TWFE regression ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Load the packages ####
library(conflicted)
library(data.table)
library(tidyverse)
library(etwfe)
library(broom)

#### Load the regressions data ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")

#### Filter the data to the preferred specification ####
data = data %>% filter(score %in% seq(-5, 0, 1)| treated == 0 )

#### Make a balanced panel for the Goodman bacon decomposition ####
data = data %>% group_by(fips) %>% mutate(count = n())
data = filter(data, count == 6)
data %>% group_by(count) %>% summarise(n())

#### Run the regression across all dependent variables and models ####
est =  etwfe(fml  = avg ~ 1, tvar = year,  gvar = enter_year,
             data = data, vcov = ~cbsa_id)

#### Aggregate the point estimates to ATT ####
agg = emfx(est, type = "group")

#### Extract the coefficients ####
coeff_df <- as.data.frame(est$coeftable)
coeff_df$term <- rownames(coeff_df)
head(coeff_df)

#### Extract the cohort and period ####
coeff_df <- coeff_df %>%
  mutate(
    gvar  = as.numeric(str_match(term, "enter_year::(\\d+):year")[,2]),
    tvar  = as.numeric(str_match(term, "year::(\\d+)")[,2])
  )

#### Calculate the event_time ####
coeff_df <- coeff_df %>%
  mutate(event_time = tvar - gvar)

#### Calculate the group sizes ####
group_sizes <- data %>%
  filter(treated == 1) %>%
  group_by(enter_year) %>%
  summarise(n_g = n())

#### Add the group sizes to the cohort-evel coefficients ####
coeff_df <- coeff_df %>%
  left_join(group_sizes, by = c("gvar" = "enter_year"))

#### Estimate the weight of each cohort ####
etwfe_w <- coeff_df %>%
  filter(event_time >= 0) %>%
  group_by(gvar) %>%
  # step A: average across all post-treatment event times for group gvar
  summarise(
    post_est = mean(Estimate, na.rm = TRUE),
    n_g = mean(n_g, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(weight_g = n_g / sum(n_g, na.rm = TRUE),
         weighted_est = post_est * weight_g) %>%
  mutate(agg_att = sum(weighted_est, na.rm = TRUE))

#### Save the data set with the GB-weights ####
write_rds(etwfe_w, file = "03_gen/09_revision/etwfe_cohort_weights.rds")

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### _____________________________________________________________________ ####
#### Figure E2 (SADD): Estimate the weights for the SA-DD regression ####
#### _____________________________________________________________________ ####

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Load the packages ####
library(fixest)
library(data.table)
library(tidyverse)
library(stringr)
library(purrr)

#### Load the regressions data ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")

#### Filter the data to the preferred specification ####
data = data %>%
  filter(score %in% seq(-5, 0, 1) | treated == 0 )

#### Sun & Abraham regression ####
est_sa <- feols(
  avg ~ sunab(enter_year, year) | fips + year,
  data = data,
  cluster = "cbsa_id"
)

#### Print the aggregate ATT ####
summary(est_sa, agg = "att")

#### Obtain all group×event-time estimates: ####
res_none <- summary(est_sa, agg = FALSE)
res_none$coeftable

#### Parse out the cohort and event_time from coefficient names ####
df_estimates <- as.data.frame(res_none$coeftable) %>%
  mutate(
    term = rownames(res_none$coeftable),
    cohort = as.numeric(str_match(term, "cohort::(\\d+)")[,2]),
    event_time = as.numeric(str_replace_all(
      str_match(term, "year::([+-]?\\d+)")[,2], "\\+", ""
    ))
  )

#### Subset to post-treatment event times (>= 0) ####
df_estimates <- df_estimates %>%
  filter(event_time >= 0)

#### For each (cohort g, event_time e), we want to count
#    how many *treated* observations are actually in that cell
#    in the data. We'll call that n_treated_{g,e}.
####
#    Then, the approximate S&A aggregator is:
#         SUM_{g,e >= 0} [ (n_treated_{g,e} / total_treated_post) * Estimate_{g,e} ].
#    In other words, we weight each (g,e) coefficient by the fraction of
#    *treated* observations (among all post-treatment cells) that fall in (g,e).

#### Label each row with its own event-time ####
df_for_sa <- data %>%
  mutate(
    own_e = year - enter_year  # This is each unit's event time
  )

#### Compute n_treated_{g,e} for e >= 0 ####
#    i.e. how many observations in the data belong to cohort g
#    AND are exactly e years after their own enter_year?
unique_cohort_event <- df_estimates %>%
  distinct(cohort, event_time) %>%
  rename(g = cohort, e = event_time)

####  Create the list of results ####
results_list <- map_df(seq_len(nrow(unique_cohort_event)), function(i) {
  g_i <- unique_cohort_event$g[i]
  e_i <- unique_cohort_event$e[i]

  sub_treated <- df_for_sa %>%
    filter(enter_year == g_i, own_e == e_i)

  tibble(
    g = g_i,
    e = e_i,
    n_treated = nrow(sub_treated)
  )
})

#### Join n_treated_{g,e} onto estimates ####
df_weights <- df_estimates %>%
  left_join(results_list, by = c("cohort" = "g", "event_time" = "e"))

#### The total # of treated observations across all (g,e >= 0) ####
total_treated_post <- sum(df_weights$n_treated, na.rm = TRUE)

#### Compute approximate S&A weight = fraction of treated in that (g,e) ####
df_weights <- df_weights %>%
  mutate(
    # weight for (g,e) is n_treated_{g,e} / total_treated_post
    w_ge = n_treated / total_treated_post,
    contribution = Estimate * w_ge
  )

#### Sum the contributions to get an approximate aggregation ####
att_sa_exact <- sum(df_weights$contribution, na.rm = TRUE)
att_sa_exact

#### 7) Compare with fixest's aggregator:
summary(est_sa, agg = "att")

#### Save the data set with the GB-weights ####
write_rds(df_weights, file = "03_gen/09_revision/sadd_cohort_weights.rds")

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### _____________________________________________________________________ ####
#### Table G1: Effects of Uber on the AQI (Heterogeneous Clustering) ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Load the packages ####
library(data.table)
library(tidyverse)
library(synthdid)
library(fixest)
library(etwfe)
library(broom)

#### Load the regressions data ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")

#### Transform from wide to long format ####
data = gather(data, var, value, c(avg))

#### Run the regression across all dependent variables and models ####
est =  lapply(split(data, f = data$var), function(x)

  list(feols(value ~ treatment | year + fips,
             data = x %>% filter(score %in% seq(-5, 0, 1)| treated == 0),
             cluster = "state_name"),

       feols(value ~ treatment  | year + fips,
             data = x %>% filter(score %in% seq(-5, 0, 1)| treated == 0),
             cluster = "cbsa_id"),

       feols(value ~ treatment  | year + fips,
             data = x %>% filter(score %in% seq(-5, 0, 1)| treated == 0),
             cluster = "fips"),

       feols(value ~ sunab(enter_year, year) | fips + year,
             data = data |> filter(score %in% seq(-5,0,1) | treated == 0 ),
             cluster = "state_name") %>% summary(agg = "att"),

       feols(value ~ sunab(enter_year, year) | fips + year,
             data = data |> filter(score %in% seq(-5,0,1) | treated == 0 ),
             cluster = "cbsa_id") %>% summary(agg = "att"),

       feols(value ~ sunab(enter_year, year) | fips + year,
             data = data |> filter(score %in% seq(-5,0,1) | treated == 0 ),
             cluster = "fips") %>% summary(agg = "att"),

       etwfe(fml  = value ~ 1,  tvar = year,  gvar = enter_year,
             data = x |> filter(score %in% seq(-5,0,1)| treated == 0),
             vcov = ~state_name),

       etwfe(fml  = value ~ 1,  tvar = year,  gvar = enter_year,
             data = x |> filter(score %in% seq(-5,0,1)| treated == 0),
             vcov = ~cbsa_id),

       etwfe(fml  = value ~ 1,  tvar = year,  gvar = enter_year,
             data = x |> filter(score %in% seq(-5,0,1)| treated == 0),
             vcov = ~fips)))

#### Extract the fitted statistics ####
fitted = lapply(est, function(x) lapply(x, function(x)
  x = data.frame(N.Obs = x$nobs,
                 R2 = r2(x)[2]))) %>%

  lapply(., rbindlist, idcol = "spec") %>%

  rbindlist(., idcol = "var")

#### Aggregate the E-TWFE estimator ####
est <- lapply(est, function(x) lapply(x, function(x) {
  if("etwfe" %in% class(x)) {x <- emfx(x)}; x}))

#### Extract the relevant coefficients ####
sum = lapply(est, function(x) lapply(x, function(x)
  x = data.frame(tidy(x) |> select(term, estimate, std.error, p.value)))) %>%

  lapply(., rbindlist, idcol = "spec") %>%

  rbindlist(.)

#### Add the pre-treatment average ####
data %>% filter(treatment == 0 & treated == 1) %>%
  group_by(var) %>% filter(score %in% seq(-5, 0, 1)) %>%
  summarise(`pt_value` = mean(value))

#### Join the fitted statistics ####
sum = left_join(sum, fitted)

#### Check the final data set ####
sum

#### Save the summary values ####
write_rds(sum, file = "03_gen/05_results/twfe_cluster.rds")

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### _____________________________________________________________________ ####
#### Figure G6: TWFE Dynamic ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Load the packages ####
library(conflicted)
library(data.table)
library(tidyverse)
library(fixest)
library(broom)

#### Clear the space ####
conflict_prefer("filter", "dplyr")

#### Load the regressions data ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")

#### Transform from wide to long format ####
data = gather(data, var, value, c(avg:max))

#### Only keep the average ####
data = filter(data, var == "avg")

#### Run the regression across all dependent variables and models ####
est = feols(value ~ i(score, treated, ref = -1) + st_rain + st_tmp | year + fips,
            data = data %>% filter(score %in% seq(-5, 0, 1)),
            cluster = "cbsa_id")

#### Extract the relevant coefficients ####
sum = data.frame(tidy(est))

#### Only keep the event-time estimates ####
sum = mutate(sum, score = gsub(".*::", "", term)) %>%
  mutate(score = gsub(":.*", "",score)) %>%
  filter(grepl("treated", term)) %>% select(-term);sum

#### Organize the data set #####
sum = select(sum, c(score, estimate:p.value))

#### Check the final data set ####
sum %>% head(.)

#### Save the summary values ####
write_rds(sum, file = "03_gen/05_results/twfe_dynamic.rds")

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### _____________________________________________________________________ ####
#### Figure G6: E-TWFE Dynamic ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Load the packages ####
library(conflicted)
library(data.table)
library(tidyverse)
library(fixest)
library(broom)

#### Clear the space ####
conflict_prefer("filter", "dplyr")

#### Load the regressions data ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")

#### Transform from wide to long format ####
data = gather(data, var, value, c(avg:max))

#### Only keep the average ####
data = filter(data, var == "avg")

#### Run the regression across all dependent variables and models ####
#### Run the regression across all dependent variables and models ####
est = etwfe(fml = value ~ st_tmp + st_rain, tvar = year,  gvar = enter_year,
            data = data |> filter(score %in% seq(-5,0,1)| treated == 0),
            vcov = ~cbsa_id)

#### Aggregate the point estimates to ATT ####
agg = emfx(est, type = "event", post_only = F)

#### Extract the relevant coefficients ####
sum = data.frame(tidy(agg)) %>%
  select(c(score = event, estimate:p.value)) %>%
  filter(score > -6)

#### Check the final data set ####
sum %>% head(.)

#### Save the summary values ####
write_rds(sum, file = "03_gen/05_results/etwfe_dynamic.rds")

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### _____________________________________________________________________ ####
#### Figure G6: SA-DD Dynamic ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Load the packages ####
library(conflicted)
library(data.table)
library(tidyverse)
library(fixest)
library(broom)

#### Clear the space ####
conflict_prefer("filter", "dplyr")

#### Load the regressions data ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")

#### Transform from wide to long format ####
data = gather(data, var, value, c(avg:max))

#### Only keep the average ####
data = filter(data, var == "avg")
data = mutate(data, enter_year = ifelse(enter_year > 2016, 2020, enter_year))

sort(unique(data$enter_year))
#### Run the regression across all dependent variables and models ####
est = feols(value ~ sunab(enter_year, year,
                          ref.c = c("2020", "2016", "2015",
                                    "2014", "2013", "2012")) +
              st_rain + st_tmp | enter_year + year,
            data = data|> filter(score %in% seq(-5,0,1) | treated == 0),
            cluster = "cbsa_id")

#### Extract the relevant coefficients ####
sum = data.frame(tidy(est))

#### Only keep the event-time estimates ####
sum = mutate(sum, score = gsub(".*::", "", term)) %>%
  filter(grepl("year", term)) %>% select(-term);sum

#### Organize the data set #####
sum = select(sum, c(score, estimate:p.value))

#### Check the final data set ####
sum %>% head(.)

#### Save the summary values ####
write_rds(sum, file = "03_gen/05_results/sadd_dynamic.rds")

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### _____________________________________________________________________ ####
#### Table H5-Cols 4-6: Controlling for gasoline prices and the gas to coal switch ####
#### _____________________________________________________________________ ####

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Load the packages ####
library(fixest)
library(data.table)
library(etwfe)
library(tidyverse)
library(stringr)
library(broom)
library(purrr)
library(readxl)

#### Load the regressions data ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")
gas = read.csv("02_data/06_other/6_Gasoline_price_data_states/statesgasoline.csv")
pp_fips = read_xlsx("02_data/06_other/8_Power_generation_states and zip/coal_and_natgas_share_by_fips.xlsx")
pp_state = read_xlsx("02_data/06_other/8_Power_generation_states and zip/Coal_NatGas_Share_by_State_Year.xlsx")

#### Change the names ####
pp_fips = select(pp_fips, year = Year, fips = FIPS, netgen_fips = Total_NETGEN_MWh,
                 c_fips = C_NETGEN_MWh, ng_fips = NG_NETGEN_MWh,
                 c_share_fips = c_share, ng_share_fips = ng_share)

pp_state = select(pp_state, year = Year, stusps = State, netgen_st = Total_NETGEN_MWh,
                  c_st = Coal_NETGEN_MWh, ng_st = NatGas_NETGEN_MWh,
                  c_share_st = coal_share, ng_share_st = natgas_share)


#### Aggregate tje state data ####
pp_state = pp_state %>% group_by(year, stusps) %>%
  summarise_all(mean, na.rm = T)

#### Exclude errors in the state-level data ####
pp_state = filter(pp_state, !is.na(stusps) & stusps != "." & year != ".")

#### Filter the data to the preferred specification ####
data = data %>% filter(score %in% seq(-5, 0, 1) | treated == 0)
data = data %>% mutate(statesfips = substr(fips, 1,2) %>% as.numeric(.))

#### Add the new variables ####
data = left_join(data, gas)
data = left_join(data, pp_fips %>% mutate(year = as.numeric(year)))
data = left_join(data, pp_state %>% mutate(year = as.numeric(year)))

#### Scale generation from MWh to TWh for numerical stability (E-TWFE VCOV) ####
data = mutate(data, netgen_st = netgen_st / 1e6)

#### Transform NAs in state generation to zeros ####
data = mutate_at(data, vars(netgen_st, c_share_st, ng_share_st), function(x)
  x = ifelse(is.na(x), 0, x))

#### Run the regressions
est = list(TWFE = feols(avg ~ treatment +  netgen_st + dollarpergallon  +
                          c_share_st+ ng_share_st| year + fips,
                        data =  data,
                        cluster = "cbsa_id"),

           `E-TWFE` = etwfe(fml  = avg ~  netgen_st + dollarpergallon  +
                              c_share_st + ng_share_st, tvar = year,  gvar = enter_year,
                            data = data, vcov = ~cbsa_id),

           `SA-DD` = feols(
             avg ~ sunab(enter_year, year) + netgen_st + dollarpergallon  +
               c_share_st+ ng_share_st | fips + year,
             data = data,cluster = "cbsa_id") %>% summary(agg = "att"))

#### Extract the coefficient table ####
sum = lapply(est, function(x)
  data.frame(R2 = r2(x)[2], BIC = BIC(x), N.Obs = nobs(x))) %>%
  rbindlist(., idcol = "estimator")

#### Aggregate the E-TWFE estimator ####
est <- lapply(est, function(x)  {
  if("etwfe" %in% class(x)) {x <- emfx(x)}; x})

#### Extract the relevant coefficients ####
sum = lapply(est, function(x)

  data.frame(tidy(x)) |>
    select(term, estimate, std.error, p.value)) %>%

  rbindlist(., idcol = "estimator") %>%

  filter(term == "treatment" | term == "ATT" | term == ".Dtreat")%>%

  left_join(., sum)

#### Add the pre-treatment average ####
sum$pt_value= data %>% ungroup() %>%  filter(treatment == 0 & treated == 1) %>%
  filter(score %in% seq(-5, 0, 1)) %>%
  summarise(`pt_value` = mean(avg))  %>% unlist()

#### Include the number of observations ####
sum$N.Cohorts = length(unique(data$enter_year))
sum$N.Counties = length(unique(data$fips))
sum$N.Periods = length(unique(data$year))

#### Check the final data set ####
sum

#### Save the summary values ####
write_rds(sum, file = "03_gen/05_results/gasoline_and_st_pp_robust.rds")

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()
#### _____________________________________________________________________ ####
#### Table H5-Cols 1-3: Controlling for gasoline prices and the gas to coal switch ####
#### _____________________________________________________________________ ####

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Load the packages ####
library(fixest)
library(data.table)
library(etwfe)
library(tidyverse)
library(stringr)
library(broom)
library(purrr)
library(readxl)

#### Load the regressions data ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")
gas = read.csv("02_data/06_other/6_Gasoline_price_data_states/statesgasoline.csv")
pp_fips = read_xlsx("02_data/06_other/8_Power_generation_states and zip/coal_and_natgas_share_by_fips.xlsx")
pp_state = read_xlsx("02_data/06_other/8_Power_generation_states and zip/Coal_NatGas_Share_by_State_Year.xlsx")

#### Change the names ####
pp_fips = select(pp_fips, year = Year, fips = FIPS, netgen_fips = Total_NETGEN_MWh,
                 c_fips = C_NETGEN_MWh, ng_fips = NG_NETGEN_MWh,
                 c_share_fips = c_share, ng_share_fips = ng_share)

pp_state = select(pp_state, year = Year, stusps = State, netgen_st = Total_NETGEN_MWh,
                  c_st = Coal_NETGEN_MWh, ng_st = NatGas_NETGEN_MWh,
                  c_share_st = coal_share, ng_share_st = natgas_share)


#### Aggregate the state data ####
pp_state = pp_state %>% group_by(year, stusps) %>%
  summarise_all(mean, na.rm = T)

#### Exclude errors in the state file ####
pp_state = filter(pp_state, !is.na(stusps) & stusps != "." & year != ".")

#### Filter the data to the preferred specification ####
data = data %>% filter(score %in% seq(-5, 0, 1) | treated == 0)
data = data %>% mutate(statesfips = substr(fips, 1,2) %>% as.numeric(.))

#### Add the data sets together
data = left_join(data, gas)
data = left_join(data, pp_fips %>% mutate(year = as.numeric(year)))
data = left_join(data, pp_state %>% mutate(year = as.numeric(year)))

#### Scale generation from MWh to TWh for numerical stability (E-TWFE VCOV) ####
data = mutate(data, netgen_fips = netgen_fips / 1e6, c_fips = c_fips / 1e6, ng_fips = ng_fips / 1e6)

#### Substitute NAs with counties with no power plants to zero ####
data = mutate_at(data, vars(netgen_fips, c_fips, ng_fips, c_share_fips, ng_share_fips), function(x)
  x = ifelse(is.na(x), 0, x))

#### Run the regressions
est = list(TWFE = feols(avg ~ treatment +  netgen_fips + dollarpergallon  +
                          c_fips+ ng_fips| year + fips,
                        data =  data,
                        cluster = "cbsa_id"),

           `E-TWFE` = etwfe(fml  = avg ~  netgen_fips + dollarpergallon  +
                              c_fips + ng_fips, tvar = year,  gvar = enter_year,
                            data = data, vcov = ~cbsa_id),

           `SA-DD` = feols(
             avg ~ sunab(enter_year, year) + netgen_fips + dollarpergallon  +
               c_fips+ ng_fips | fips + year,
             data = data,cluster = "cbsa_id") %>% summary(agg = "att"))

#### Extract the coefficient table ####
sum = lapply(est, function(x)
  data.frame(R2 = r2(x)[2], BIC = BIC(x), N.Obs = nobs(x))) %>%
  rbindlist(., idcol = "estimator")

#### Aggregate the E-TWFE estimator ####
est <- lapply(est, function(x)  {
  if("etwfe" %in% class(x)) {x <- emfx(x)}; x})

#### Extract the relevant coefficients ####
sum = lapply(est, function(x)

  data.frame(tidy(x)) |>
    select(term, estimate, std.error, p.value)) %>%

  rbindlist(., idcol = "estimator") %>%

  filter(term == "treatment" | term == "ATT" | term == ".Dtreat")%>%

  left_join(., sum)

#### Add the pre-treatment average ####
sum$pt_value= data %>% ungroup() %>%  filter(treatment == 0 & treated == 1) %>%
  filter(score %in% seq(-5, 0, 1)) %>%
  summarise(`pt_value` = mean(avg))  %>% unlist()

#### Include the number of observations ####
sum$N.Cohorts = length(unique(data$enter_year))
sum$N.Counties = length(unique(data$fips))
sum$N.Periods = length(unique(data$year))

#### Check the final data set ####
sum %>% head(.)

#### Save the summary values ####
write_rds(sum, file = "03_gen/05_results/gasoline_and_fips_pp_robust.rds")

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### _____________________________________________________________________ ####
#### Table H6: Balancing regressions for selected variables ####
#### _____________________________________________________________________ ####

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Load the packages ####
library(fixest)
library(data.table)
library(etwfe)
library(tidyverse)
library(stringr)
library(broom)
library(readxl)

#### _____________________________________________________________________ ####
#### Columns 1-3: NAAQS, Forest Fire, Power Plant Shut-Down ####
#### _____________________________________________________________________ ####
#### Note: These are binary outcomes estimated with E-TWFE logit + fe="vs". ####
#### The marginaleffects package (v0.31+) cannot compute SEs for non-      ####
#### Gaussian models with absorbed FEs (see etwfe GitHub issue #64).       ####
#### We use delete-one-cluster jackknife SEs instead.                      ####

#### Load the regressions data ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")

#### Only keep short-term changes ####
data = filter(data, score %in% seq(-5, 0, 1) | treated == 0)

#### Select the relevant variables ####
data = select(data, year, enter_year, fips, treated,
              treatment, cbsa_id, NAAQS, forest_fire, ret_pp)

#### Transform from long to wide format ####
data_long = gather(data, var, value, c(NAAQS:ret_pp))

#### E-TWFE Estimates (logit, fe = "vs") ####
etwfe_est = lapply(split(data_long, f = data_long$var), function(x)
  etwfe(fml  = value ~ 1,
        tvar = year,
        gvar = enter_year,
        data = x, family = "logit",
        vcov = ~cbsa_id, fe = "vs"))

#### Extract the fitted statistics ####
fitted = lapply(etwfe_est, function(x)
  data.frame(N.Obs = nobs(x), R2 = r2(x)[1])) %>%
  rbindlist(., idcol = "spec")

#### Aggregate the E-TWFE results (point estimates only) ####
agg = lapply(etwfe_est, function(x) suppressWarnings(emfx(x, vcov = FALSE)))

#### Extract the point estimates ####
sum_cols13 = lapply(agg, function(x) {
  data.frame(spec = NA, term = "ATT",
             estimate = x$estimate[1],
             std.error = NA_real_,
             p.value = NA_real_)
}) %>% rbindlist(., idcol = "spec")
sum_cols13$spec = names(agg)
sum_cols13 = left_join(sum_cols13, fitted)

#### Delete-one-cluster jackknife for standard errors ####
cbsa_ids = sort(unique(data$cbsa_id))
G = length(cbsa_ids)

for (v in c("NAAQS", "forest_fire", "ret_pp")) {
  theta_jack = rep(NA_real_, G)
  v_data = data_long[data_long$var == v, ]
  for (g in 1:G) {
    tryCatch({
      d_g = v_data[v_data$cbsa_id != cbsa_ids[g], ]
      m_g = suppressWarnings(
        etwfe(fml = value ~ 1, tvar = year, gvar = enter_year,
              data = d_g, family = "logit",
              vcov = ~cbsa_id, fe = "vs"))
      a_g = suppressWarnings(emfx(m_g, vcov = FALSE))
      theta_jack[g] = a_g$estimate[1]
    }, error = function(e) {})
  }
  theta_bar = mean(theta_jack, na.rm = TRUE)
  se_jack = sqrt(((G - 1) / G) * sum((theta_jack[!is.na(theta_jack)] - theta_bar)^2))
  sum_cols13$std.error[sum_cols13$spec == v] = se_jack
}

#### Check the results ####
sum_cols13

#### Clear before next section ####
rm(list = setdiff(ls(), "sum_cols13")); gc()

#### _____________________________________________________________________ ####
#### Columns 4-6: County Coal, Gas, and Other share ####
#### _____________________________________________________________________ ####

#### Load the regressions data ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")
pp_fips = read_xlsx("02_data/06_other/8_Power_generation_states and zip/coal_and_natgas_share_by_fips.xlsx")

#### Rename and recompute shares from raw MWh ####
#### Note: ng_share column in source Excel is buggy (identical to c_share) ####
pp_fips = select(pp_fips, year = Year, fips = FIPS, netgen_fips = Total_NETGEN_MWh,
                 c_fips = C_NETGEN_MWh, ng_fips = NG_NETGEN_MWh,
                 c_share_fips = c_share, ng_share_fips = ng_share) %>%
  mutate(c_share_fips = c_fips/netgen_fips,
         ng_share_fips = ng_fips/netgen_fips)

#### Filter the data to the preferred specification ####
data = data %>% filter(score %in% seq(-5, 0, 1) | treated == 0)

#### Merge plant-level data ####
data = left_join(data, pp_fips %>% mutate(year = as.numeric(year)))

#### Substitute NAs for counties with no power plants to zero ####
data = mutate_at(data, vars(netgen_fips, c_fips, ng_fips,
                            c_share_fips, ng_share_fips), function(x)
  x = ifelse(is.na(x), 0, x))

#### Compute other share ####
data = mutate(data, other_share = (1-(ng_share_fips + c_share_fips)))

#### E-TWFE Estimates ####
etwfe_est = list(
  c_share_fips  = etwfe(fml = c_share_fips ~ 1, tvar = year, gvar = enter_year,
                        data = data, vcov = ~cbsa_id),
  ng_share_fips = etwfe(fml = ng_share_fips ~ 1, tvar = year, gvar = enter_year,
                        data = data, vcov = ~cbsa_id),
  other_share   = etwfe(fml = other_share ~ 1, tvar = year, gvar = enter_year,
                        data = data, vcov = ~cbsa_id))

#### Extract the fitted statistics ####
fitted = lapply(etwfe_est, function(x)
  data.frame(N.Obs = nobs(x), R2 = r2(x)[2])) %>%
  rbindlist(., idcol = "spec")

#### Aggregate the E-TWFE results ####
agg = lapply(etwfe_est, function(x) emfx(x))

#### Extract the relevant coefficients ####
sum_cols46 = lapply(agg, function(x)
  data.frame(tidy(x)) |>
    select(term, estimate, std.error, p.value)) %>%
  rbindlist(., idcol = "spec") %>%
  filter(term == "ATT" | term == ".Dtreat") %>%
  left_join(., fitted)

#### Check the results ####
sum_cols46

#### Clear before next section ####
rm(list = setdiff(ls(), c("sum_cols13", "sum_cols46"))); gc()

#### _____________________________________________________________________ ####
#### Column 7: Toxic Release Inventory (Log) ####
#### _____________________________________________________________________ ####

#### Load the regressions data ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")
tri = read_csv("02_data/06_other/9_TRI/avgtotalrelease.csv")

#### Transform the FIPS ####
tri = mutate(tri, fips = str_pad(fips, width = 5, side = "left", pad = 0)) %>%
  select(year, fips, tri = avgtotalreleases)

#### Filter the data to the preferred specification ####
data = data %>% filter(score %in% seq(-5, 0, 1) | treated == 0)

#### Merge TRI data ####
data = left_join(data, tri)
data = mutate(data, tri = ifelse(is.na(tri), 0, tri))

#### E-TWFE Estimate ####
etwfe_est = list(
  log_tri = etwfe(fml = log(tri) ~ 1, tvar = year, gvar = enter_year,
                  data = data, vcov = ~cbsa_id))

#### Extract the fitted statistics ####
fitted = lapply(etwfe_est, function(x)
  data.frame(N.Obs = nobs(x), R2 = r2(x)[2])) %>%
  rbindlist(., idcol = "spec")

#### Aggregate the E-TWFE results ####
agg = lapply(etwfe_est, function(x) emfx(x))

#### Extract the relevant coefficients ####
sum_col7 = lapply(agg, function(x)
  data.frame(tidy(x)) |>
    select(term, estimate, std.error, p.value)) %>%
  rbindlist(., idcol = "spec") %>%
  filter(term == "ATT" | term == ".Dtreat") %>%
  left_join(., fitted)

#### Check the results ####
sum_col7

#### _____________________________________________________________________ ####
#### Combine all columns and save ####
#### _____________________________________________________________________ ####
sum = rbindlist(list(sum_cols13, sum_cols46, sum_col7))
sum

#### Save the summary values ####
write_rds(sum, file = "03_gen/05_results/balance_selected_vars.rds")

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### _____________________________________________________________________ ####
#### Table H7: Macro-covariates balancing regressions ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Load the packages ####
library(conflicted)
library(data.table)
library(tidyverse)
library(fixest)
library(broom)

#### Clear the space ####
conflict_prefer("filter", "dplyr")

#### Load the regressions data ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")

#### Only keep short-term changes ####
data = filter(data, score %in% seq(-5, 0, 1) | treated == 0)

#### Select the relevant variables ####
data = select(data, year, enter_year, fips, treated,
              treatment, cbsa_id, incomepercapita,
              unemp_rate, const_exp, white, pop)

#### Transform some of the variables ####
data = mutate(data, white = white*100, const_exp = const_exp/1000)

#### Transform from long to wide format ####
data = gather(data, var, value, c(incomepercapita:white))

#### TWFE Estimates ####
twfe = lapply(split(data, f = data$var), function(x)
  feols(log(value) ~ treatment  | year + fips, data = x,
        cluster = "cbsa_id"))

#### ETWFE Estimates ####
etwfe = lapply(split(data, f = data$var), function(x)
  etwfe(fml  = log(value) ~ 1,
        tvar = year,
        gvar = enter_year,
        data = x,
        vcov = ~cbsa_id))

#### Bind both estimators together ####
est = list(TWFE = twfe, `E-TWFE` = etwfe)

#### Extract the fitted statistics ####
fitted = lapply(est, function(x) lapply(x, function(x)
  x = data.frame(N.Obs = nobs(x), R2 = r2(x)[2]))) %>%
  lapply(., rbindlist, idcol = "spec") %>%
  rbindlist(., idcol = "estimator")

#### Aggregate the ETWFE results ####
est$`E-TWFE` = lapply(est$`E-TWFE` , function(x) emfx(x))

#### Extract the relevant coefficients ####
sum = lapply(est, function(x) lapply(x, function(x)
  x = data.frame(tidy(x)) |>
    select(term, estimate, std.error, p.value))) %>%

  lapply(., rbindlist, idcol = "spec") %>%
  rbindlist(., idcol = "estimator") %>%

  filter(term == "treatment" | term == "ATT" | term == ".Dtreat")

#### Add additional statistics ####
sum = left_join(sum, fitted)

#### Include the number of observations ####
sum$N.Counties = length(unique(data$fips))
sum$N.Periods = length(unique(data$year))
sum$N.Cohorts = length(unique(data$enter_year))

#### Add the pre-treatment average ####
sum = data %>% filter(treatment == 0 & treated == 1) %>%
  group_by(spec = var) %>%
  summarise(`pt_value` = mean(value, na.rm = T)) %>%
  left_join(sum, .)

#### Transform the log estimates to percentage change ####
sum = mutate(sum,
             estimate = (exp(estimate) - 1)*100,
             std.error = (exp(std.error) - 1)*100) %>%
  filter(estimator == "E-TWFE")


#### Organize the data set #####
sum = select(sum, c(estimator, spec, estimate:pt_value))


#### Check the final data set ####
sum

#### Save the summary values ####
write_rds(sum, file = "03_gen/05_results/macro_reg.rds")

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()
#### _____________________________________________________________________ ####
#### Table H8: Testing for heterogeneous pre-trends for different controls ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Load the packages ####
library(conflicted)
library(data.table)
library(tidyverse)
library(fixest)
library(broom)

#### Clear the space ####
conflict_prefer("filter", "dplyr")

#### Load the regressions data ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")

#### Only keep short-term changes ####
data = filter(data, score %in% seq(-5, -1, 1) | treated == 0)

#### Select the relevant variables ####
data = select(data, year, score, enter_year, fips, treated,
              treatment, cbsa_id, incomepercapita,
              unemp_rate, const_exp, white, forest_fire, NAAQS, new_pp, pop, avg) %>%
  mutate(const_exp = const_exp/10000, incomepercapita = incomepercapita/1000, white = white*100, ) %>%
  mutate_at(vars(incomepercapita, unemp_rate, const_exp, white), log)

#### Transform some of the variables ####
data = mutate(data, pre_treatment_indicator = ifelse(score < 0, 1, 0))
data = gather(data, var, value, c(incomepercapita:new_pp))

#### TWFE Estimates ####
est = lapply(split(data, f = data$var), function(x)
  feols(value ~ treated:score | year + fips, data = x, cluster = "cbsa_id"))

#### Extract the fitted statistics ####
fitted = lapply(est, function(x)
  x = data.frame(N.Obs = nobs(x), R2 = r2(x)[2])) %>%
  rbindlist(., idcol = "var")

#### Extract the relevant coefficients ####
sum = lapply(est, function(x)
  x = data.frame(tidy(x)) |>
    select(term, estimate, std.error, p.value)) %>%

  rbindlist(., idcol = "var") %>%

  filter(grepl("score", term))

#### Add additional statistics ####
sum = left_join(sum, fitted)

#### Include the number of observations ####
sum$N.Counties = length(unique(data$fips))
sum$N.Periods = length(unique(data$year))
sum$N.Cohorts = length(unique(data$enter_year))


#### Organize the data set #####
sum = select(sum, c(var, estimate:N.Cohorts))

#### Check the final data set ####
sum %>% head(.)

#### Save the summary values ####
write_rds(sum, file = "03_gen/05_results/macro_trend_test.rds")

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### _____________________________________________________________________ ####
#### Table H9 -- Cols (1) and (2): Effects of Uber on PM2.5 ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Load the packages ####
library(conflicted)
library(data.table)
library(tidyverse)
library(synthdid)
library(fixest)
library(broom)
library(etwfe)

#### Load the data ####
data = read_rds("03_gen/04_reg/sat_reg_test.rds")

#### Determine the effect ####
est = list(`TWFE` = feols(pm25 ~ treatment | year + fips, notes = F,
                          data = data, cluster = "cbsa_id"),


           ETWFE = etwfe(fml  = pm25 ~ 1,
                         tvar = year,
                         gvar = enter_year,
                         data = data, vcov = ~cbsa_id))


#### Extract the fitted statistics ####
fitted = lapply(est, function(x)
  x = data.frame(N.Obs = nobs(x), R2 = r2(x)[2])) %>%
  rbindlist(., idcol = "spec")

#### Aggregate the mundalak estimator ###
est$ETWFE = emfx(est$ETWFE)

#### Extract the relevant coefficients ####
sum = lapply(est, function(x)
  x = data.frame(tidy(x)) |>
    select(term, estimate, std.error, p.value)) %>%

  rbindlist(., idcol = "spec") %>%

  filter(term == "treatment" | term == "ATT" | term == ".Dtreat")

#### Include the number of observations ####
sum$N.Counties = length(unique(data$fips))
sum$N.Periods = length(unique(data$year))
sum$N.Cohorts = length(unique(data$enter_year))

#### Add the pre-treatment average ####
sum$pt_value = (filter(data, treatment == 0 & treated == 1) %>% ungroup() |>
                  summarise(pt_raw = median(pm25, na.rm = T)))$pt_raw

#### Add the nobs ###
sum = left_join(sum, fitted)

#### Review the data set ####
sum

#### Save the summary values ####
write_rds(sum, file = "03_gen/05_results/did_pm25.rds")

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### _____________________________________________________________________ ####
#### Table H9 -- Cols (2) and (4): Effects of Uber on PM2.5 (SDiD) ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Load the packages ####
library(conflicted)
library(data.table)
library(tidyverse)
library(synthdid)
library(fixest)
library(etwfe)
library(broom)

#### Load the data ####
data = read_rds("03_gen/04_reg/sat_reg_test.rds")

#### Gather the variables ####
data = gather(data, var, value, c(pm25))

#### Take away all NAs in total ####
data = filter(data, is.na(value) == F)

#### Determine the enter years ####
tg = sort(unique(filter(data, enter_year < 2017)$enter_year))

#### Split the data into a list of data frames for each treatment group ####
data = lapply(tg, function(x)
  filter(data, enter_year >= x) %>%
    mutate(tg = x, score = year - tg) %>%
    filter(score %in% seq(-5, 1, 1)) %>%
    mutate(treated = ifelse(enter_year == tg, 1, 0)) %>%
    mutate(treatment = ifelse(year >= enter_year & treated == 1, 1, 0)))

#### Transform the data frames into a balanced panel ####
data = lapply(data, function(x)
  x %>% group_by(fips, var) %>% mutate(count = n()) %>%
    filter(count == 7) %>% as.data.frame(.))

#### Add names to the list elements ####
names(data) = tg

#### Split the data sets by dependent variable ###
data = lapply(data, function(x)
  split(x, f = x$var))

#### Construct the panel object for the SDiD algorithm ####
panel = lapply(data, function(x) lapply(x, function(x)
  panel.matrices(x, unit = "fips", time = "score",
                 outcome = "value", treatment = "treatment")))

#### Estimate synth DD ####
synth = lapply(panel, function(x) lapply(x, function(x)
  synthdid::synthdid_estimate(x$Y, x$N0, x$T0)))

#### Extract the unit weights ####
weights = list(uw = lapply(synth, function(x) lapply(x, function(x)
  data.frame(synthdid_controls(x, weight.type='omega', mass = 1)) %>%
    mutate(fips = as.character(rownames(.))) %>% rename(uw = estimate.1))) %>%
    lapply(., rbindlist, idcol = "var") %>% rbindlist(., idcol = "tg"),

  tw = lapply(synth, function(x) lapply(x, function(x)
    data.frame(synthdid_controls(x, weight.type='lambda', mass = 1)) %>%
      mutate(score = as.numeric(rownames(.))) %>% rename(tw = estimate.1))) %>%
    lapply(., rbindlist, idcol = "var") %>% rbindlist(., idcol = "tg"))

#### Add the weights to the data sets ####
data = lapply(data, rbindlist) %>% rbindlist(.) %>%
  mutate(tg = as.character(tg)) %>%
  left_join(., weights$uw) %>% left_join(., weights$tw) %>%
  mutate(uw = ifelse(treated > 0, 1, uw),
         tw = ifelse(score >= 0, 1, tw)) %>%
  mutate(weights = uw*tw)

#### Estimate the effect ####
est = lapply(split(data, f = data$var), function(x)

  list(`SDID_TWFE` = feols(value ~ treatment |  year + fips, notes = F,
                           data = x, cluster = "cbsa_id",
                           weights = ~weights),

       `SDID_ETWFE` = etwfe(fml  = value ~ 1,
                            tvar = year,
                            gvar = enter_year,
                            data = x, weights = ~weights)))

#### Extract the fitted statistics ####
fitted = lapply(est, function(x) lapply(x, function(x)
  x = data.frame(N.Obs = nobs(x), R2 = r2(x)[2]))) %>%


  lapply(., rbindlist, idcol = "spec") %>%

  rbindlist(., idcol = "var")

#### Aggregate the mundalak estimator ###
est$pm25$SDID_ETWFE = emfx(est$pm25$SDID_ETWFE, vcov = F)

#### Extract the relevant coefficients ####
sum = lapply(est, function(x) lapply(x, function(x)
  x = data.frame(tidy(x)) |> select(term, estimate))) %>%

  lapply(., rbindlist, idcol = "spec") %>%
  rbindlist(., idcol = "var") %>%

  filter(term == "treatment" | term == "ATT" | term == ".Dtreat")


#### Construct the function to estimate the Jacknife SEs ####
jack_se = function(x, y){

  new = filter(x, cbsa_id != y)

  # run second stage
  lapply(split(new, f = new$var), function(x)

    list(`SDID_TWFE` = feols(value ~ treatment  |  year + fips, notes = F,
                             data = x, cluster = "cbsa_id", weights = ~weights),

         `SDID_ETWFE` = etwfe(fml  = value ~ 1,
                              tvar = year,
                              gvar = enter_year,
                              data = x, weights = ~weights) %>% emfx(., vcov = F)) %>%

      lapply(., function(x) x = tidy(x) %>% filter(term == "treatment" | term == "ATT" | term == ".Dtreat") %>%
               select(estimate))) %>%

    lapply(., rbindlist, idcol = "spec") %>%

    rbindlist(., idcol = "var")}

#### Run the 100 bootstraps ####
jacknife = lapply(unique(filter(data, weights > 0)$cbsa_id), function(x)
  jack_se(data, x))

#### Determine the standard error of the placebo estimates ####
jse = rbindlist(jacknife, idcol = "btr") %>%

  left_join(., select(sum, var, raw_est = estimate, spec))

#### Aggregate the Standard Error ####
jse = ungroup(jse) %>% group_by(var, spec) %>%
  mutate(diff = (estimate - raw_est)^2, df = (n()-1)/n()) %>%
  group_by(var, spec) %>% summarise(jse = sum(diff)*unique(df)) %>%
  mutate(jse = sqrt(jse))

#### Add the placebo SE to the data set ####
sum = left_join(sum, jse)  %>%
  mutate(jp = 2*pnorm(abs(estimate/jse), lower.tail = F))

#### Re-organize the data set ####
sum = select(sum, c(var, spec, estimate, std.error = jse, p.value = jp))

#### Add the pre-treatment average ####
sum = data %>% filter(treatment == 0 & treated == 1) %>%
  group_by(var) %>% summarise(pt_value = mean(value, na.rm = T)) %>%
  left_join(sum, .)

#### Include the number of observations ####
sum$N.Counties = length(unique(data$fips))
sum$N.Periods = length(unique(data$year))
sum$N.Cohorts = length(unique(data$enter_year))

#### Add the fitted values ####
sum = left_join(sum, fitted)

#### Check the final data set ####
sum %>% head(.)

#### Save the summary values ####
write_rds(sum, file = "03_gen/05_results/sdid_pm25.rds")

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### ____________________________________________________________________ ####
#### Table H10 -- Cols (1) to (3): Effects only with treated units from 2012 ####
#### Table H11 -- Cols (4) to (6): Effects only with treated units from 2012 ####
#### ____________________________________________________________________ ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Load the packages ####
library(fixest)
library(data.table)
library(tidyverse)
library(stringr)
library(purrr)

#### Load the regressions data ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")

#### Filter the data to the preferred specification ####
data = data %>% filter(score %in% seq(-5, 0, 1) | treated == 0)
data = filter(data, enter_year != 2010, enter_year!= 2011)

#### Run the regressions
est = list(TWFE = feols(avg ~ treatment | year + fips,
                        data =  data,
                        cluster = "cbsa_id"),

           `E-TWFE` = etwfe(fml  = avg ~ 1, tvar = year,  gvar = enter_year,
                            data = data, vcov = ~cbsa_id),

           `SADD` = feols(avg ~ sunab(enter_year, year) |
                            year + fips, data = data,
                          cluster = "cbsa_id") %>% summary(agg = "att"))

#### Extract the coefficient table ####
sum = lapply(est, function(x)
  data.frame(R2 = r2(x)[2], BIC = BIC(x), N.Obs = nobs(x))) %>%
  rbindlist(., idcol = "estimator")

#### Aggregate the E-TWFE estimator ####
est <- lapply(est, function(x)  {
  if("etwfe" %in% class(x)) {x <- emfx(x)}; x})

#### Extract the relevant coefficients ####
sum = lapply(est, function(x)

  data.frame(tidy(x)) |>
    select(term, estimate, std.error, p.value)) %>%

  rbindlist(., idcol = "estimator") %>%

  filter(term == "treatment" | term == "ATT" | term == ".Dtreat")%>%

  left_join(., sum)

#### Add the pre-treatment average ####
sum$pt_value= data %>% ungroup() %>%  filter(treatment == 0 & treated == 1) %>%
  filter(score %in% seq(-5, 0, 1)) %>%
  summarise(`pt_value` = mean(avg))  %>% unlist()

#### Include the number of observations ####
sum$N.Cohorts = length(unique(data$enter_year))
sum$N.Counties = length(unique(data$fips))
sum$N.Periods = length(unique(data$year))

#### Check the final data set ####
sum %>% head(.)

#### Save the summary values ####
write_rds(sum, file = "03_gen/05_results/only_2012_2016_cohorts.rds")

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()
#### _____________________________________________________________________ ####
#### Table H10 -- Cols (4) and (5): Effects only with not-yet treated ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Load the packages ####
library(fixest)
library(data.table)
library(tidyverse)
library(stringr)
library(purrr)

#### Load the regressions data ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")

#### Filter the data to the prefered specification ####
data = data %>% filter(score %in% seq(-5, 0, 1))
data = filter(data, treated == 1)

#### Make a balanced panel for the Goodman bacon decomposition ####
data = data %>% group_by(fips) %>% mutate(count = n())
data %>% group_by(count) %>% summarise(n())
data = filter(data, count == 6)

#### Run the regressions
est = list(TWFE = feols(avg ~ treatment | year + fips,
                        data =  data,
                        cluster = "cbsa_id"),

           `E-TWFE` = etwfe(fml  = avg ~ 1, tvar = year,  gvar = enter_year,
                            data = data, vcov = ~cbsa_id))

#### Extract the coefficient table ####
sum = lapply(est, function(x)
  data.frame(R2 = r2(x)[2], BIC = BIC(x), N.Obs = nobs(x))) %>%
  rbindlist(., idcol = "estimator")

#### Aggregate the E-TWFE estimator ####
est <- lapply(est, function(x)  {
  if("etwfe" %in% class(x)) {x <- emfx(x)}; x})

#### Extract the relevant coefficients ####
sum = lapply(est, function(x)

  data.frame(tidy(x)) |>
    select(term, estimate, std.error, p.value)) %>%

  rbindlist(., idcol = "estimator") %>%

  filter(term == "treatment" | term == "ATT" | term == ".Dtreat")%>%

  left_join(., sum)

#### Add the pre-treatment average ####
sum$pt_value= data %>% ungroup() %>%  filter(treatment == 0 & treated == 1) %>%
  filter(score %in% seq(-5, 0, 1)) %>%
  summarise(`pt_value` = mean(avg))  %>% unlist()

#### Include the number of observations ####
sum$N.Cohorts = length(unique(data$enter_year))
sum$N.Counties = length(unique(data$fips))
sum$N.Periods = length(unique(data$year))

#### Check the final data set ####
sum %>% head(.)

#### Save the summary values ####
write_rds(sum, file = "03_gen/05_results/only_nyt_controls.rds")

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### ____________________________________________________________________ ####
#### Table H11 -- Cols (1) to (3): Effects using the enter year of Uber x ####
#### ____________________________________________________________________ ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Load the packages ####
library(fixest)
library(data.table)
library(etwfe)
library(tidyverse)
library(stringr)
library(broom)
library(purrr)

#### Uber date ####
uber = read_rds("03_gen/03_uber/uber_x.rds")

#### Only keep the relevant data ####
uber = mutate(uber, enter_year = year(enter_date),
              fips = as.character(fips) %>% str_pad("left", pad = "0", width = 5)) %>%
  select(fips, enter_year)

#### Load the regressions data ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")

#### Exclude the Uber black variables ####
data = select(data, -c(enter_year)) %>% left_join(uber)
data = mutate(data, treated = ifelse(is.na(enter_year) == F, 1, 0))
data = mutate(data, enter_year = ifelse(is.na(enter_year), 2020, enter_year))
data = mutate(data, score = year - enter_year)
data = mutate(data, treatment = ifelse(treated == 1 & year >= enter_year, 1, 0))

#### Filter the data to the preferred specification ####
data = data %>% filter(score %in% seq(-5, 0, 1) | treated == 0)

#### Run the regressions
est = list(TWFE = feols(avg ~ treatment | year + fips,
                        data =  data,
                        cluster = "cbsa_id"),

           `E-TWFE` = etwfe(fml  = avg ~ 1, tvar = year,  gvar = enter_year,
                            data = data, vcov = ~cbsa_id) ,

           `SA-DD` <- feols(
             avg ~ sunab(enter_year, year) | fips + year,
             data = data,cluster = "cbsa_id") %>% summary(agg = "att"))

#### Extract the coefficient table ####
sum = lapply(est, function(x)
  data.frame(R2 = r2(x)[2], BIC = BIC(x), N.Obs = nobs(x))) %>%
  rbindlist(., idcol = "estimator")

#### Aggregate the E-TWFE estimator ####
est <- lapply(est, function(x)  {
  if("etwfe" %in% class(x)) {x <- emfx(x)}; x})

#### Extract the relevant coefficients ####
sum = lapply(est, function(x)

  data.frame(tidy(x)) |>
    select(term, estimate, std.error, p.value)) %>%

  rbindlist(., idcol = "estimator") %>%

  filter(term == "treatment" | term == "ATT" | term == ".Dtreat")%>%

  left_join(., sum)

#### Add the pre-treatment average ####
sum$pt_value= data %>% ungroup() %>%  filter(treatment == 0 & treated == 1) %>%
  filter(score %in% seq(-5, 0, 1)) %>%
  summarise(`pt_value` = mean(avg))  %>% unlist()

#### Include the number of observations ####
sum$N.Cohorts = length(unique(data$enter_year))
sum$N.Counties = length(unique(data$fips))
sum$N.Periods = length(unique(data$year))

#### Check the final data set ####
sum

#### Save the summary values ####
write_rds(sum, file = "03_gen/05_results/introduction_uber_x.rds")

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### _____________________________________________________________________ ####
#### Table H12 -- Cols (1) to (3): Estimates across different granularities ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Load the packages ####
library(conflicted)
library(data.table)
library(tidyverse)
library(stringr)
library(fixest)
library(purrr)
library(etwfe)
library(broom)

#### Solve conflicts ####
conflict_prefer("month", "lubridate")
conflict_prefer("filter", "dplyr")

#### Load the regressions data ####
yearly = read_rds("03_gen/04_reg/yearly_reg.rds")
monthly = read_rds("03_gen/04_reg/year_monthly_reg.rds")
daily = read_rds("03_gen/04_reg/daily_reg.rds")

#### Convert the year-month objects to numeric and factor for ETWFE and SADD ####
daily = mutate(daily, score = date - enter_date)
daily = mutate(daily, treated = ifelse(enter_date > as.Date("2016-12-31"),1, 0))
daily = mutate(daily, year = year(date), month = month(date), dow = weekdays(date))
daily = mutate(daily, state = substr(fips, 1,2))
monthly = mutate(monthly, state = substr(fips, 1,2))

#### Run the regressions
est = list(est_y = feols(avg ~ treatment | year + fips,
                         data =  yearly %>% filter(score %in% seq(-5, 0, 1)| treated == 0)),

           est_m = feols(aqi ~ treatment + temp + temp^2 + rain | year^month + fips + state^month,
                         data =  monthly %>% filter(score %in% seq(-72, 12, 1)| treated == 0),
                         cluster = "cbsa_id"),

           est_d = feols(aqi ~ treatment + tmp + tmp^2 + rain + atm|
                           fips^year + state^month + state^dow,
                         data =  daily %>% filter(score %in% seq(-1830, 366, 1)| treated == 0),
                         cluster = "cbsa_id"))

#### Extract the relevant coefficients ####
sum = lapply(est, function(x)

  data.frame(tidy(x), R2 = r2(x)[2], BIC = BIC(x), N.Obs = nobs(x))) %>%

  rbindlist(., idcol = "agg") %>%

  filter(term == "treatment")

#### Add the pre-treatment average ####
sum = list(est_y = yearly %>% ungroup() %>%
             filter(score %in% seq(-5, 0, 1)| treated == 0) %>%
             summarise(`pt_value` = mean(avg)),

           est_m = monthly %>% ungroup() %>%
             filter(score %in% seq(-72, 12, 1)| treated == 0) %>%
             summarise(`pt_value` = mean(aqi)),

           est_d = daily %>% ungroup() %>%
             filter(score %in% seq(-1830, 366, 1)| treated == 0) %>%
             summarise(`pt_value` = mean(aqi))) %>%
  bind_rows(., .id = "agg") %>% left_join(sum, .)


#### Check the final data set ####
sum

#### Save the summary values ####
write_rds(sum, file = "03_gen/05_results/att_granularity.rds")

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

### _____________________________________________________________________ ####
#### Table H12 -- Cols (4) to (6): Estimates with stacked panel ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Load the packages ####
library(conflicted)
library(data.table)
library(tidyverse)
library(stringr)
library(fixest)
library(purrr)
library(etwfe)
library(broom)

#### Solve conflicts ####
conflict_prefer("month", "lubridate")
conflict_prefer("filter", "dplyr")

#### Load the regressions data ####
data = read_rds("03_gen/04_reg/yearly_rolling_reg.rds")

#### Filter the data to the prefered specification ####
data = data %>% filter(year_int %in% seq(-5, 0, 1))
#data = mutate(data, fips = paste0(fips, subpanel_id))
#data = mutate(data, cbsa_id = paste0(cbsa_id, subpanel_id))

#### Create the different treatment groups ####
data = data %>% group_by(subpanel_id) %>% mutate(g = ifelse(treated == 1, 0, 6))

#### Run the regressions ####
#### Note: The original table had the TWFE and E-TWFE columns swapped.       ####
#### The SA-DD estimator reduces to TWFE in this setting because there is a   ####
#### single treated cohort (g=0) and a single post-treatment period           ####
#### (year_int=0), so there is no treatment effect heterogeneity for Sun &    ####
#### Abraham (2021) to correct. We therefore report the TWFE estimate for     ####
#### the SA-DD column. The original sunab() specification no longer works     ####
#### with fixest >= 0.13.                                                     ####

est_ols = feols(aqi ~ treatment | year^subpanel_id + fips^subpanel_id,
                data = data, cluster = "cbsa_id^subpanel_id")

est_etwfe = etwfe(fml = aqi ~ 1, tvar = year_int, gvar = g, gref = 6,
                  data = data, vcov = ~cbsa_id^subpanel_id, ivar = subpanel_id)
agg_etwfe = emfx(est_etwfe)

#### Extract the coefficient table ####
sum_ols = data.frame(
  estimator = "ols", term = "treatment",
  estimate  = coef(est_ols)["treatment"],
  std.error = se(est_ols)["treatment"],
  p.value   = pvalue(est_ols)["treatment"],
  R2 = r2(est_ols)[2], BIC = BIC(est_ols), N.Obs = nobs(est_ols))

t_etwfe = tidy(agg_etwfe)
sum_etwfe = data.frame(
  estimator = "e_twfe", term = t_etwfe$term[1],
  estimate  = t_etwfe$estimate[1],
  std.error = t_etwfe$std.error[1],
  p.value   = t_etwfe$p.value[1],
  R2 = r2(est_etwfe)[2], BIC = BIC(est_etwfe), N.Obs = nobs(est_etwfe))

#### SA-DD = TWFE in this setting (single cohort, single post-period) ####
sum_sa = sum_ols
sum_sa$estimator = "sa"

sum = rbindlist(list(sum_ols, sum_etwfe, sum_sa))

#### Add the pre-treatment average ####
sum$pt_value= data %>% ungroup() %>%
  filter(treatment == 0 & treated == 1) %>%
  filter(year_int %in% seq(-5, 0, 1)) %>%
  summarise(`pt_value` = mean(aqi))  %>% unlist()

#### Include the number of observations ####
sum$N.Cohorts = length(unique(data$g))
sum$N.Counties = length(unique(data$fips))
sum$N.Periods = length(unique(data$year_int))

#### Check the final data set ####
sum %>% head(.)

#### Save the summary values ####
write_rds(sum, file = "03_gen/05_results/att_yearly_rolling.rds")

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### _____________________________________________________________________ ####
#### Table I.19: Effects for share of gas and green cars in Cal. ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Load the packages ####
library(tidyverse)
library(synthdid)
library(data.table)
library(fixest)
library(broom)
library(etwfe)

#### Load the data ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")
gas = read_csv("02_data/06_other/15_california_gas_consumption/california_gas_city_fips.csv")
gas_price = read.csv("02_data/06_other/6_Gasoline_price_data_states/statesgasoline.csv")
hybrid = read.csv("02_data/06_other/10_vehicle_california/env_cars_ca.csv")

#### Add the state FIPS code to the data ####
data = mutate(data, statesfips = substr(fips,1,2) %>% as.numeric(.))
data = filter(data, statesfips == 6)

#### Only keep California ####
hybrid = select(hybrid, fips, year, type = DashboardFuelTypeGroup, n_cars = NumberofVehicles)
hybrid$type = gsub("Plug-in Hybrid \\(PHEV\\)|Gasoline Hybrid" ,"Hybrid", hybrid$type)
hybrid$type = gsub("Battery Electric \\(BEV\\)|Fuel Cell \\(FCEV\\)" ,"Electric", hybrid$type)
hybrid = filter(hybrid, type != "Other")
hybrid = hybrid %>% group_by(fips, year, type) %>% summarise(n_cars = sum(n_cars))
hybrid = hybrid %>% group_by(fips, year) %>% mutate(share = (n_cars/sum(n_cars)))
hybrid = hybrid %>% select(-n_cars) %>% spread(., type, share)

#### Transform the cars data from wide to long format ####
gas = select(gas, fips, year, sales, stations) %>%
  mutate(fips = as.numeric(fips)) %>% group_by(fips, year) %>%
  summarise(sales = mean(sales, na.rm = T), stations = mean(stations, na.rm =T))

#### Left join the available cars to the Uber data ####
data = left_join(data %>% mutate(fips = as.numeric(fips)), gas)

data = select(data, fips, cbsa_id, score, treatment, treated,
              statesfips, year, enter_year, sales, stations, pop, cty_size)

#### Include gasoline prices at the state level ####
data = data %>%
  left_join(., gas_price %>% select(-stusps))
data = left_join(data, hybrid)
data = as.data.frame(data)


#### Only keep data with valid cars-share data ####
# Only data from 2010
test = filter(data, is.na(Gasoline) == F)

#### Make electric cars zeros when they are NA ####
test$Electric = ifelse(is.na(test$Electric), 0, test$Electric)
test$clean = test$Hybrid + test$Electric

#### Run the ETWFE estimates for each ####
est = list(etwfe(clean   ~ 1,
                 tvar = year,
                 gvar = enter_year,
                 data = test, vcov = ~cbsa_id,
                 weights = ~pop),


           etwfe(clean   ~ 1,
                 tvar = year,
                 gvar = enter_year,
                 data = test|> filter(score %in% seq(-5, 3, 1) | treated == 0), vcov = ~cbsa_id,
                 weights = ~pop),

           etwfe(clean   ~ 1,
                 tvar = year,
                 gvar = enter_year,
                 data = test|> filter(score %in% seq(-5, 0, 1) | treated == 0), vcov = ~cbsa_id,
                 weights = ~pop),

           etwfe(Gasoline   ~ 1,
                 tvar = year,
                 gvar = enter_year,
                 data = test, vcov = ~cbsa_id,
                 weights = ~pop),



           etwfe(Gasoline   ~ 1,
                 tvar = year,
                 gvar = enter_year,
                 data = test|> filter(score %in% seq(-5, 3, 1) | treated == 0), vcov = ~cbsa_id,
                 weights = ~pop),


           etwfe(Gasoline   ~ 1,
                 tvar = year,
                 gvar = enter_year,
                 data = test|> filter(score %in% seq(-5, 0, 1) | treated == 0), vcov = ~cbsa_id,
                 weights = ~pop))



#### Extract the fitted statistics ####
fitted = lapply(est, function(x)
  x = data.frame(N.Obs = nobs(x), R2 = r2(x)[2])) %>%
  rbindlist(., idcol = "spec")

#### Aggregate the E-TWFE estimator ####
est <- lapply(est, function(x) {
  if("etwfe" %in% class(x)) {x <- emfx(x)}; x})

#### Extract the relevant coefficients ####
sum = lapply(est, function(x)
  x = data.frame(tidy(x)) |>
    select(term, estimate, std.error, p.value)) %>%

  rbindlist(., idcol = "spec") %>%

  filter(term == "treatment" | term == "ATT" | term == ".Dtreat")

#### Add the pre-treatment average ####
sum = test %>% filter(treatment == 0 & treated == 1) %>% ungroup() %>%
  summarise(`pt_value_gas` = mean(Gasoline, na.rm = T))  %>%
  crossing(tibble(spec = paste0(c(1:6)) %>% as.numeric(.))) %>%
  left_join(sum, .)

sum = test %>% filter(treatment == 0 & treated == 1) %>% ungroup() %>%
  summarise(`pt_value_clean` = mean(clean, na.rm = T))  %>%
  crossing(tibble(spec = paste0(c(1:6)) %>% as.numeric(.))) %>%
  left_join(sum, .)

sum = left_join(sum, fitted)

#### Add the number of counties ####
sum$N.Counties = length(unique(test$fips))
sum$N.Periods = length(unique(test$year))
sum$N.Cohorts = length(unique(test$enter_year))

#### Plot the coefficients ####
sum

#### Save the summary values ####
write_rds(sum, file = "03_gen/05_results/did_gas_share.rds")

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()


#### _____________________________________________________________________ ####
#### Figure J.1 (left). Heterogeneous effects by weekday (annual) ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Load the packages ####
library(tidyverse)
library(synthdid)
library(data.table)
library(fixest)
library(broom)
library(readxl)
library(sf)

#### Load the regressions data ####
data = read_rds("03_gen/04_reg/yearly_weekday_reg.rds")

#### Run the regression across all dependent variables and models ####
est  = feols(avg ~  i(weekday, treatment, ref = "Friday") +
               tmp + rain  | year + fips + weekday,
             data = data %>% filter(score %in% seq(-5, 0, 1)),
             cluster = "cbsa_id")

#### Extract the relevant coefficients ####
sum = data.frame(tidy(est)) %>%

  filter(grepl("treatment", term)) %>%
  select(estimate, std.error, term)

#### Extract the weekday ####
sum = mutate(sum, term = gsub(".*::", "", term))
sum = mutate(sum, term = gsub(":.*", "", term))
sum = mutate(sum, term = substr(term, 1,2))
sum = rename(sum, var = term)

#### Add the Friday Estimate for Comparison ####
sum = rbind(sum, data.frame(estimate = 0, std.error = 0, var = "Fr")) %>%
  mutate(var = factor(var, levels = c("Fr", "Mo", "Tu", "We","Th", "Sa","Su")))

#### Compute the average pre-treatment AQI across weekdays ####
mean_aqi = data |> filter(treatment == 0) |>
  group_by(weekday) |> summarise(mean_aqi = mean(avg, na.rm = T)) |>
  rename(var = weekday) %>% mutate(var = substr(var, 1,2))

#### Add the average aqi of each region ####
sum = left_join(sum, mean_aqi)

#### Compute the average pre-treatment AQI across regions ####
fes = data |> filter(treatment == 0) |>
  summarise(N.Counties = length(unique(fips)),
            N.Periods = length(unique(year)),
            N.Obs = nobs(est))

#### Add the average aqi of each region ####
sum$N.Counties = fes$N.Counties
sum$N.Periods = fes$N.Periods
sum$N.Obs = fes$N.Obs

#### Check the data set ####
glimpse(sum)

#### Save the summary values ####
write_rds(sum, file = "03_gen/05_results/twfe_yearly_weekdays.rds")

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### _____________________________________________________________________ ####
#### Figure J.1 (right). Heterogeneous effects by weekday (daily) ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Load the packages ####
library(tidyverse)
library(synthdid)
library(data.table)
library(fixest)
library(broom)

#### Load the data ####
data = read_rds("03_gen/04_reg/daily_reg.rds")

#### Create the weekday variable ####
data = mutate(data, weekday = weekdays(date), year = year(date), month = month(date))

#### Run the regression estimates #####
data = mutate(data, weekday = substr(weekday, 1,2)) %>%
  mutate(weekday = factor(weekday, levels = c("Su", "Mo", "Tu", "We","Th", "Fr", "Sa"))) %>%
  mutate(weekday = relevel(weekday, ref = "Fr"))

#### Run both specifications #####
est = feols(aqi ~  i(weekday, treatment, ref = "Fr") + rain + tmp|
              fips^year + fips^month + fips^weekday,
            data = data, mem.clean = T, cluster = "cbsa_id")


#### Extract the relevant coefficients ####
sum = data.frame(estimate = data.frame(est$coeftable)[,1],
                 std.error = data.frame(est$coeftable)[,2],
                 var = rownames(est$coeftable))

#### Extract the weekday ####
sum = filter(sum, grepl("treatment", var))
sum = sum %>%  mutate(var = gsub(":treatment|weekday::|month::", "", var))

#### Add the Friday Estimate for Comparison ####
sum = rbind(sum, data.frame(estimate = 0, std.error = 0, var = "Fr")) %>%
  mutate(var = factor(var, levels = c("Fr", "Mo", "Tu", "We","Th", "Sa","Su")))

#### Compute the average pre-treatment AQI across regions ####
mean_aqi = data |> filter(treatment == 0) |>
  group_by(weekday) |> summarise(mean_aqi = mean(aqi, na.rm = T)) |>
  rename(var = weekday)

#### Add the average aqi of each region ####
sum = left_join(sum, mean_aqi)

#### Compute the average pre-treatment AQI across regions ####
fes = data |> filter(treatment == 0) |>
  summarise(N.Counties = length(unique(fips)),
            N.Periods = length(unique(year)),
            N.Obs = nobs(est))

#### Add the average aqi of each region ####
sum$N.Counties = fes$N.Counties
sum$N.Periods = fes$N.Periods
sum$N.Obs = fes$N.Obs

#### Check the summary data ####
glimpse(sum)

#### Save the summary values ####
write_rds(sum, file = "03_gen/05_results/twfe_daily_weekdays.rds")

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### _____________________________________________________________________ ####
#### Figure J.2 (left). Heterogeneous effects by month (annual) ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Load the packages ####
library(tidyverse)
library(synthdid)
library(data.table)
library(fixest)
library(broom)
library(readxl)
library(sf)

#### Load the regressions data ####
data = read_rds("03_gen/04_reg/yearly_monthly_reg.rds")

#### Transform the month to a character vector ####
data = mutate(data, month = month.abb[as.numeric(month)])

#### Run the regression across all dependent variables and models ####
est  = feols(avg ~   i(month, treatment, ref = "Jan") +
               tmp + rain  + tmp^2| year + fips + month,
             data = data %>% filter(score %in% seq(-5, 0, 1)),
             cluster = "cbsa_id")

#### Extract the relevant coefficients ####
sum = data.frame(tidy(est)) %>%

  filter(grepl("treatment", term)) %>%
  select(estimate, std.error, term)

#### Extract the weekday ####
sum = mutate(sum, term = gsub(".*::", "", term))
sum = mutate(sum, term = gsub(":.*", "", term))
sum = rename(sum, var = term)

#### Add the January Estimate for Comparison ####
sum = rbind(sum, data.frame(estimate = 0, std.error = 0, var = "Jan"))

#### Compute the average pre-treatment AQI across weekdays ####
mean_aqi = data |> filter(treatment == 0) |>
  group_by(month) |> summarise(mean_aqi = mean(avg, na.rm = T)) |>
  rename(var = month)

#### Add the average aqi of each region ####
sum = left_join(sum, mean_aqi)

#### Compute the average pre-treatment AQI across regions ####
fes = data |> filter(treatment == 0) |>
  summarise(N.Counties = length(unique(fips)),
            N.Periods = length(unique(year)),
            N.Obs = nobs(est))

#### Add the average aqi of each region ####
sum$N.Counties = fes$N.Counties
sum$N.Periods = fes$N.Periods
sum$N.Obs = fes$N.Obs

#### Check the data set ####
glimpse(sum)

#### Save the summary values ####
write_rds(sum, file = "03_gen/05_results/twfe_yearly_months.rds")

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### _____________________________________________________________________ ####
#### Figure J.2 (right). Heterogeneous effects by month (daily) ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Load the data ####
data = read_rds("03_gen/04_reg/daily_reg.rds")

#### Create the weekday variable ####
data = mutate(data, weekday = weekdays(date), year = year(date), month = month(date))

#### Run the regression estimates #####
data = mutate(data, month = lubridate::month(date, label = T, abbr = T))

#### Run both specifications #####
est = feols(aqi ~  i(month, treatment, ref = "Jan") + rain + tmp + wsp|
              fips^year + fips^month + fips^weekday,
            data = data, mem.clean = T, cluster = "cbsa_id")

#### Check the results ####
iplot(est)

#### Extract the relevant coefficients ####
sum = data.frame(est = data.frame(est$coeftable)[,1],
                 se = data.frame(est$coeftable)[,2],
                 var = rownames(est$coeftable))

#### Extract the weekday ####
sum = filter(sum, grepl("treatment", var))
sum = sum %>%  mutate(var = gsub(":treatment|weekday::|month::", "", var))

#### Add the January Estimate for Comparison ####
sum = rbind(sum, data.frame(est = 0, se = 0, var = "Jan"))

#### Compute the average pre-treatment AQI across regions ####
mean_aqi = data |> filter(treatment == 0) |>
  group_by(month) |> summarise(mean_aqi = mean(aqi, na.rm = T)) |>
  rename(var = month)

#### Add the average aqi of each region ####
sum = left_join(sum, mean_aqi)

#### Extract the number of observations ####
fes = data |> filter(treatment == 0) |>
  summarise(N.Counties = length(unique(fips)),
            N.Periods = length(unique(year)),
            N.Obs = nobs(est))

#### Add the number of observations and FEs ####
sum$N.Counties = fes$N.Counties
sum$N.Periods = fes$N.Periods
sum$N.Obs = fes$N.Obs

#### Check the summary data ####
glimpse(sum)

#### Save the summary values ####
write_rds(sum, file = "03_gen/05_results/twfe_daily_months.rds")

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### _____________________________________________________________________ ####
#### Figure J.4 (left): Heterogeneous effects by hour (Annual Average) ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Load the packages ####
library(conflicted)
library(tidyverse)
library(fixest)
library(broom)
library(zoo)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the yearly-hourly regression data ####
yearly = read_rds("03_gen/09_revision/yearly_hourly_reg.rds")

#### Prepare hour as factor ####
yearly = mutate(yearly, hour = as.factor(as.numeric(hour)))

#### Run the regression ####
est = feols(avg ~ i(hour, treatment, ref = "5") +
              tmp + rain + I(tmp^2) | year + fips + hour^fips,
            data = yearly %>% filter(score %in% seq(-5, 0, 1)),
            cluster = "cbsa_id")

#### Extract the relevant coefficients ####
sum = data.frame(est = (est$coeftable)[,1],
                 se = data.frame(est$coeftable)[,2],
                 var = rownames(est$coeftable)) %>%
  filter(grepl("treatment", var))

#### Clean variable names ####
sum = sum %>%
  mutate(var = gsub(":treatment|hour::", "", var)) %>%
  mutate(var = as.numeric(var))

#### Add the reference hour (5 AM) ####
sum = rbind(sum, data.frame(est = 0, se = 0, var = 5))

#### Compute the average pre-treatment AQI by hour ####
mean_aqi = yearly |> filter(treatment == 0) |>
  group_by(hour) |> summarise(pt_value = mean(avg, na.rm = T)) |>
  rename(var = hour) %>% mutate(var = as.numeric(as.character(var)))

#### Add the average aqi of each hour ####
sum = left_join(sum, mean_aqi)

#### Add sample info ####
sum$N.Counties = length(unique(yearly$fips))
sum$N.Periods = length(unique(yearly$year))
sum$N.Obs = nobs(est)

#### Check the data set ####
glimpse(sum)

#### Save the summary values ####
write_rds(sum, file = "03_gen/09_revision/nc_yearly_diff_twfe.rds")

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### _____________________________________________________________________ ####
#### Figure J.4 (right): Heterogeneous effects by hour (Hourly Average) ####
#### NOTE: This regression requires ~70GB memory (61M rows). Run on HPC. ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Load the packages ####
library(conflicted)
library(tidyverse)
library(fixest)
library(zoo)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the hourly regression data ####
data = read_rds("03_gen/09_revision/hourly_reg_aqi.rds")

#### Add cbsa_id from uber.rds ####
uber = read_rds("03_gen/03_uber/uber.rds")
data = left_join(data, uber %>% select(fips, cbsa_id), by = "fips")
rm(uber); gc()

#### Prepare hour as factor ####
data = mutate(data, hour = as.factor(as.numeric(hour)))

#### Run the regression ####
est = feols(now_cast ~ tmp + rain + i(hour, treatment, ref = "5") |
              fips^year^month + fips^weekday + fips^hour,
            data = data, mem.clean = T,
            cluster = "cbsa_id^year^month")

#### Extract the relevant coefficients ####
sum = data.frame(est = (est$coeftable)[,1],
                 se = data.frame(est$coeftable)[,2],
                 var = rownames(est$coeftable)) %>%
  filter(grepl("treatment", var))

#### Clean variable names ####
sum = sum %>%
  mutate(var = gsub(":treatment|hour::", "", var)) %>%
  mutate(var = as.numeric(var))

#### Add the reference hour (5 AM) ####
sum = rbind(sum, data.frame(est = 0, se = 0, var = 5))

#### Compute the average pre-treatment NowCast by hour ####
mean_aqi = data |> filter(treatment == 0) |>
  group_by(hour) |> summarise(pt_value = mean(now_cast, na.rm = T)) |>
  rename(var = hour) %>% mutate(var = as.numeric(as.character(var)))

#### Add the average NowCast of each hour ####
sum = left_join(sum, mean_aqi)

#### Add sample info ####
sum$N.Counties = length(unique(data$fips))
sum$N.Periods = length(unique(data$date))*24
sum$N.Obs = nobs(est)

#### Check the data set ####
glimpse(sum)

#### Save the summary values ####
write_rds(sum, file = "03_gen/09_revision/nc_hourly_diff_twfe.rds")

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### _____________________________________________________________________ ####
#### Table J.1 (Cols 1-3): Effect of Uber on the NowCast AQI (Annual)   ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Load the packages ####
library(conflicted)
library(tidyverse)
library(data.table)
library(fixest)
library(broom)
library(zoo)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

#### Load the yearly NowCast data ####
yearly = read_rds("03_gen/09_revision/yearly_nc_reg.rds")

#### Run the three annual specifications ####
est_annual = list(
  `(1)` = feols(avg ~ treatment | year + fips,
                data = yearly %>% filter(score %in% seq(-5, 0, 1)),
                cluster = "cbsa_id"),

  `(2)` = feols(avg ~ treatment + tmp + rain | year + fips,
                data = yearly %>% filter(score %in% seq(-5, 0, 1)),
                cluster = "cbsa_id"),

  `(3)` = feols(avg ~ treatment + tmp + rain |
                  year + fips + NAAQS + forest_fire + new_pp + ret_pp,
                data = yearly %>% filter(score %in% seq(-5, 0, 1)),
                cluster = "cbsa_id"))

#### Extract the relevant coefficients ####
sum_annual = lapply(est_annual, function(x)
  x = data.frame(tidy(x), N.Obs = x$nobs,
                 R2 = r2(x)[2],
                 BIC = BIC(x))) %>%
  rbindlist(., idcol = "spec") %>%
  filter(term == "treatment")

#### Add the pre-treatment average ####
sum_annual$pt_value = (yearly %>% filter(treatment == 0 & treated == 1) %>%
                         filter(score %in% seq(-5, 0, 1)) %>%
                         summarise(pt_value = mean(avg)))$pt_value

#### Add the number of counties and periods ####
sum_annual$N.Counties = length(unique(filter(yearly, score %in% seq(-5, 0, 1))$fips))
sum_annual$N.Periods = length(unique(yearly$year))

#### Save the annual results ####
write_rds(sum_annual, file = "03_gen/09_revision/nc_yearly_twfe.rds")

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### _____________________________________________________________________ ####
#### Table J.1 (Cols 4-6): Effect of Uber on the NowCast AQI (Hourly)   ####
#### NOTE: This regression requires ~60GB memory (61M rows). Run on HPC. ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Load the packages ####
library(conflicted)
library(tidyverse)
library(data.table)
library(fixest)
library(broom)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the hourly NowCast data ####
data = read_rds("03_gen/09_revision/hourly_reg_aqi.rds")

#### Add cbsa_id from uber.rds ####
uber = read_rds("03_gen/03_uber/uber.rds")
data = left_join(data, uber %>% select(fips, cbsa_id), by = "fips")
rm(uber); gc()

#### Compute score and treated for filtering ####
data = mutate(data, enter_year = lubridate::year(enter_date))
data = mutate(data, score = year - enter_year)
data = mutate(data, treated = ifelse(enter_year < 2017, 1, 0))

#### Filter to score in seq(-5, 0, 1) ####
data_filtered = data %>% filter(score %in% seq(-5, 0, 1))

#### Run the three hourly specifications ####
est_hourly = list(
  `(4)` = feols(now_cast ~ treatment | fips^year^month + weekday + hour,
                data = data_filtered,
                cluster = "cbsa_id^date"),

  `(5)` = feols(now_cast ~ treatment | fips^year^month + fips^weekday + fips^hour,
                data = data_filtered,
                cluster = "cbsa_id^date"),

  `(6)` = feols(now_cast ~ treatment | fips^year^month + fips^weekday + fips^hour + tmp_bin + rain_bin,
                data = data_filtered,
                cluster = "cbsa_id^date"))

#### Extract the relevant coefficients ####
sum_hourly = lapply(est_hourly, function(x)
  x = data.frame(tidy(x), N.Obs = x$nobs,
                 R2 = r2(x)[2],
                 BIC = BIC(x))) %>%
  rbindlist(., idcol = "spec") %>%
  filter(term == "treatment")

#### Add the pre-treatment average ####
sum_hourly$pt_value = (data_filtered %>% filter(treatment == 0 & treated == 1) %>%
                         summarise(pt_value = mean(now_cast)))$pt_value

#### Add the number of counties and periods ####
sum_hourly$N.Counties = length(unique(data_filtered$fips))
sum_hourly$N.Periods = length(unique(as.character(data_filtered$date)))*24

#### Save the hourly results ####
write_rds(sum_hourly, file = "03_gen/09_revision/nc_hourly_twfe.rds")

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### _____________________________________________________________________ ####
#### Tables I.15 & I.16: NYC Taxi Autoregressive Models                   ####
#### _____________________________________________________________________ ####
#### Original code was in Stata:
#### 04_scripts/old_scripts/Time_series_analysis/data_time_series_daily_post_4july2022.do
#### This R replication uses arima() with QR-based collinearity handling
#### to match Stata's automatic dropping of collinear regressors.
#### Coefficients match to 3 decimal places. SEs differ slightly because
#### Stata uses OPG (outer product of gradients) standard errors while
#### R's arima() uses Hessian-based standard errors.

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Load the packages ####
library(tidyverse)

#### Load the data ####
data <- read_csv("02_data/06_other/20_NYC_time_series/nyc_taxi_2009_2017_daily.csv",
                  show_col_types = FALSE)

#### Create variables matching the Stata code ####
data$date <- as.Date(data$trip_pickup_datetime1)
data <- data %>%
  mutate(
    ln_trip = log(trip),
    ln_prcp = log(prcp + 1),
    ln_tmax = log(tmax),
    ln_tmin = log(pmax(tmin, 0.01)),
    year_val = year(date),
    month_val = month(date)
  )

#### Create ALL dummies exactly as Stata does (m_1-m_12, y_1-y_9) ####
for (m in 1:12) data[[paste0("m_", m)]] <- as.integer(data$month_val == m)
for (y in 1:9)  data[[paste0("y_", y)]] <- as.integer(data$year_val == (2008 + y))

#### Helper: build full-rank xreg matrix by dropping collinear columns via QR ####
make_xreg_fullrank <- function(xmat) {
  qr_x <- qr(xmat)
  r <- qr_x$rank
  if (r < ncol(xmat)) {
    keep <- qr_x$pivot[1:r]
    xmat <- xmat[, keep, drop = FALSE]
  }
  xmat
}

#### Helper: fit ARIMA(p,0,0) with xreg ####
fit_ar <- function(y, xreg, p, method = "CSS-ML", use_mean = TRUE) {
  tryCatch({
    fit <- arima(y, order = c(p, 0, 0), xreg = xreg, method = method,
                 include.mean = use_mean,
                 optim.control = list(maxit = 5000))
    uber_coef <- coef(fit)["uber"]
    uber_se   <- sqrt(fit$var.coef["uber", "uber"])
    bic_val   <- BIC(fit)
    list(coef = uber_coef, se = uber_se, bic = bic_val, n = length(y), fit = fit)
  }, error = function(e) {
    fit <- arima(y, order = c(p, 0, 0), xreg = xreg, method = "CSS",
                 include.mean = use_mean,
                 optim.control = list(maxit = 5000))
    uber_coef <- coef(fit)["uber"]
    uber_se   <- sqrt(fit$var.coef["uber", "uber"])
    n <- length(y)
    k <- length(coef(fit))
    ss <- fit$sigma2 * n
    bic_val <- n * log(ss/n) + k * log(n)
    list(coef = uber_coef, se = uber_se, bic = bic_val, n = n, fit = fit)
  })
}

#### Table I.15: AR(1) with 3 specifications ####

## Spec (1): AR(1) + uber only
xreg1 <- cbind(uber = data$uber)
r1 <- fit_ar(data$ln_trip, xreg1, 1)

## Spec (2): AR(1) + uber + weather
xreg2 <- cbind(uber = data$uber, ln_prcp = data$ln_prcp,
               ln_tmax = data$ln_tmax, ln_tmin = data$ln_tmin)
r2 <- fit_ar(data$ln_trip, xreg2, 1)

## Spec (3): AR(1) + uber + weather + FEs
## Include intercept in QR to handle collinearity between constant, month
## dummies, year dummies, and uber. Then use include.mean=FALSE in arima().
month_cols <- paste0("m_", 1:12)
year_cols  <- paste0("y_", 1:9)
xreg3_full <- cbind(
  intercept = 1,
  as.matrix(data[, c("uber", "ln_prcp", "ln_tmax", "ln_tmin",
                      month_cols, year_cols, "weekdays")])
)
xreg3 <- make_xreg_fullrank(xreg3_full)
r3 <- fit_ar(data$ln_trip, xreg3, 1, use_mean = FALSE)

#### Table I.16: AR(2) through AR(5), full specification ####
results_i16 <- list()
for (p in 2:5) {
  results_i16[[p-1]] <- fit_ar(data$ln_trip, xreg3, p, use_mean = FALSE)
}

#### Save the results ####
sum_i15 <- data.frame(
  spec = c("(1)", "(2)", "(3)"),
  coef = c(r1$coef, r2$coef, r3$coef),
  se   = c(r1$se, r2$se, r3$se),
  bic  = c(r1$bic, r2$bic, r3$bic),
  n    = c(r1$n, r2$n, r3$n)
)

sum_i16 <- data.frame(
  spec = paste0("AR-", 2:5),
  coef = sapply(results_i16, function(x) x$coef),
  se   = sapply(results_i16, function(x) x$se),
  bic  = sapply(results_i16, function(x) x$bic),
  n    = sapply(results_i16, function(x) x$n)
)

write_rds(list(i15 = sum_i15, i16 = sum_i16), file = "03_gen/09_revision/nyc_taxi_ar.rds")

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()
