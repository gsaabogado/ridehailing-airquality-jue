#### ##################################################################### ####
####                       Two Ways Fixed Effects                          ####
#### Part of the replication package                                       ####
#### ##################################################################### ####
#### Set the working directory to the replication folder ####
## Path set by 00_master.R; if running standalone, set root manually
if (!exists("root")) root <- getwd()
setwd(root)

#### _____________________________________________________________________ ####
#### Table 3 - column 1: Effects of Uber on the air quality index (TWFE) ####
#### Table H2: Robustness checks - Effects of Uber on the air quality index (TWFE) ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

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

#### Run the regression across all dependent variables and models ####
est =  lapply(split(data, f = data$var), function(x)

  list(`(1)` = feols(value ~ treatment | year + fips,
                     data = x, cluster = "cbsa_id"),


       `(2)` = feols(value ~ treatment | year + fips,
                     data = x %>% filter(score %in% seq(-5, 0, 1) | treated == 0 ),
                     cluster = "cbsa_id"),

       `(3)` = feols(value ~ treatment +
                       st_tmp + st_rain | year + fips,
                     data = x %>% filter(score %in% seq(-5, 0, 1)| treated == 0),
                     cluster = "cbsa_id"),

       `(4)` = feols(value ~ treatment +
                       st_tmp + st_tmp^2 + dew + st_rain + st_rh + st_wsp +
                       u_wind + v_wind + atm | year + fips,
                     data = x %>% filter(score %in% seq(-5, 0, 1)| treated == 0),
                     cluster = "cbsa_id"),

       `(5)` = feols(value ~ treatment +
                       st_tmp + st_rain +
                       white + incomepercapita  + const_exp |
                       year + fips  + forest_fire + new_pp + ret_pp,
                     data = x %>% filter(score %in% seq(-5, 0, 1)| treated == 0),
                     cluster = "cbsa_id")))


#### Extract the relevant coefficients ####
sum = lapply(est, function(x) lapply(x, function(x)
  x = data.frame(tidy(x), N.Obs = x$nobs,
                 R2 = r2(x)[2],
                 BIC = BIC(x),
                 N.Counties = x$fixef_sizes[2],
                 N.Periods = x$fixef_sizes[1]))) %>%

  lapply(., rbindlist, idcol = "spec") %>%

  rbindlist(., idcol = "var") %>%

  filter(term == "treatment")


#### Add the pre-treatment average ####
sum = list(data %>% filter(treatment == 0 & treated == 1) %>%
             group_by(var) %>% filter(score %in% seq(-5, 0, 1)) %>%
             summarise(`pt_value` = mean(value))  %>%
             crossing(tibble(spec = paste0("(", c(2:5), ")"))),

           data %>% filter(treatment == 0 & treated == 1) %>%
             group_by(var) %>% summarise(`pt_value` = mean(value)) %>%
             mutate(spec = "(1)")) %>% rbindlist(.) %>%
  left_join(sum, .)


#### Organize the data set #####
sum = select(sum, c(var, spec, estimate:pt_value))

#### Check the final data set ####
sum %>% head(.)

#### Save the summary values ####
write_rds(sum, file = "03_gen/05_results/twfe.rds")

#### Clear the space ####
rm(list = ls()); gc()

#### _____________________________________________________________________ ####
#### Table 3 - column 2: Effects of Uber on the air quality index (ETWFE) ####
#### Table H3: Robustness checks - Effects of Uber on the air quality index (ETWFE) ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

#### Load the packages ####
library(conflicted)
library(data.table)
library(tidyverse)
library(etwfe)
library(broom)

#### Load the regressions data ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")

#### Transform from wide to long format ####
data = gather(data, var, value, c(avg:max))
#data = filter(data, var == "avg")

#### Run the regression across all dependent variables and models ####
est = lapply(split(data, f = data$var), function(x)

  list(

    `(1)`= etwfe(fml  = value ~ 1, tvar = year,  gvar = enter_year,
                 data = x, vcov = ~cbsa_id, fe = "vs"),

    `(2)` = etwfe(fml  = value ~ 1,  tvar = year,  gvar = enter_year,
                  data = x |> filter(score %in% seq(-5,0,1)| treated == 0),
                  vcov = ~cbsa_id, fe = "vs"),

    `(3)` = etwfe(fml  = value ~ st_tmp + st_rain, tvar = year,  gvar = enter_year,
                  data = x |> filter(score %in% seq(-5,0,1)| treated == 0),
                  vcov = ~cbsa_id, fe = "vs"),

    `(4)`= etwfe(fml = value ~ st_tmp +  st_rain  +
                   forest_fire + new_pp + ret_pp + white +
                   incomepercapita + const_exp, tvar = year,  gvar = enter_year,
                 data = x |> filter(score %in% seq(-5,0,1) | treated == 0),
                 vcov = ~cbsa_id, fe = "vs")))

#### Aggregate the point estimates to ATT ####
agg = lapply(est, function(x) lapply(x, emfx)); agg$avg

#### Extract the relevant coefficients ####
sum = lapply(agg, function(x) lapply(x, function(x)
  data.frame(tidy(x)))) %>%
  lapply(rbindlist, idcol = "spec") %>%
  rbindlist(., idcol = "var") |>
  select(c(var, spec, estimate:`p.value`))

#### Include additional statistics ####
sum = lapply(est, function(x) lapply(x, function(x)
  data.frame(R2 = r2(x)[2], BIC = BIC(x), N.Obs = nobs(x),
             N.Cohorts = x$fixef_sizes[1],
             N.Periods = x$fixef_sizes[2]))) %>%
  lapply(rbindlist, idcol = "spec") %>%
  rbindlist(., idcol = "var") %>% left_join(sum, .)

#### Include the number of observations ####
sum$N.Counties = length(unique(data$fips))

#### Add the pre-treatment average ####
sum = list(data %>% filter(treatment == 0 & treated == 1) %>%
             group_by(var) %>% filter(score %in% seq(-5, 0, 1)) %>%
             summarise(`pt_value` = mean(value))  %>%
             crossing(tibble(spec = paste0("(", c(2:4), ")"))),

           data %>% filter(treatment == 0 & treated == 1) %>%
             group_by(var) %>% summarise(`pt_value` = mean(value)) %>%
             mutate(spec = "(1)")) %>% rbindlist(.) %>%
  left_join(sum, .)


#### Check the final data set ####
sum %>% head(.)

#### Save the summary values ####
write_rds(sum, file = "03_gen/05_results/etwfe.rds")

#### Clear the space ####
rm(list = ls()); gc()

#### _____________________________________________________________________ ####
#### Table 3 - column 3: Effects of Uber on the air quality index (SADD) ####
#### Table H3: Robustness checks - Effects of Uber on the air quality index (SADD) ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

#### Load the packages ####
library(tidyverse)
library(synthdid)
library(data.table)
library(fixest)
library(etwfe)
library(broom)

#### Load the regressions data ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")

#### Transform from wide to long format ####
data = gather(data, var, value, c(avg:max))

#### Estimate the effects with SUNAB ####
est =  lapply(split(data, f = data$var), function(x)

  list(`(1)` = feols(value ~ sunab(enter_year, year) | year + fips,
                     data = x, cluster = "cbsa_id") %>%
         summary(agg = "att"),


       `(2)` = feols(value ~ sunab(enter_year, year) | year + fips,
                     data = x %>% filter(score %in% seq(-5, 0, 1) | treated == 0 ),
                     cluster = "cbsa_id") %>% summary(agg = "att"),

       `(3)` = feols(value ~ sunab(enter_year, year) +
                       st_tmp + st_rain  | year + fips,
                     data = x %>% filter(score %in% seq(-5, 0, 1) | treated == 0),
                     cluster = "cbsa_id") %>% summary(agg = "att"),


       `(4)` = feols(value ~ sunab(enter_year, year) +
                       st_tmp + st_rain +  white + incomepercapita  + const_exp |
                       year + fips + forest_fire + new_pp + ret_pp,
                     data = x %>% filter(score %in% seq(-5, 0, 1) | treated == 0),
                     cluster = "cbsa_id") %>% summary(agg = "att")))

#### Extract the relevant coefficients ####
sum = lapply(est, function(x) lapply(x, function(x)
  data.frame(tidy(x)) |> filter(term == "ATT"))) %>%
  lapply(rbindlist, idcol = "spec") %>%
  rbindlist(., idcol = "var") |>
  select(c(var, spec, estimate:`p.value`))

#### Include additional statistics ####
sum = lapply(est, function(x) lapply(x, function(x)
  data.frame(R2 = r2(x)[2], BIC = BIC(x), N.Obs = nobs(x)))) %>%
  lapply(rbindlist, idcol = "spec") %>%
  rbindlist(., idcol = "var") %>% left_join(sum, .)

#### Include the number of observations ####
sum$N.Cohorts = length(unique(data$enter_year))
sum$N.Counties = length(unique(data$fips))
sum$N.Periods = length(unique(data$year))

#### Add the pre-treatment average ####
sum = list(data %>% filter(treatment == 0 & treated == 1) %>%
             group_by(var) %>% filter(score %in% seq(-5, 0, 1)) %>%
             summarise(`pt_value` = mean(value))  %>%
             crossing(tibble(spec = paste0("(", c(2:4), ")"))),

           data %>% filter(treatment == 0 & treated == 1) %>%
             group_by(var) %>% summarise(`pt_value` = mean(value)) %>%
             mutate(spec = "(1)")) %>% rbindlist(.) %>%
  left_join(sum, .)


#### Check the final data set ####
sum %>% head(.)

#### Save the summary values ####
write_rds(sum, file = "03_gen/05_results/sadd.rds")

#### Clear the space ####
rm(list = ls()); gc()

#### _____________________________________________________________________ ####
#### Table 3 - column 4: Synthetic TWFE-DD model ####
#### Table H4: Robustness checks - Synthetic TWFE-DD model ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

#### Load the packages ####
library(conflicted)
library(data.table)
library(tidyverse)
library(synthdid)
library(fixest)
library(broom)

#### Set the working directory ####
setwd(root)

#### Load the regressions data ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")

#### Transform from wide to long format ####
data = gather(data, var, value, c(avg:max))

#### Determine the enter years ####
tg = sort(unique(filter(data, enter_year < 2017)$enter_year))

#### Split the data into a list of data frames for each treatment group ####
data = lapply(tg, function(x)
  filter(data, enter_year >= x) %>%
    mutate(tg = x, score = year - tg) %>%
    filter(score %in% seq(-5, 0, 1)) %>%
    mutate(treated = ifelse(enter_year == tg, 1, 0)) %>%
    mutate(treatment = ifelse(year >= enter_year & treated == 1, 1, 0)))

#### Transform the data frames into a balanced panel ####
data = lapply(data, function(x)
  x %>% group_by(fips, var) %>% mutate(count = n()) %>%
    filter(count == 6) %>% as.data.frame(.))

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
  synthdid_estimate(x$Y, x$N0, x$T0)))

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

#### Estimate the effects with the weights from the SDiD model ####
est = lapply(split(data, f = data$var), function(x)

  list(`(1)` = feols(value ~ treatment | year + fips,
                     data = x, weights = ~weights,
                     notes = F),

       `(2)` = feols(value ~ treatment + st_tmp + st_rain| year + fips,
                     data = x, weights = ~weights, notes = F),

       `(3)` = feols(value ~ treatment + st_tmp + st_rain +
                       incomepercapita + white + const_exp |
                       year + fips  + forest_fire,
                     data = x, weights = ~weights, notes = F)))

#### Construct the function to estimate the Jacknife SEs ####
jack_se = function(x, y){

  new = filter(x, cbsa_id != y)

  lapply(split(new, f = new$var), function(x)

    list(`(1)` = feols(value ~ treatment | year + fips, notes = F,
                       data = x, weights = ~weights),

         `(2)` = feols(value ~ treatment + st_tmp + st_rain| year + fips, notes = F,
                       data = x, weights = ~weights),

         `(3)` = feols(value ~ treatment + st_tmp + st_rain + incomepercapita + white +
                         const_exp | year + fips  + forest_fire, notes = F, data = x,  weights = ~weights)) %>%

      lapply(., function(x) x = tidy(x) %>% filter(term == "treatment") %>%
               select(estimate))) %>%

    lapply(., rbindlist, idcol = "spec") %>%

    rbindlist(., idcol = "var")}

#### Run the 100 bootstraps ####
jacknife = lapply(unique(filter(data, weights > 0)$cbsa_id), function(x)
  jack_se(data, x))

#### Extract the relevant coefficients ####
sum = lapply(est, function(x) lapply(x, function(x)
  x = data.frame(tidy(x), N.Obs = x$nobs, R2 = r2(x)[2],
                 BIC = BIC(x),
                 N.Counties = x$fixef_sizes[2],
                 N.Periods = x$fixef_sizes[1]))) %>%

  lapply(., rbindlist, idcol = "spec") %>%
  rbindlist(., idcol = "var") %>%

  filter(term == "treatment") %>%

  select(c(var:estimate, N.Obs:N.Periods))


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
sum = select(sum, c(var, spec, estimate, std.error = jse, p.value = jp,
                    N.Obs:N.Periods))

#### Add the pre-treatment average ####
sum = list(data %>% filter(treatment == 0 & treated == 1) %>%
             group_by(var) %>% filter(score %in% seq(-5, 0, 1)) %>%
             summarise(`pt_value` = mean(value))  %>%
             crossing(tibble(spec = paste0("(", c(1:3), ")")))) %>%
  rbindlist(.) %>%
  left_join(sum, .)

#### Check the final data set ####
sum$N.Cohorts = length(unique(data$enter_year))
sum %>% head(.)

#### Save the summary values ####
write_rds(sum, file = "03_gen/05_results/sdid.rds")

#### Clear the space ####
rm(list = ls()); gc()

#### _____________________________________________________________________ ####
#### Table 3 - column 5: Synthetic ETWFE-DD model ####
#### Table H4: Robustness checks - Synthetic ETWFE-DD model ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

#### Set the working directory ####
setwd(root)

#### Load the packages ####
library(conflicted)
library(data.table)
library(tidyverse)
library(parallel)
library(synthdid)
library(fixest)
library(broom)
library(etwfe)

#### Load the regressions data ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")

#### Transform from wide to long format ####
data = gather(data, var, value, c(avg:max))

#### Determine the enter years ####
tg = sort(unique(filter(data, enter_year < 2017)$enter_year))

#### Split the data into a list of data frames for each treatment group ####
data = lapply(tg, function(x)
  filter(data, enter_year >= x) %>%
    mutate(tg = x, score = year - tg) %>%
    filter(score %in% seq(-5, 0, 1)) %>%
    mutate(treated = ifelse(enter_year == tg, 1, 0)) %>%
    mutate(treatment = ifelse(year >= enter_year & treated == 1, 1, 0)))

#### Transform the data frames into a balanced panel ####
data = lapply(data, function(x)
  x %>% group_by(fips, var) %>% mutate(count = n()) %>%
    filter(count == 6) %>% as.data.frame(.))

#### Add names to the list elements ####
names(data) = tg

#### Split the data sets by dependent variable ###
data = lapply(data, function(x) split(x, f = x$var))

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

#### Add an indicator variable for the county by treatment group indicator ####
data = mutate(data, fips_tg = paste(fips, tg))
#data = filter(data, var == "avg")

#### Estimate the effects with the weights from the SDiD model ####
est = lapply(split(data, f = data$var), function(x)

  list(

    `(1)`= etwfe(fml  = value ~ 1, tvar = year,  gvar = enter_year,
                 data = x, weights = x$weights, fe = "vs"),

    `(2)` = etwfe(fml  = value ~ st_tmp + st_rain,
                  tvar = year,  gvar = enter_year,
                  data = x, weights = x$weights, fe = "vs"),

    `(3)`= etwfe(fml  = value ~ st_tmp + st_rain + forest_fire +
                   incomepercapita + white + const_exp,
                 tvar = year, gvar = enter_year,
                 data = x, weights = x$weights, fe = "vs")))

#### Aggregate point estimates ####
agg = lapply(est, function(x) lapply(x, emfx, vcov = FALSE))

#### Construct the function to estimate the Jacknife SEs ####
jack_se = function(x, y){

  new = filter(x, cbsa_id != y)

  # run second stage
  lapply(split(new, f = new$var), function(x)

    list(

      `(1)`= etwfe(fml  = value ~ 1, tvar = year,  gvar = enter_year,
                   data = x, weights = x$weights, fe = "vs"),

      `(2)` = etwfe(fml  = value ~ st_tmp + st_rain,
                    tvar = year,  gvar = enter_year,
                    data = x, weights = x$weights, fe = "vs"),

      `(3)`= etwfe(fml  = value ~ st_tmp + st_rain +
                     incomepercapita + white + const_exp +
                     forest_fire,
                   tvar = year, gvar = enter_year, data = x,
                   weights = x$weights, fe = "vs")) %>%

      lapply(., emfx, vcov = FALSE)) %>%

    lapply(., function(x) lapply(x, function(x)
      x = tidy(x) %>% select(estimate))) %>%

    lapply(., rbindlist, idcol = "spec") %>%
    rbindlist(idcol = "var")
}

#### Run the jacknife estimates ####
jacknife = mclapply(unique(filter(data, weights > 0)$cbsa_id),
                    function(x) jack_se(data, x), mc.cores = 20)

#### Extract the relevant coefficients ####
sum = lapply(agg, function(x) lapply(x, function(x)
  data.frame(tidy(x)))) %>%
  lapply(rbindlist, idcol = "spec") %>%
  rbindlist(., idcol = "var") |>
  select(c(var, spec, estimate))

#### Determine the standard error of the placebo estimates ####
jse = rbindlist(jacknife, idcol = "btr") %>%

  left_join(., select(sum, var, spec, raw_est = estimate))

#### Aggregate the Standard Error ####
jse = ungroup(jse) %>% group_by(var, spec) %>%
  mutate(diff = (estimate - raw_est)^2, df = (n()-1)/n()) %>%
  group_by(var, spec) %>% summarise(jse = sum(diff)*unique(df)) %>%
  mutate(jse = sqrt(jse))

#### Add the placebo SE to the data set ####
sum = left_join(sum, jse)  %>%
  mutate(jp = 2*pnorm(abs(estimate/jse), lower.tail = F))

#### Include additional statistics####
sum = lapply(est, function(x) lapply(x, function(x)
  data.frame(R2 = r2(x)[2], BIC = BIC(x), N.Obs = nobs(x)))) %>%
  lapply(rbindlist, idcol = "spec") %>%
  rbindlist(., idcol = "var") %>% left_join(sum, .)


#### Include the number of observations ####
sum$N.Counties = length(unique(data$fips))
sum$N.Periods = length(unique(data$year))
sum$N.Cohorts = length(unique(data$enter_year))

#### Add the pre-treatment average ####
sum = list(data %>% filter(treatment == 0 & treated == 1) %>%
             group_by(var) %>% filter(score %in% seq(-5, 0, 1)) %>%
             summarise(`pt_value` = mean(value))  %>%
             crossing(tibble(spec = paste0("(", c(1:3), ")")))) %>%
  rbindlist(.) %>%
  left_join(sum, .)

#### Re-organize the data ####
sum = select(sum, c(var, spec, estimate, std.error = jse, p.value = jp,  R2:pt_value))
x = filter(sum, var == "avg"); x

#### Check the final data set ####
sum %>% head(.)

#### Save the summary values ####
write_rds(sum, file = "03_gen/05_results/sdid_etwfe.rds")

#### Clear the space ####
rm(list = ls()); gc()

#### _____________________________________________________________________ ####
#### Table 3 - column 6: Synthetic SADD model ####
#### Table H4: Robustness checks - Synthetic SADD model ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

#### Set the working directory ####
setwd(root)

#### Load the packages ####
library(conflicted)
library(data.table)
library(tidyverse)
library(parallel)
library(synthdid)
library(fixest)
library(broom)

#### Load the regressions data ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")

#### Transform from wide to long format ####
data = gather(data, var, value, c(avg:max))

#### Determine the enter years ####
tg = sort(unique(filter(data, enter_year < 2017)$enter_year))

#### Split the data into a list of data frames for each treatment group ####
data = lapply(tg, function(x)
  filter(data, enter_year >= x) %>%
    mutate(tg = x, score = year - tg) %>%
    filter(score %in% seq(-5, 0, 1)) %>%
    mutate(treated = ifelse(enter_year == tg, 1, 0)) %>%
    mutate(treatment = ifelse(year >= enter_year & treated == 1, 1, 0)))

#### Transform the data frames into a balanced panel ####
data = lapply(data, function(x)
  x %>% group_by(fips, var) %>% mutate(count = n()) %>%
    filter(count == 6) %>% as.data.frame(.))

#### Add names to the list elements ####
names(data) = tg

#### Split the data sets by dependent variable ###
data = lapply(data, function(x) split(x, f = x$var))

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

#### Add an indicator variable for the county by treatment group indicator ####
data = mutate(data, fips_tg = paste(fips, tg))
#data = filter(data, var == "avg")

#### Estimate the effects with the weights from the SDiD model ####
est = lapply(split(data, f = data$var), function(x)

  list(

    `(1)`= feols(value ~ sunab(enter_year, year) |fips + year,
                 data = x, notes = F,  weights = x$weights) %>% summary(agg = "att"),

    `(2)` =  feols(value ~ sunab(enter_year, year) + st_tmp + st_rain |
                     fips + year, data = x, notes = F,
                   weights = x$weights) %>% summary(agg = "att"),

    `(3)` =  feols(value ~ sunab(enter_year, year) + st_rain + st_tmp +
                     incomepercapita + white + const_exp + forest_fire|
                     fips + year, data = x, notes = F,
                   weights = x$weights) %>% summary(agg = "att")))

#### Construct the function to estimate the Jacknife SEs ####
jack_se = function(x, y){

  new = filter(x, cbsa_id != y)

  # run second stage
  lapply(split(new, f = new$var), function(x)

    list(

      `(1)`= feols(value ~ sunab(enter_year, year) |fips + year,
                   data = x, notes = F,  weights = x$weights) %>% summary(agg = "att"),

      `(2)` =  feols(value ~ sunab(enter_year, year) + st_tmp + st_rain |
                       fips + year, data = x, notes = F,
                     weights = x$weights) %>% summary(agg = "att"),

      `(3)` =  feols(value ~ sunab(enter_year, year) + st_rain + st_tmp +
                       incomepercapita + white + const_exp + forest_fire|
                       fips + year, data = x, notes = F,
                     weights = x$weights) %>% summary(agg = "att"))) %>%

    lapply(., function(x) lapply(x, function(x)
      x = tidy(x) |> filter(term == "ATT") %>% select(estimate, term))) %>%

    lapply(., rbindlist, idcol = "spec") %>%
    rbindlist(idcol = "var")
}

#### Run the jacknife estimates ####
jacknife = mclapply(unique(filter(data, weights > 0)$cbsa_id),
                    function(x) jack_se(data, x), mc.cores = 20)

#### Extract the relevant coefficients ####
sum = lapply(est, function(x) lapply(x, function(x)
  data.frame(tidy(x)) |> filter(term == "ATT"))) %>%
  lapply(rbindlist, idcol = "spec") %>%
  rbindlist(., idcol = "var") |>
  select(c(var, spec, estimate))

#### Determine the standard error of the placebo estimates ####
jse = rbindlist(jacknife, idcol = "btr") %>%

  left_join(., select(sum, var, spec, raw_est = estimate))

#### Aggregate the Standard Error ####
jse = ungroup(jse) %>% group_by(var, spec) %>%
  mutate(diff = (estimate - raw_est)^2, df = (n()-1)/n()) %>%
  group_by(var, spec) %>% summarise(jse = sum(diff)*unique(df)) %>%
  mutate(jse = sqrt(jse))

#### Add the placebo SE to the data set ####
sum = left_join(sum, jse)  %>%
  mutate(jp = 2*pnorm(abs(estimate/jse), lower.tail = F))

#### Include additional statistics####
sum = lapply(est, function(x) lapply(x, function(x)
  data.frame(R2 = r2(x)[2], BIC = BIC(x), N.Obs = nobs(x)))) %>%
  lapply(rbindlist, idcol = "spec") %>%
  rbindlist(., idcol = "var") %>% left_join(sum, .)

#### Include the number of observations ####
sum$N.Counties = length(unique(data$fips))
sum$N.Periods = length(unique(data$year))
sum$N.Cohorts = length(unique(data$enter_year))

#### Add the pre-treatment average ####
sum = list(data %>% filter(treatment == 0 & treated == 1) %>%
             group_by(var) %>% filter(score %in% seq(-5, 0, 1)) %>%
             summarise(`pt_value` = mean(value))  %>%
             crossing(tibble(spec = paste0("(", c(1:3), ")")))) %>%
  rbindlist(.) %>%
  left_join(sum, .)

#### Re-organize the data ####
sum = select(sum, c(var, spec, estimate, std.error = jse, p.value = jp,  R2:pt_value))
x = filter(sum, var == "avg"); x

#### Check the final data set ####
sum %>% head(.)

#### Save the summary values ####
write_rds(sum, file = "03_gen/05_results/sdid_sadd.rds")

#### Clear the space ####
rm(list = ls()); gc()

#### _____________________________________________________________________ ####
#### Figure 3: Dynamic estimates for the effect of Uber on the AQI (TWFEs) ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

#### Load the packages ####
library(conflicted)
library(data.table)
library(tidyverse)
library(synthdid)
library(fixest)
library(broom)

#### Load the regressions data ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")

#### Transform from wide to long format ####
data = gather(data, var, value, c(avg:max))

#### Determine the enter years ####
tg = sort(unique(filter(data, enter_year < 2017)$enter_year))

#### Split the data into a list of data frames for each treatment group ####
data = lapply(tg, function(x)
  filter(data, enter_year >= x) %>%
    mutate(tg = x, score = year - tg) %>%
    filter(score %in% seq(-5, 0, 1)) %>%
    mutate(treated = ifelse(enter_year == tg, 1, 0)) %>%
    mutate(treatment = ifelse(year >= enter_year & treated == 1, 1, 0)))

#### Transform the data frames into a balanced panel ####
data = lapply(data, function(x)
  x %>% group_by(fips, var) %>% mutate(count = n()) %>%
    filter(count == 6) %>% as.data.frame(.))

#### Add names to the list elements ####
names(data) = tg

#### Only keep the effect on the average ####
data = lapply(data, function(x) filter(x, var == "avg"))

#### Split the data sets by dependent variable ###
data = lapply(data, function(x)
  split(x, f = x$var))

#### Construct the panel object for the SDiD algorithm ####
panel = lapply(data, function(x) lapply(x, function(x)
  panel.matrices(x, unit = "fips", time = "score",
                 outcome = "value", treatment = "treatment")))

#### Estimate synth DD ####
synth = lapply(panel, function(x) lapply(x, function(x)
  synthdid_estimate(x$Y, x$N0, x$T0)))

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

#### Estimate the effects with the weights from the SDiD model ####
est = feols(value ~ i(score, treated, ref = -1) +
              st_tmp + st_rain | year + fips,
            data = data,
            weights = ~weights, notes = F)

#### Construct the function to estimate the Jacknife SEs ####
jack_se = function(x, y){

  new = filter(x, cbsa_id != y)

  feols(value ~ i(score, treated, ref = -1) +
          st_tmp + st_rain | year + fips,
        data = new,
        weights = ~weights, notes = F) %>%

    tidy(.) %>% filter(grepl("treated", term)) %>%
    select(term, estimate)}

#### Run the 100 bootstraps ####
jacknife = mclapply(unique(filter(data, weights > 0)$cbsa_id), function(x)
  jack_se(data, x), mc.cores = 40)

#### Extract the relevant coefficients ####
sum = tidy(est) %>%
  filter(grepl("treated", term))

#### Determine the standard error of the placebo estimates ####
jse = rbindlist(jacknife, idcol = "btr") %>%

  left_join(., select(sum, term, raw_est = estimate))

#### Aggregate the Standard Error ####
jse = ungroup(jse) %>% group_by(term) %>%
  mutate(diff = (estimate - raw_est)^2, df = (n()-1)/n()) %>%
  group_by(term) %>% summarise(jse = sum(diff)*unique(df)) %>%
  mutate(jse = sqrt(jse))

#### Add the placebo SE to the data set ####
sum = left_join(sum, jse)  %>%
  mutate(jp = 2*pnorm(abs(estimate/jse), lower.tail = F))

#### Re-organize the data set ####
sum = select(sum, c(term, estimate, std.error = jse, p.value = jp))

#### Only keep the raw score ####
sum = mutate(sum, score = gsub(".*::", "", term)) %>%
  mutate(score = gsub(":.*", "",score)) %>% select(-term);sum

#### Check the final data set ####
sum %>% head(.)

#### Save the summary values ####
write_rds(sum, file = "03_gen/05_results/sdid_twfe_dynamic.rds")

#### Clear the space ####
rm(list = ls()); gc()
#### _____________________________________________________________________ ####
#### Figure 3: Dynamic estimates for the effect of Uber on the AQI (ETWFEs) ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

#### Set the working directory ####
setwd(root)

#### Load the packages ####
library(conflicted)
library(tidyverse)
library(parallel)
library(synthdid)
library(fixest)
library(broom)
library(etwfe)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the regressions data ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")

#### Transform from wide to long format ####
data = gather(data, var, value, c(avg:max))
data = filter(data, var == "avg")

#### Determine the enter years ####
tg = sort(unique(filter(data, enter_year < 2017)$enter_year))

#### Split the data into a list of data frames for each treatment group ####
data = lapply(tg, function(x)
  filter(data, enter_year >= x) %>%
    mutate(tg = x, score = year - tg) %>%
    filter(score %in% seq(-5, 0, 1)) %>%
    mutate(treated = ifelse(enter_year == tg, 1, 0)) %>%
    mutate(treatment = ifelse(year >= enter_year & treated == 1, 1, 0)))

#### Transform the data frames into a balanced panel ####
data = lapply(data, function(x)
  x %>% group_by(fips, var) %>% mutate(count = n()) %>%
    filter(count == 6) %>% as.data.frame(.))

#### Add names to the list elements ####
names(data) = tg

#### Split the data sets by dependent variable ###
data = lapply(data, function(x) split(x, f = x$var))

#### Construct the panel object for the SDiD algorithm ####
panel = lapply(data, function(x) lapply(x, function(x)
  panel.matrices(x, unit = "fips", time = "score",
                 outcome = "value", treatment = "treatment")))

#### Estimate synth DD ####
synth = lapply(panel, function(x) lapply(x, function(x)
  synthdid::synthdid_estimate(x$Y, x$N0, x$T0)))

#### Extract the unit weights ####
weights = list(
  uw = lapply(synth, function(x) lapply(x, function(x)
    data.frame(synthdid_controls(x, weight.type='omega', mass = 1)) %>%
      mutate(fips = as.character(rownames(.))) %>% rename(uw = estimate.1))) %>%
    lapply(., function(x) bind_rows(x, .id = "var")) %>%
    bind_rows(.id = "tg"),

  tw = lapply(synth, function(x) lapply(x, function(x)
    data.frame(synthdid_controls(x, weight.type='lambda', mass = 1)) %>%
      mutate(score = as.numeric(rownames(.))) %>% rename(tw = estimate.1))) %>%
    lapply(., function(x) bind_rows(x, .id = "var")) %>%
    bind_rows(.id = "tg")
)

#### Add the weights to the data sets ####
data = lapply(data, bind_rows) %>%
  bind_rows() %>%
  mutate(tg = as.character(tg)) %>%
  left_join(., weights$uw) %>%
  left_join(., weights$tw) %>%
  mutate(uw = ifelse(treated > 0, 1, uw),
         tw = ifelse(score >= 0, 1, tw)) %>%
  mutate(weights = uw*tw)

#### Add an indicator variable for the county by treatment group indicator ####
data = mutate(data, fips_tg = paste(fips, tg))

#### Estimate the effects with the weights from the SDiD model ####
#### Run the regression across all dependent variables and models ####
est = etwfe(fml = value ~ st_tmp + st_rain, tvar = year,
            gvar = enter_year,  data = data,
            vcov = ~cbsa_id, weights = data$weights)

#### Aggregate the point estimates to ATT ####
agg = emfx(est, type = "event", post_only = F)

#### Construct the function to estimate the Jacknife SEs ####
jack_se = function(x, y){

  new = filter(x, cbsa_id != y)

  etwfe(fml = value ~ st_tmp + st_rain, tvar = year,
        gvar = enter_year,  data = new,
        vcov = ~cbsa_id, weights = new$weights) %>%

    emfx(., type = "event", post_only = F) %>%

    tidy(.) %>% select(score = event, estimate, std.error) %>%
    filter(score > -6)
}

#### Run the jacknife estimates ####
jacknife = mclapply(unique(filter(data, weights > 0)$cbsa_id),
                    function(x) jack_se(data, x), mc.cores = 10)

#### Extract the relevant coefficients ####
sum = data.frame(tidy(agg)) %>%
  select(c(score = event, estimate))

#### Determine the standard error of the placebo estimates ####
jse = bind_rows(jacknife, .id = "btr") %>%
  left_join(., select(sum, score, raw_est = estimate))

#### Aggregate the Standard Error ####
jse = ungroup(jse) %>% group_by(score) %>%
  mutate(diff = (estimate - raw_est)^2, df = (n()-1)/n()) %>%
  group_by(score) %>% summarise(jse = sum(diff)*unique(df)) %>%
  mutate(jse = sqrt(jse))

#### Add the placebo SE to the data set ####
sum = left_join(sum, jse)  %>%
  mutate(jp = 2*pnorm(abs(estimate/jse), lower.tail = F))

#### Re-organize the data set ####
sum = select(sum, c(score, estimate, std.error = jse, p.value = jp))

#### Re-organize the data ####
sum = filter(sum, score > -6); sum

#### Save the summary values ####
write_rds(sum, file = "03_gen/05_results/sdid_etwfe_dynamic.rds")

#### Clear the space ####
rm(list = ls()); gc()

#### _____________________________________________________________________ ####
#### Figure 3: Dynamic estimates for the effect of Uber on the AQI (SADD) ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

#### Load the packages ####
library(conflicted)
library(data.table)
library(tidyverse)
library(synthdid)
library(fixest)
library(broom)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the regressions data ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")

#### Transform from wide to long format ####
data = gather(data, var, value, c(avg:max))

#### Determine the enter years ####
tg = sort(unique(filter(data, enter_year < 2017)$enter_year))

#### Split the data into a list of data frames for each treatment group ####
data = lapply(tg, function(x)
  filter(data, enter_year >= x) %>%
    mutate(tg = x, score = year - tg) %>%
    filter(score %in% seq(-5, 0, 1)) %>%
    mutate(treated = ifelse(enter_year == tg, 1, 0)) %>%
    mutate(treatment = ifelse(year >= enter_year & treated == 1, 1, 0)))

#### Transform the data frames into a balanced panel ####
data = lapply(data, function(x)
  x %>% group_by(fips, var) %>% mutate(count = n()) %>%
    filter(count == 6) %>% as.data.frame(.))

#### Add names to the list elements ####
names(data) = tg

#### Only keep the effect on the average ####
data = lapply(data, function(x) filter(x, var == "avg"))

#### Split the data sets by dependent variable ###
data = lapply(data, function(x)
  split(x, f = x$var))

#### Construct the panel object for the SDiD algorithm ####
panel = lapply(data, function(x) lapply(x, function(x)
  panel.matrices(x, unit = "fips", time = "score",
                 outcome = "value", treatment = "treatment")))

#### Estimate synth DD ####
synth = lapply(panel, function(x) lapply(x, function(x)
  synthdid_estimate(x$Y, x$N0, x$T0)))

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

#### Estimate the effects with the weights from the SDiD model ####
est = feols(value ~ sunab(enter_year, year,
                          ref.c = c("2020", "2019", "2018", "2017")) +
              st_rain:tg + st_tmp:tg | tg^fips+ year, data = data,
            cluster = "cbsa_id", weights = ~weights)

#### Construct the function to estimate the Jacknife SEs ####
jack_se = function(x, y){

  new = filter(x, cbsa_id != y)

  feols(value ~ sunab(enter_year, year,
                      ref.c = c("2020", "2019", "2018", "2017")) +
          st_rain:tg + st_tmp:tg | tg^fips+ year, data = new,
        cluster = "cbsa_id", weights = ~weights, notes = F) %>%

    tidy(.)  %>%
    mutate(score = gsub(".*::", "", term)) %>%
    filter(grepl("year", term)) %>%
    filter(score %in% c(-5:0)) %>%
    select(score, estimate)}

#### Run the 100 bootstraps ####
jacknife = mclapply(unique(filter(data, weights > 0)$cbsa_id), function(x)
  jack_se(data, x), mc.cores = 10)

#### Extract the relevant coefficients ####
sum = data.frame(tidy(est)) %>%
  mutate(score = gsub(".*::", "", term)) %>%
  filter(grepl("year", term)) %>% filter(score %in% c(-5:0)) %>%
  select(-term); sum

#### Determine the standard error of the placebo estimates ####
jse = rbindlist(jacknife, idcol = "btr") %>%

  left_join(., select(sum, score, raw_est = estimate))

#### Aggregate the Standard Error ####
jse = ungroup(jse) %>% group_by(score) %>%
  mutate(diff = (estimate - raw_est)^2, df = (n()-1)/n()) %>%
  group_by(score) %>% summarise(jse = sum(diff)*unique(df)) %>%
  mutate(jse = sqrt(jse))

#### Add the placebo SE to the data set ####
sum = left_join(sum, jse)  %>%
  mutate(jp = 2*pnorm(abs(estimate/jse), lower.tail = F))

#### Re-organize the data set ####
sum = select(sum, c(score, estimate, std.error = jse, p.value = jp))

#### Check the final data set ####
sum %>% head(.)

#### Save the summary values ####
write_rds(sum, file = "03_gen/05_results/sdid_sadd_dynamic.rds")

#### Clear the space ####
rm(list = ls()); gc()
#### _____________________________________________________________________ ####
#### Table 4 - Columns 1-3:  Effects of Uber on the number of air quality alerts (AQI > 100)####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

#### Load the packages ####
library(tidyverse)
library(synthdid)
library(data.table)
library(fixest)
library(broom)
library(etwfe)

#### Load the regressions data ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")

#### Transform from wide to long format ####
data = gather(data, var, value, c(alert:days_c))


#### Run the regression across all dependent variables and models ####
est =  lapply(split(data, f = data$var), function(x)

  list(TWFE = feols(value ~ treatment | year + fips,
                    data = x %>% filter(score %in% seq(-5, 0, 1)| treated == 0 ),
                    cluster = "cbsa_id"),

       `E-TWFE` = etwfe(fml  = value ~ 1, tvar = year, gvar = enter_year,
                        data = x%>% filter(score %in% seq(-5, 0, 1)| treated == 0),
                        vcov = ~cbsa_id),

       `SADD` = feols(value ~ sunab(enter_year, year) |
                        year + fips,
                      data = x %>% filter(score %in% seq(-5, 0, 1)| treated == 0 ),
                      cluster = "cbsa_id") %>% summary(agg = "att")))


#### Extract the coefficient table ####
sum = lapply(est, function(x) lapply(x, function(x)
  data.frame(R2 = r2(x)[2], BIC = BIC(x), N.Obs = nobs(x)))) %>%
  lapply(rbindlist, idcol = "estimator") %>%
  rbindlist(., idcol = "var")

#### Aggregate the E-TWFE estimator ####
est <- lapply(est, function(x) lapply(x, function(x) {
  if("etwfe" %in% class(x)) {x <- emfx(x)}; x}))

#### Extract the relevant coefficients ####
sum = lapply(est, function(x) lapply(x, function(x)

  data.frame(tidy(x)) |>
    select(term, estimate, std.error, p.value))) %>%

  lapply(., rbindlist, idcol = "estimator") %>%
  rbindlist(., idcol = "var") %>%

  filter(term == "treatment" | term == "ATT" | term == ".Dtreat")%>%

  left_join(., sum)


#### Add the pre-treatment average ####
sum = data %>% filter(treatment == 0 & treated == 1) %>%
  group_by(var) %>% filter(score %in% seq(-5, 0, 1)) %>%
  summarise(`pt_value` = mean(value))  %>%
  left_join(sum, .)

#### Include the number of observations ####
sum$N.Cohorts = length(unique(data$enter_year))
sum$N.Counties = length(unique(data$fips))
sum$N.Periods = length(unique(data$year))

#### Organize the data set #####
sum = select(sum, c(var, estimator, estimate:N.Periods, pt_value)); sum

#### Check the final data set ####
sum %>% head(.)

#### Save the summary values ####
write_rds(sum, file = "03_gen/05_results/raw_alerts.rds")

#### Clear the space ####
rm(list = ls()); gc()


#### _____________________________________________________________________ ####
#### Table 4 - Columns 4-6: SDiD For Air Quality Alerts ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

#### Load the packages ####
library(tidyverse)
library(synthdid)
library(data.table)
library(parallel)
library(etwfe)
library(fixest)
library(broom)

#### Change the working directoy ####
setwd(root)

#### Load the regressions data ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")

#### Transform from wide to long format ####))
data = gather(data, var, value, c(alert:days_c))

#### Determine the enter years ####
tg = sort(unique(filter(data, enter_year < 2017)$enter_year))

#### Split the data into a list of data frames for each treatment group ####
data = lapply(tg, function(x)
  filter(data, enter_year >= x) %>%
    mutate(tg = x, score = year - tg) %>%
    filter(score %in% seq(-5, 0, 1)) %>%
    mutate(treated = ifelse(enter_year == tg, 1, 0)) %>%
    mutate(treatment = ifelse(year >= enter_year & treated == 1, 1, 0)))

#### Transform the data frames into a balanced panel ####
data = lapply(data, function(x)
  x %>% group_by(fips, var) %>% mutate(count = n()) %>%
    filter(count == 6) %>% as.data.frame(.))

#### Add names to the list elements ####
names(data) = tg

#### Split the data sets by dependent variable ###
data = lapply(data, function(x) split(x, f = x$var))

#### Construct the panel object for the SDiD algorithm ####
panel = lapply(data, function(x) lapply(x, function(x)
  panel.matrices(x, unit = "fips", time = "score",
                 outcome = "value", treatment = "treatment")))

#### Estimate SDiD ####
synth = lapply(panel, function(x) lapply(x, function(x)
  synthdid::synthdid_estimate(x$Y, x$N0, x$T0)))

#### Extract the unit weights ####
weights = list(uw = lapply(synth, function(x) lapply(x, function(x)
  data.frame(synthdid_controls(x, weight.type='omega', mass = 1)) %>%
    mutate(fips = rownames(.)) %>% rename(uw = estimate.1))) %>%
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

#### Run the regression across all dependent variables and models ####
#data = filter(data, var == "alert")

#### Estimate the effect on alerts ####
est =  lapply(split(data, f = data$var), function(x)

  list(TWFE = feols(value ~ treatment | year + fips,
                    data = x, weights = ~weights, notes = F),

       `E-TWFE` = etwfe(fml  = value ~ 1,
                        tvar = year, gvar = enter_year,
                        data = x, weights = ~weights, fe = "vs"),

       `SADD` = feols(value ~ sunab(enter_year, year)  |
                        year + fips, data = x, notes = F,
                      weights = ~weights) %>% summary(agg = "att")))

#### Extract the coefficient table ####
coef = lapply(est, function(x) lapply(x, function(x)
  data.frame(R2 = r2(x)[2], BIC = BIC(x), N.Obs = nobs(x)))) %>%
  lapply(rbindlist, idcol = "estimator") %>%
  rbindlist(., idcol = "var")

#### Aggregate the E-TWFE estimator ####
est <- lapply(est, function(x) lapply(x, function(x) {
  if("etwfe" %in% class(x)) {x <- emfx(x)}; x}))

#### Construct the function to estimate the Jackknife SEs ####
jack_se = function(x, y){

  new = filter(x, cbsa_id != y)

  # run second stage
  lapply(split(new, f = new$var), function(x)

    list(`TWFE` = feols(value ~ treatment | year + fips,
                        data = x, weights = ~weights, notes = F),

         `SADD` = feols(value ~ sunab(enter_year, year) |
                          year + fips, data = x, notes = F,
                        weights = ~weights) %>% summary(agg = "att"),

         `E-TWFE` = etwfe(fml  = value ~ 1,
                          tvar = year, gvar = enter_year,
                          data = x, weights = ~weights) %>%
           emfx(., vcov = F)) %>%

      lapply(., function(x) x = tidy(x) %>%
               filter(term == "treatment" | term == "ATT" | term == ".Dtreat") %>%
               select(estimate))) %>%

    lapply(., rbindlist, idcol = "estimator") %>%

    rbindlist(., idcol = "var")}

#### Run the 100 bootstraps ####
#library(parallel)
jacknife = mclapply(unique(filter(data, weights > 0)$cbsa_id), function(x)
  jack_se(data, x), mc.cores = 10)

#### Extract the relevant coefficients ####
sum = lapply(est, function(x) lapply(x, function(x)
  x = data.frame(tidy(x)) |> select(term, estimate))) %>%

  lapply(., rbindlist, idcol = "estimator") %>%
  rbindlist(., idcol = "var") %>%

  filter(term == "treatment" | term == "ATT" | term == ".Dtreat")

#### Determine the standard error of the placebo estimates ####
jse = rbindlist(jacknife, idcol = "btr") %>%

  left_join(., select(sum, var, estimator, raw_est = estimate))

#### Aggregate the Standard Error ####
jse = ungroup(jse) %>% group_by(var, estimator) %>%
  mutate(diff = (estimate - raw_est)^2, df = (n()-1)/n()) %>%
  group_by(var, estimator) %>% summarise(jse = sum(diff)*unique(df)) %>%
  mutate(jse = sqrt(jse))

#### Add the placebo SE to the data set ####
sum = left_join(sum, jse)  %>%
  mutate(jp = 2*pnorm(abs(estimate/jse), lower.tail = F))

#### Include additional statistics####
sum = left_join(sum, coef)

#### Include the number of observations ####
sum$N.Counties = length(unique(data$fips))
sum$N.Periods = length(unique(data$year))
sum$N.Cohorts = length(unique(data$enter_year))

#### Add the pre-treatment average ####
sum = data %>% filter(treatment == 0 & treated == 1) %>%
  group_by(var) %>%
  summarise(pt_value = mean(value, na.rm = T)) %>%
  left_join(sum,.)

#### Re-organize the data ####
sum = select(sum, c(var, estimator, estimate,
                    std.error = jse, p.value = jp,
                    R2:pt_value))

#### Check the final data set ####
sum

#### Save the summary values ####
write_rds(sum, file = "03_gen/05_results/sdid_alerts.rds")

#### Clear the space ####
rm(list = ls()); gc()


#### _____________________________________________________________________ ####
#### Table 5 -- Specs 1-2 Effect of Uber on the amount of light-duty vehicles ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls());gc()

#### Load packages ####
library(tidyverse)
library(synthdid)
library(data.table)
library(fixest)
library(etwfe)
library(broom)

#### Load the data ####
data = read_rds("03_gen/04_reg/yearly_reg.rds") %>% filter(year < 2017)
cars = read_csv("02_data/06_other/02_AvailableCars/AvailableCars.csv")

#### Transform the cars data from wide to long format ####
cars = gather(cars, year, cars, -states, -county, -code, -fips) %>%
  mutate(year = as.numeric(gsub("vehiclereg_", "", year))) %>%
  select(fips, year, cars) %>% mutate(fips = sprintf("%05d", fips))

#### Function to linearly interpolate observations from 2005 to 2009 ####
predict_cars <- function(data) {
  lm_model <- lm(cars ~ year, data = data)
  predict_years <- data.frame(year = 2005:2009)
  predicted_cars <- predict(lm_model, newdata = predict_years)
  return(data.frame(year = 2005:2009, cars = predicted_cars))
}

#### Apply the function for each FIPS code and bind with original data ####
predicted_data <- cars %>%
  group_by(fips) %>%
  do(predict_cars(.))

#### Combine the original and predicted data ####
cars <- bind_rows(cars, predicted_data) %>%
  arrange(fips, year)

#### Left join the available cars to the Uber data ####
data = left_join(data, cars)

#### Compute the available number of household cars per 1,000 inhabitants ####
data = mutate(data, cars = (cars/pop)*1000)

#### Estimate the effects with the weights from the SDiD model ####
est = list(`(1) TWFE` = feols(cars ~ treatment + treated:incomepercapita | year + fips,
                              data = data, cluster = "cbsa_id"),

           `(2) TWFE` = feols(cars ~ treatment + treated:incomepercapita | year + fips,
                              data = data %>% filter(score %in% seq(-5, 0, 1)| treated == 0 ),
                              cluster = "cbsa_id"),

           `(1) SADD` = feols(cars ~ sunab(enter_year, year) + incomepercapita | year + fips,
                              data = data,
                              cluster = "cbsa_id") %>% summary(agg = "att"),

           `(2) SADD` = feols(cars ~ sunab(enter_year, year) + incomepercapita  | year + fips,
                              data = data %>% filter(score %in% seq(-5, 0, 1)| treated == 0 ),
                              cluster = "cbsa_id") %>% summary(agg = "att"),

           `(1) ETWFE`= etwfe(fml = cars ~ incomepercapita, tvar = year, gvar = enter_year,
                              data = data, vcov = ~cbsa_id,
                              ivar = cbsa_id, fe = "vs"),

           `(2) ETWFE`= etwfe(fml  = cars ~ incomepercapita, tvar = year,  gvar = enter_year,
                              data = data %>% filter(score %in% seq(-5, 0, 1)| treated == 0 ),
                              vcov = ~cbsa_id, ivar = cbsa_id, fe = "vs"))

#### Extract the coefficient table ####
fitted = lapply(est, function(x)
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

  left_join(., fitted)

#### Add the pre-treatment average ####
sum = list(data %>% filter(treatment == 0 & treated == 1) %>% ungroup() %>%
             filter(score %in% seq(-5, 0, 1)) %>%
             summarise(`pt_value` = mean(cars, na.rm = T))  %>%
             crossing(tibble(estimator = c("(2) TWFE", "(2) ETWFE", "(2) SADD"))),

           data %>% filter(treatment == 0 & treated == 1) %>% ungroup() %>%
             summarise(`pt_value` = mean(cars, na.rm = T))  %>%
             crossing(tibble(estimator = c("(1) TWFE", "(1) ETWFE", "(1) SADD")))) %>%
  rbindlist(.) %>% left_join(sum, .)

#### Include the number of observations ####
sum$N.Cohorts = length(unique(data$enter_year))
sum$N.Counties = length(unique(data$fips))
sum$N.Periods = length(unique(data$year))

#### Organize the data set #####
sum = select(sum, c(estimator, estimate:N.Periods, pt_value)); sum

#### Check the final data set ####
sum %>% head(.)

#### Save the summary values ####
write_rds(sum, file = "03_gen/05_results/raw_hh_vehicles.rds")

#### Clear the space ####
rm(list = ls()); gc()
#### _____________________________________________________________________ ####
#### Table 5 -- Specs 3 Effect of Uber on the amount of light-duty vehicles ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls());gc()

#### Load packages ####
library(tidyverse)
library(synthdid)
library(data.table)
library(fixest)
library(etwfe)
library(broom)

#### Load the data ####
data = read_rds("03_gen/04_reg/yearly_reg.rds") %>% filter(year < 2017)
cars = read_csv("02_data/06_other/02_AvailableCars/AvailableCars.csv")

#### Transform the cars data from wide to long format ####
cars = gather(cars, year, cars, -states, -county, -code, -fips) %>%
  mutate(year = as.numeric(gsub("vehiclereg_", "", year))) %>%
  select(fips, year, cars) %>% mutate(fips = sprintf("%05d", fips))

#### Function to linearly interpolate observations from 2005 to 2009 ####
predict_cars <- function(data) {
  lm_model <- lm(cars ~ year, data = data)
  predict_years <- data.frame(year = 2005:2009)
  predicted_cars <- predict(lm_model, newdata = predict_years)
  return(data.frame(year = 2005:2009, cars = predicted_cars))
}

#### Apply the function for each FIPS code and bind with original data ####
predicted_data <- cars %>%
  group_by(fips) %>%
  do(predict_cars(.))

#### Combine the original and predicted data ####
cars <- bind_rows(cars, predicted_data) %>%
  arrange(fips, year)

#### Left join the available cars to the Uber data ####
data = left_join(data, cars)

#### Compute the available number of household cars per 1,000 inhabitants ####
data = mutate(data, cars = (cars/pop)*1000)

#### Filter missing observations ####
data = filter(data, is.na(cars) == F)

#### Determine the enter years ####
tg = sort(unique(filter(data, enter_year < 2017)$enter_year))

#### Split the data into a list of data frames for each treatment group ####
data = lapply(tg, function(x)
  filter(data, enter_year >= x) %>%
    mutate(tg = x, score = year - tg) %>%
    filter(score %in% seq(-5, 0, 1)) %>%
    mutate(treated = ifelse(enter_year == tg, 1, 0)) %>%
    mutate(treatment = ifelse(year >= enter_year & treated == 1, 1, 0)))

#### Transform the data frames into a balanced panel ####
data = lapply(data, function(x)
  x %>% group_by(fips) %>% mutate(count = n()) %>%
    filter(count == 6) %>% as.data.frame(.))

#### Add names to the list elements ####
names(data) = tg

#### Construct the panel object for the SDiD algorithm ####
panel = lapply(data, function(x)
  panel.matrices(x, unit = "fips", time = "score",
                 outcome = "cars", treatment = "treatment"))

#### Estimate synth DD ####
synth = lapply(panel, function(x)
  synthdid::synthdid_estimate(x$Y, x$N0, x$T0))

#### Extract the unit weights ####
weights = list(uw = lapply(synth, function(x)
  data.frame(synthdid_controls(x, weight.type='omega', mass = 1)) %>%
    mutate(fips = rownames(.)) %>% rename(uw = estimate.1)) %>%
    rbindlist(., idcol = "tg"),

  tw = lapply(synth, function(x)
    data.frame(synthdid_controls(x, weight.type='lambda', mass = 1)) %>%
      mutate(score = as.numeric(rownames(.))) %>% rename(tw = estimate.1)) %>%
    rbindlist(., idcol = "tg"))

#### Add the weights to the data sets ####
data =  rbindlist(data) %>%
  mutate(tg = as.character(tg)) %>%
  left_join(., weights$uw) %>% left_join(., weights$tw) %>%
  mutate(uw = ifelse(treated > 0, 1, uw),
         tw = ifelse(score >= 0, 1, tw)) %>%
  mutate(weights = uw*tw)

#### Estimate the effects with the weights from the SDiD model ####
est = list(

  `(3) TWFE` = feols(cars ~ treatment + treated:incomepercapita | year + fips,
                     data = data,
                     cluster = "cbsa_id", weights = ~weights),

  `(3) SADD` = feols(cars ~ sunab(enter_year, year) + incomepercapita | year + fips,
                     data = data, weights = ~weights,
                     cluster = "cbsa_id") %>% summary(agg = "att"),

  `(3) ETWFE`= etwfe(fml  = cars ~ incomepercapita, tvar = year,  gvar = enter_year,
                     data = data, vcov = ~cbsa_id, weights = ~weights, fe = "vs"))

#### Extract the coefficient table ####
coef = lapply(est, function(x)
  data.frame(R2 = r2(x)[2], BIC = BIC(x), N.Obs = nobs(x))) %>%
  rbindlist(., idcol = "estimator")

#### Aggregate the E-TWFE estimator ####
est <- lapply(est, function(x) {
  if("etwfe" %in% class(x)) {x <- emfx(x)}; x})

#### Construct the function to estimate the Jacknife SEs ####
jack_se = function(x, y){

  new = filter(x, cbsa_id != y)

  # run second stage
  list(

    `(3) TWFE` = feols(cars ~ treatment + treated:incomepercapita | year + fips,
                       data = new, notes = F,
                       cluster = "cbsa_id", weights = ~weights),

    `(3) SADD` = feols(cars ~ sunab(enter_year, year) + incomepercapita | year + fips,
                       data = new, weights = ~weights, notes = F,
                       cluster = "cbsa_id") %>% summary(agg = "att"),

    `(3) ETWFE`= etwfe(fml  = cars ~ incomepercapita, tvar = year,  gvar = enter_year,
                       data = new, vcov = ~cbsa_id, weights = ~weights) %>%
      emfx(.)) %>%

    lapply(., function(x) x = tidy(x) %>%
             filter(term == "treatment" | term == "ATT" | term == ".Dtreat") %>%
             select(estimate)) %>%

    rbindlist(., idcol = "estimator")}

#### Run the 100 bootstraps ####
jacknife = mclapply(unique(filter(data, weights > 0)$cbsa_id), function(x)
  jack_se(data, x), mc.cores = 20)

#### Extract the relevant coefficients ####
sum = lapply(est, function(x)
  x = data.frame(tidy(x)) |> select(term, estimate)) %>%

  rbindlist(., idcol = "estimator") %>%

  filter(term == "treatment" | term == "ATT" | term == ".Dtreat")

#### Determine the standard error of the placebo estimates ####
jse = rbindlist(jacknife, idcol = "btr") %>%

  left_join(., select(sum, estimator, raw_est = estimate))

#### Aggregate the Standard Error ####
jse = ungroup(jse) %>% group_by(estimator) %>%
  mutate(diff = (estimate - raw_est)^2, df = (n()-1)/n()) %>%
  group_by(estimator) %>% summarise(jse = sum(diff)*unique(df)) %>%
  mutate(jse = sqrt(jse))

#### Add the placebo SE to the data set ####
sum = left_join(sum, jse)  %>%
  mutate(jp = 2*pnorm(abs(estimate/jse), lower.tail = F))

#### Include additional statistics####
sum = left_join(sum, coef)

#### Include the number of observations ####
sum$N.Counties = length(unique(data$fips))
sum$N.Periods = length(unique(data$year))
sum$N.Cohorts = length(unique(data$enter_year))

#### Add the pre-treatment average ####
sum$pt_value = (data %>% filter(treatment == 0 & treated == 1) %>%
                  summarise(pt_value = mean(cars, na.rm = T)))$pt_value

#### Re-organize the data ####
sum = select(sum, c(estimator, estimate,
                    std.error = jse, p.value = jp,
                    R2:pt_value))

#### Check the final data set ####
sum %>% head(.)

#### Save the summary values ####
write_rds(sum, file = "03_gen/05_results/synth_hh_vehicles.rds")

#### Clear the space ####
rm(list = ls()); gc()


#### _____________________________________________________________________ ####
#### Table 6 - Specs 1-4: Effect of Uber on commuting behavior (DiD) ####
#### Figure I13 -- Effect of Uber on commuting behavior (robustness)  ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

#### Load the packages ####
library(tidyverse)
library(synthdid)
library(data.table)
library(fixest)
library(broom)

#### Load the regressions data ####
data = read_rds("03_gen/04_reg/yearly_reg.rds") %>% filter(year < 2017)

#### Load the transportation index ####
tindex = read_rds("02_data/06_other/01_pti/pti_b.rds")%>%
  mutate(GEOID = sprintf("%05s", GEOID)) %>%
  select(c(year, GEOID, cars:walk, other, total, imputed))

#### Filter incomplete data ####
na_fips = unique(filter(tindex, is.na(total) == T)$GEOID)

#### Add the t-index data ####
data = left_join(data, tindex %>% mutate(year = as.numeric(year)) %>%
                   rename(fips = GEOID))

#### Add the bike and walk modes together ####
data = mutate(data, bike_walk = walk + bike) %>% select(-bike, -walk)

#### Transform the data from wide to long format ####
data = gather(data, var, value, c(cars, public_transport, bike_walk, other))

#### Estimate the number of counties with valid data ####
length(unique(data$fips))
length(unique((data |> filter(is.na(total) == F))$fips))
length(unique((data |> filter(!(fips %in% na_fips)))$fips))

#### Estimate the effect ####
est = lapply(split(data, f = data$var), function(x)

  list(`(1) TWFE` = feols(log(value/total) ~ treatment|
                            year + fips, notes = F,
                          data = x, cluster = "cbsa_id"),

       `(2) TWFE` = feols(log(value/total) ~ treatment  |
                            year + fips, notes = F,
                          data = x |> filter(score %in% seq(-5, 1, 1) | treated == 0),
                          cluster = "cbsa_id"),

       `(3) TWFE` = feols(log(value/total) ~ treatment  |
                            year + fips, notes = F,
                          data = x |> filter(score %in% seq(-5, 0, 1) | treated == 0),
                          cluster = "cbsa_id"),

       `(4) TWFE` = feols(log(value/total) ~ treatment |
                            year + fips, notes = F,
                          data = x |> filter(!(fips %in% na_fips)),
                          cluster = "cbsa_id"),

       `(1) SADD` = feols(log(value/total) ~ sunab(enter_year, year) |
                            enter_year + year, notes = F,
                          data = x, cluster = "cbsa_id") %>%  summary(agg = "att"),


       `(2) SADD` = feols(log(value/total) ~ sunab(enter_year, year) |
                            enter_year + year, notes = F,
                          data = x|> filter(score %in% seq(-5, 1, 1) | treated == 0),
                          cluster = "cbsa_id") %>%  summary(agg = "att"),

       `(3) SADD` = feols(log(value/total) ~ sunab(enter_year, year) |
                            enter_year + year, notes = F,
                          data = x|> filter(score %in% seq(-5, 0, 1) | treated == 0),
                          cluster = "cbsa_id") %>%  summary(agg = "att"),

       `(4) SADD` = feols(log(value/total) ~ sunab(enter_year, year) +
                            st_tmp + st_rain | enter_year + year, notes = F,
                          data = x|> filter(!(fips %in% na_fips)),
                          cluster = "cbsa_id") %>%  summary(agg = "att"),

       `(1) ETWFE` = etwfe(fml  = log(value/total) ~ 1,
                           tvar = year,  gvar = enter_year,
                           data = x, vcov = ~cbsa_id, fe = "vs") %>% emfx(.),

       `(2) ETWFE` = etwfe(fml  = log(value/total) ~ 1,
                           tvar = year,  gvar = enter_year,
                           data = x|> filter(score %in% seq(-5, 1, 1) | treated == 0),
                           vcov = ~cbsa_id, fe = "vs") %>% emfx(.),

       `(3) ETWFE` = etwfe(fml  = log(value/total) ~ 1,
                           tvar = year,  gvar = enter_year,
                           data = x|> filter(score %in% seq(-5, 0, 1) | treated == 0),
                           vcov = ~cbsa_id, fe = "vs") %>% emfx(.),

       `(4) ETWFE` = etwfe(fml  = log(value/total) ~ 1,
                           tvar = year,
                           gvar = enter_year,
                           data = x |> filter(!(fips %in% na_fips)),
                           vcov = ~cbsa_id, fe = "vs") %>% emfx(.)))

#### Extract the relevant coefficients ####
sum = lapply(est, function(x) lapply(x, function(x)
  x = data.frame(tidy(x)) |>
    select(term, estimate, std.error, p.value))) %>%

  lapply(., rbindlist, idcol = "spec") %>%
  rbindlist(., idcol = "var") %>%

  filter(term == "treatment" | term == "ATT" | term == ".Dtreat")

#### Transform the point estimates into percentage changes ####
sum = mutate_at(sum, vars(estimate, std.error), function(x)
  100*(exp(x)-1))

#### Extract the coefficients ####
sum %>% filter(grepl("ETWFE", spec))

#### Save the summary values ####
write_rds(sum, file = "03_gen/05_results/did_commuting_ols.rds")

#### Clear the space ####
rm(list = ls()); gc()
#### _____________________________________________________________________ ####
#### Table 6 - Spec 5 Effect of Uber on commuting behavior (SDiD) ####
#### Figure I13 -- Effect of Uber on commuting behavior (robustness)  ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

#### Set the working directory ####
setwd(root)

#### Load the packages ####
library(tidyverse)
library(synthdid)
library(data.table)
library(fixest)
library(parallel)
library(broom)
library(etwfe)

#### Load the regressions data ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")

#### Load the data on the transportation index ####
tindex = read_rds("02_data/06_other/01_pti/pti_b.rds")%>%
  mutate(GEOID = sprintf("%05s", GEOID)) %>%
  select(c(year, GEOID, cars:walk, other, total, imputed))

#### Filter incomplete data ####
na_fips = filter(tindex, is.na(total) == T)$GEOID
tindex = filter(tindex, !(GEOID %in% na_fips))

#### Add the t index data ####
data = left_join(data, tindex %>% mutate(year = as.numeric(year)) %>%
                   rename(fips = GEOID))

#### Create a composite of bike and walk ####
data = mutate(data, bike_walk = walk + bike) %>% select(-bike, -walk)

#### Gather the variables ####
data = gather(data, var, value, c(cars, public_transport, other))

#### Take away all NAs in total ####
data = filter(data, is.na(value) == F)

#### Transform into shares ####
data = mutate(data, value = 100*(value/total)); rm(tindex, na_fips)

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
names(data) = tg; rm(tg)

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
    mutate(fips = rownames(.)) %>% rename(uw = estimate.1))) %>%
    lapply(., rbindlist, idcol = "var") %>% rbindlist(., idcol = "tg"),

  tw = lapply(synth, function(x) lapply(x, function(x)
    data.frame(synthdid_controls(x, weight.type='lambda', mass = 1)) %>%
      mutate(score = as.numeric(rownames(.))) %>% rename(tw = estimate.1))) %>%
    lapply(., rbindlist, idcol = "var") %>% rbindlist(., idcol = "tg")); rm(panel, synth)

#### Add the weights to the data sets ####
data = lapply(data, rbindlist) %>% rbindlist(.) %>%
  mutate(tg = as.character(tg)) %>%
  left_join(., weights$uw) %>% left_join(., weights$tw) %>%
  mutate(uw = ifelse(treated > 0, 1, uw),
         tw = ifelse(score >= 0, 1, tw)) %>%
  mutate(weights = uw*tw); rm(weights)

#### Estimate the effect ####
est = lapply(split(data, f = data$var), function(x)

  list(`SDiD-OLS` = feols(log(value) ~ treatment  | year + fips, notes = F,
                          data = x, weights = ~weights),

       `SDiD-SADD` = feols(log(value) ~ sunab(enter_year, year) |
                             enter_year + year, notes = F,
                           data = x, weights = ~weights) %>%  summary(agg = "att"),

       `SDiD-ETWFE` = etwfe(log(value) ~ 1,
                            tvar = year,
                            gvar = enter_year,
                            data = x,
                            weights = ~weights, fe = "vs") %>% emfx(., vcov = F)))

#### Construct the function to estimate the Jackknife SEs ####
jack_se = function(x, y){

  new = filter(x, cbsa_id != y)

  # run second stage
  lapply(split(new, f = new$var), function(x)

    list(`SDiD-OLS` = feols(log(value) ~ treatment | year + fips, notes = F,
                            data = x, weights = ~weights),

         `SDiD-SADD` = feols(log(value) ~ sunab(enter_year, year) |
                               enter_year + year, notes = F,
                             data = x, weights = ~weights) %>%
           summary(agg = "att"),

         `SDiD-ETWFE` = etwfe(fml  = log(value) ~ 1,
                              tvar = year,
                              gvar = enter_year,
                              data = x,
                              weights = ~weights) %>% emfx(., vcov = F)) %>%

      lapply(., function(x) x = tidy(x) %>% filter(term == "treatment" | term == "ATT" | term == ".Dtreat") %>%
               select(estimate))) %>%

    lapply(., rbindlist, idcol = "spec") %>%

    rbindlist(., idcol = "var")}

#### Run the 100 bootstraps ####
jacknife = mclapply(unique(data$cbsa_id), function(x)
  jack_se(data, x), mc.cores = 20)

#### Extract the relevant coefficients ####
sum = lapply(est, function(x) lapply(x, function(x)
  x = data.frame(tidy(x)) |> select(term, estimate))) %>%

  lapply(., rbindlist, idcol = "spec") %>%
  rbindlist(., idcol = "var") %>%

  filter(term == "treatment" | term == "ATT" | term == ".Dtreat")


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

#### Transform the point estimates into percentage changes ####
sum = mutate_at(sum, vars(estimate, std.error), function(x)
  100*(exp(x)-1))

#### Extract the coefficients ####
sum %>% filter(grepl("ETWFE", spec))

#### Save the summary values ####
write_rds(sum, file = "03_gen/05_results/sdid_commuting_ols.rds")

#### Clear the space ####
rm(list = ls()); gc()


#### _____________________________________________________________________ ####
#### Table 6 - Spec 5 Effect of Uber on commuting behavior (SDiD-restricted) ####
#### _____________________________________________________________________ ####
rm(list = ls()); gc()

#### Set the working directory ####
setwd(root)

#### Load the packages ####
library(tidyverse)
library(synthdid)
library(data.table)
library(fixest)
library(parallel)
library(broom)

#### Load the regressions data ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")

#### Load the data on the transportation index ####
tindex = read_rds("02_data/06_other/01_pti/pti_b.rds")%>%
  mutate(GEOID = sprintf("%05s", GEOID)) %>%
  select(c(year, GEOID, cars:walk, other, total, imputed))

#### Filter incomplete data ####
na_fips = filter(tindex, is.na(total) == T)$GEOID
tindex = filter(tindex, !(GEOID %in% na_fips))

#### Add the t index data ####
data = left_join(data, tindex %>% mutate(year = as.numeric(year)) %>%
                   rename(fips = GEOID))

#### Create a composite of bike and walk ####
data = mutate(data, bike_walk = walk + bike) %>% select(-bike, -walk)

#### Gather the variables ####
data = gather(data, var, value, c(cars, public_transport, other))

#### Take away all NAs in total ####
data = filter(data, is.na(value) == F)

#### Transform into shares ####
data = mutate(data, value = 100*(value/total)); rm(tindex, na_fips)

#### Determine the enter years ####
tg = sort(unique(filter(data, enter_year < 2017)$enter_year))

#### Split the data into a list of data frames for each treatment group ####
data = lapply(tg, function(x)
  filter(data, enter_year >= x) %>%
    mutate(tg = x, score = year - tg) %>%
    filter(score %in% seq(-5, 0, 1)) %>%
    mutate(treated = ifelse(enter_year == tg, 1, 0)) %>%
    mutate(treatment = ifelse(year >= enter_year & treated == 1, 1, 0)))

#### Transform the data frames into a balanced panel ####
data = lapply(data, function(x)
  x %>% group_by(fips, var) %>% mutate(count = n()) %>%
    filter(count == 6) %>% as.data.frame(.))

#### Add names to the list elements ####
names(data) = tg; rm(tg)

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
    mutate(fips = rownames(.)) %>% rename(uw = estimate.1))) %>%
    lapply(., rbindlist, idcol = "var") %>% rbindlist(., idcol = "tg"),

  tw = lapply(synth, function(x) lapply(x, function(x)
    data.frame(synthdid_controls(x, weight.type='lambda', mass = 1)) %>%
      mutate(score = as.numeric(rownames(.))) %>% rename(tw = estimate.1))) %>%
    lapply(., rbindlist, idcol = "var") %>% rbindlist(., idcol = "tg")); rm(panel, synth)

#### Add the weights to the data sets ####
data = lapply(data, rbindlist) %>% rbindlist(.) %>%
  mutate(tg = as.character(tg)) %>%
  left_join(., weights$uw) %>% left_join(., weights$tw) %>%
  mutate(uw = ifelse(treated > 0, 1, uw),
         tw = ifelse(score >= 0, 1, tw)) %>%
  mutate(weights = uw*tw); rm(weights)

#### Estimate the effect ####
est = lapply(split(data, f = data$var), function(x)

  list(`SDiD-OLS` = feols(log(value) ~ treatment  | year + fips, notes = F,
                          data = x, weights = ~weights),

       `SDiD-SADD` = feols(log(value) ~ sunab(enter_year, year) |
                             enter_year + year, notes = F,
                           data = x, weights = ~weights) %>%  summary(agg = "att"),

       `SDiD-ETWFE` = etwfe(log(value) ~ 1,
                            tvar = year,
                            gvar = enter_year,
                            data = x,
                            weights = ~weights, fe = "vs") %>% emfx(., vcov = F)))

#### Construct the function to estimate the Jacknife SEs ####
jack_se = function(x, y){

  new = filter(x, cbsa_id != y)

  # run second stage
  lapply(split(new, f = new$var), function(x)

    list(`SDiD-OLS` = feols(log(value) ~ treatment | year + fips, notes = F,
                            data = x, weights = ~weights),

         `SDiD-SADD` = feols(log(value) ~ sunab(enter_year, year) |
                               enter_year + year, notes = F,
                             data = x, weights = ~weights) %>%
           summary(agg = "att"),

         `SDiD-ETWFE` = etwfe(fml  = log(value) ~ 1,
                              tvar = year,
                              gvar = enter_year,
                              data = x,
                              weights = ~weights) %>% emfx(., vcov = F)) %>%

      lapply(., function(x) x = tidy(x) %>% filter(term == "treatment" | term == "ATT" | term == ".Dtreat") %>%
               select(estimate))) %>%

    lapply(., rbindlist, idcol = "spec") %>%

    rbindlist(., idcol = "var")}

#### Run the 100 bootstraps ####
jacknife = mclapply(unique(data$cbsa_id), function(x)
  jack_se(data, x), mc.cores = 20)

#### Extract the relevant coefficients ####
sum = lapply(est, function(x) lapply(x, function(x)
  x = data.frame(tidy(x)) |> select(term, estimate))) %>%

  lapply(., rbindlist, idcol = "spec") %>%
  rbindlist(., idcol = "var") %>%

  filter(term == "treatment" | term == "ATT" | term == ".Dtreat")


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

#### Transform the point estimates into percentage changes ####
sum = mutate_at(sum, vars(estimate, std.error), function(x)
  100*(exp(x)-1))

#### Extract the coefficients ####
sum %>% filter(grepl("ETWFE", spec))

#### Save the summary values ####
write_rds(sum, file = "03_gen/05_results/sdid_commuting_rest_ols.rds")

#### _____________________________________________________________________ ####
#### Table 8: Effects of Uber on gasoline sales in California ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

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

#### Transform the cars data from wide to long format ####
gas = select(gas, fips, year, sales, stations) %>%
  mutate(fips = as.numeric(fips))

#### Left join the available cars to the Uber data ####
data = left_join(data %>% mutate(fips = as.numeric(fips)), gas)

#### Calculate the pre-treatment value on the number of available cars ####
mean(filter(data, enter_year > year)$sales, na.rm = T)

#### Run the DiD models for the transportation index ####
est = list(`(1)` = feols(log(sales/stations) ~ treatment  |
                           year + fips, notes = F,
                         data = data, cluster = "cbsa_id",
                         weights = ~(pop/cty_size)),


           `(2)` = feols(log(sales/stations) ~ sunab(enter_year, year) | fips + year, notes = F,
                         data = data, cluster = "cbsa_id",
                         weights = ~(pop/cty_size)) %>% summary(agg = "att"),

           `(3)` = etwfe(fml  = log(sales/stations) ~ 1,
                         tvar = year,
                         gvar = enter_year,
                         data = data, vcov = ~cbsa_id,
                         weights = ~(pop/cty_size)),


           `(4)` = etwfe(fml  = log(sales/stations) ~ 1,
                         tvar = year,
                         gvar = enter_year,
                         data = data|> filter(score %in% seq(-5, 3, 1) | treated == 0),
                         vcov = ~cbsa_id, weights = ~(pop/cty_size)))

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
sum = list(data %>% filter(treatment == 0 & treated == 1) %>% ungroup() %>%
             summarise(`pt_value` = mean(sales/stations, na.rm = T))  %>%
             crossing(tibble(spec = paste0("(", c(1:3), ")"))),

           data %>% filter(treatment == 0 & treated == 1) %>% ungroup() %>%
             filter(score %in% seq(-5, 3, 1)) %>%
             summarise(`pt_value` = mean(sales/stations, na.rm = T)) %>%
             mutate(spec = "(4)")) %>% rbindlist(.) %>%
  left_join(sum, .)

#### Transform the point estimates into percentage changes ####
sum = mutate_at(sum, vars(estimate, std.error), function(x)100*(exp(x)-1))

#### Add the nobs ###
sum = left_join(sum, fitted)

#### Add the number of counties ####
data = filter(data, is.na(sales) == F)
sum$N.Counties = length(unique(data$fips))
sum$N.Periods = length(unique(data$year))
sum$N.Cohorts = length(unique(data$enter_year))

#### Check the results ####
sum

#### Save the summary values ####
write_rds(sum, file = "03_gen/05_results/did_gasoline.rds")

#### Clear the space ####
rm(list = ls()); gc()
