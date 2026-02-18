#### ##################################################################### ####
####                        Figures for the study                          ####
####       (Replication Package version)                                   ####
#### ##################################################################### ####

#### Set the working directory to the replication folder ####
## Path set by 00_master.R; if running standalone, set root manually
if (!exists("root")) root <- getwd()
setwd(root)
figures_dir <- file.path(root, "output", "figures")

#### ____________________________________________________________________ ####
#### Figure 1: Expansion of Uber in the United States (2010-2016) ####
#### ____________________________________________________________________ ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Add the directory paths ####
macdir <- paste0(figures_dir, "/")

#### Add packages ####
library(tidyverse)
library(ggplot2)
library(sf)
library(ggspatial)

#### Load the data set ####
uber = read_rds("03_gen/03_uber/uber.rds")
states = st_read("02_data/05_shp/02_states", quiet = T)
counties = st_read("02_data/05_shp/03_counties", quiet = T)

#### Include the enter year  ####
uber = mutate(uber, enter_year = year(enter_date))

#### Restrict the states to continental US ####
states = filter(states, !(NAME %in% c("Alaska", "Hawaii", "Puerto Rico")))
counties = filter(counties, !(STATEFP  %in% c("02", "15", "72")))

#### Spatial distribution of treated and control units in the US  ####
plot = mutate(counties, fips = as.numeric(paste0(STATEFP, COUNTYFP))) %>%
  left_join(select(ungroup(uber), fips, enter_year, cbsa_id)) %>%
  mutate(enter_year = ifelse(enter_year > 2016, NA,  enter_year)) %>%
  st_simplify(preserveTopology = T)

#### Organize the legend ####
plot = mutate(plot, enter_year = ifelse(is.na(cbsa_id) == T, "Non Urban", enter_year))
plot = mutate(plot, enter_year = ifelse(is.na(enter_year) & is.na(cbsa_id) == F, "No Entry by 2016", enter_year))
plot = mutate(plot, enter_year = ifelse(enter_year %in% c("2010", "2011", "2012"), "Early Rollout (2010-2012)", enter_year))
plot = mutate(plot, enter_year = ifelse(enter_year %in% c("2013", "2014"), "Growth Phase (2013-2014)", enter_year))
plot = mutate(plot, enter_year = ifelse(enter_year %in% c("2015", "2016"), "Late Rollout (2015-2016)", enter_year))

#### Make the plot ####
ggplot() +
  geom_sf(data = states, fill = "black") +
  geom_sf(data = plot, lwd = 0.00001, aes(fill = enter_year),
          color = alpha("gray", 0.5)) +

  scale_fill_manual(values = c(
    "Early Rollout (2010-2012)" = "orange",

    "Growth Phase (2013-2014)" = "#88a9c3",

    "Late Rollout (2015-2016)" = "#1f628e",

    "No Entry by 2016" = "black",

    "Non Urban" = "gray90")) +

  scale_color_manual(values = alpha("red", 1)) +

  theme(panel.background = element_rect(fill = "white"),
        legend.background = element_blank(),
        legend.title = element_blank()) +

  annotation_north_arrow(location = "br", which_north = "false",
                         height = unit(1, "cm"), width = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering(fill = c("black", "white"))) +

  annotation_scale(bar_cols = c("black", "white"),
                   height = unit(0.15, "cm"), location = "bl",
                   text_cex = 1)

#### Save the plot ####
ggsave(file = paste0(macdir, "map_tratment_groups.pdf"), width = 10, height = 5)

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()
#### ..................................................................... ####
#### Table 1: Values of key variables across treatment groups ####
#### ..................................................................... ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Load packages ####
library(conflicted)
library(data.table)
library(tidyverse)
library(mmtable2)
library(kableExtra)
library(gt)

#### Solve conflicts ####
conflicts_prefer(dplyr::filter)

#### Load the data set ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")

#### Compute the average and standard deviation of key variables ####
table = data %>%

  mutate(enter_year = ifelse(enter_year > 2016, "Never Treated", as.character(enter_year))) %>%
  mutate(incomepercapita = incomepercapita/1000) |>
  mutate(pop_density = pop/cty_size) %>%
  mutate(const_exp = const_exp/1000000) %>%
  mutate(white = white*100) %>%

  ungroup() %>% select(enter_year, fips,
                       `a) Avg. Income (Ths.)`= incomepercapita,
                       `e) Pop. Density` = pop_density,
                       `e) Share of White Persons` = white,
                       `d) Const. Expenditures (Million)` = const_exp,
                       `b) Poverty Rate` = poverty_percent,
                       `c) Unemployment Rate` = unemp_rate) %>%

  gather(variable, value, -enter_year, -fips) %>%

  group_by(enter_year, variable) %>%

  summarise(avg = round(mean(value, na.rm = T), 4), sd = round(sd(value, na.rm = T),4)) %>%

  gather(stat, value, -enter_year, -variable)

#### Round income and population to zero digits and TI to 2 ####
table = mutate(table, value = case_when(variable == "Construction Expenditures"~ round(value, 2), T ~ round(value, 2)))

#### Add the number of counties and MSA per group ####
nobs = data |> group_by(enter_year) |>
  mutate(enter_year = ifelse(enter_year > 2016, "Never Treated", as.character(enter_year))) %>%
  summarise(`g) Population M.` = round(mean(pop, na.rm = T)/1000000 * length(unique(fips)),2),
            `h) N.Counties` = length(unique(fips)),
            `i) N.Metro Areas` = length(unique(cbsa_id)),
            `j) N.States` = length(unique(state_fips))) %>%
  gather(variable, value, -enter_year) |> mutate(stat = "avg")

#### Bind the table elements together ####
table = rbindlist(list(table, filter(table, enter_year != "Never Treated") %>%
                         group_by(variable, stat) %>%
                         summarise(value = round(mean(value, na.rm = T), 2)) %>%
                         mutate(enter_year = "Avg. Treated")), use.names = T) %>%
  mutate(value = ifelse(grepl("sd", stat), paste0("(", value, ")"), value))


#### Bind the nobs elements together ####
nobs = rbindlist(list(nobs, filter(nobs, enter_year != "Never Treated") %>%
                        group_by(variable, stat) %>%
                        summarise(value = round(sum(value, na.rm = T), 2)) %>%
                        mutate(enter_year = "Avg. Treated")), use.names = T)

#### Add the number of observations to the table with macro controls ####
table = rbindlist(list(table, nobs), use.names = T)

#### Create the table of descriptives ####
latex = mmtable(data = table |> select(-stat), cells = value, table_name = "Descriptives") +
  header_top(enter_year)+  header_left_top(variable); latex

#### Transform the mmtable output into a data frame ####
latex = as.data.frame(latex)
names(latex) <- as.character(unlist(latex[1, ]))
latex <- latex[-1, ]  # Remove the first row

#### Output the data frame with the results as a latex table ####
kable(latex, format = "latex", booktabs = TRUE, row.names = FALSE,
      align = c("l", rep("c", ncol(latex) - 1))) %>%
  kable_styling(latex_options = c("HOLD_position"))

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()


#### ..................................................................... ####
#### Table 2: Environmental conditions across treatment groups ####
#### ..................................................................... ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Load packages ####
library(conflicted)
library(data.table)
library(tidyverse)
library(mmtable2)
library(kableExtra)
library(gt)

#### Solve conflicts ####
conflicts_prefer(dplyr::filter)

#### Load the data set ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")

#### Add the data on ACAG PM25 ####
data = read_rds("03_gen/04_reg/sat_reg_test.rds") %>% select(fips, year, pm25) %>%
  left_join(data, .)

#### Compute the average and standard deviation of key variables ####
table = data %>%

  mutate(enter_year = ifelse(enter_year > 2016, "Never Treated", as.character(enter_year))) %>%

  ungroup() %>% select(enter_year, fips,
                       `a) Avg. AQI`= avg,
                       `b) Max AQI` = max,
                       `c) AQAs` = alert,
                       `d) PM25` = pm25,
                       `e) Average Temperature` = st_tmp,
                       `f) Precipitation` = st_rain,
                       `g) Relative Humidity` = st_rh) %>%

  gather(variable, value, -enter_year, -fips) %>%

  group_by(enter_year, variable) %>%

  summarise(avg = round(mean(value, na.rm = T), 4), sd = round(sd(value, na.rm = T),4)) %>%

  gather(stat, value, -enter_year, -variable)

#### Round income and population to zero digits and TI to 2 ####
table = mutate(table, value = case_when(variable == "Construction Expenditures"~ round(value, 2), T ~ round(value, 2)))

#### Add the number of counties and MSA per group ####
nobs = data |> group_by(enter_year) |>
  mutate(enter_year = ifelse(enter_year > 2016, "Never Treated", as.character(enter_year))) %>%
  summarise(`g) Population M.` = round(mean(pop, na.rm = T)/1000000 * length(unique(fips)),2),
            `h) N.Counties` = length(unique(fips)),
            `i) N.Metro Areas` = length(unique(cbsa_id)),
            `j) N.States` = length(unique(state_fips))) %>%
  gather(variable, value, -enter_year) |> mutate(stat = "avg")

#### Bind the table elements together ####
table = rbindlist(list(table, filter(table, enter_year != "Never Treated") %>%
                         group_by(variable, stat) %>%
                         summarise(value = round(mean(value, na.rm = T), 2)) %>%
                         mutate(enter_year = "Avg. Treated")), use.names = T) %>%
  mutate(value = ifelse(grepl("sd", stat), paste0("(", value, ")"), value))


#### Bind the nobs elements together ####
nobs = rbindlist(list(nobs, filter(nobs, enter_year != "Never Treated") %>%
                        group_by(variable, stat) %>%
                        summarise(value = round(sum(value, na.rm = T), 2)) %>%
                        mutate(enter_year = "Avg. Treated")), use.names = T)

#### Add the number of observations to the table with macro controls ####
table = rbindlist(list(table, nobs), use.names = T)

#### Create the table of descriptives ####
latex = mmtable(data = table |> select(-stat), cells = value, table_name = "Descriptives") +
  header_top(enter_year)+  header_left_top(variable); latex

#### Transform the mmtable output into a data frame ####
latex = as.data.frame(latex)
names(latex) <- as.character(unlist(latex[1, ]))
latex <- latex[-1, ]  # Remove the first row

#### Output the data frame with the results as a latex table ####
kable(latex, format = "latex", booktabs = TRUE, row.names = FALSE,
      align = c("l", rep("c", ncol(latex) - 1))) %>%
  kable_styling(latex_options = c("HOLD_position"))

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()


## Debugging code removed (leftover from development)
## head(table)
## plot = filter(table, variable %in% c("g) Population M."), enter_year <= 2016)
## plot = mutate(plot, sum = cumsum(value))
## ggplot(plot) +
##   geom_line(aes(x = enter_year, y = sum, group = 1))
## unique(table$variable)

#### ____________________________________________________________________ ####
#### Figure 2: Pre-treatment AQI for each treatment cohort ####
#### _____________________________________________________________________####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Add the directory paths ####
macdir <- paste0(figures_dir, "/")

#### Add the packages ####
library(conflicted)
library(data.table)
library(tidyverse)
library(ggplot2)

#### Solve package conflicts ####
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
    filter(count == 6) %>% as.data.frame(.)) %>%
  rbindlist(.)

#### Plot the common trends for the SDiD design ####
plot = data |>

  group_by(treated, tg, score, var) |>

  summarise(`value` = mean(value, na.rm = T)) %>%

  mutate(treated = ifelse(treated == 1, "Treated", "Not Treated")) %>%

  ungroup()


#### Add the facet ID ####
plot$tg = paste("Treatment Year:", plot$tg)

#### Make the ggplot by treatment year ####
ggplot(plot %>% filter(var == "avg")) +
  geom_line(aes(x = score, y = value, color = treated, group = treated)) +
  facet_wrap(~tg, scales = "free")+
  geom_vline(aes(xintercept = -0.5), linetype = "dashed") +

  geom_point(data = filter(plot, var == "avg",
                           score %in% c(max(score), min(score))),
             aes(x = score, y = value, color = treated, group = treated, shape = treated),
             size = 2.25) +

  scale_color_manual(values = c("Treated" =  "orange",
                                "Not Treated" =  "#49326b")) +

  labs(x = "Years to Treatment", y = expression(paste("Average AQI"))) +

  theme(plot.background = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "italic"),
        legend.title = element_blank(),
        legend.position = c(0.4, 0.125),
        legend.margin = margin(t = -10),
        legend.key = element_rect(fill = "transparent", color = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        axis.line.x = element_line(),
        panel.grid = element_blank())

#### Save the plot ####
ggsave(paste0(macdir, "common_trends_tg.png"), width = 8, height = 5)

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### ____________________________________________________________________ ####
#### Figure 3: Dynamic estimates for the effect of Uber on the AQI ####
#### _____________________________________________________________________####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Add the directory paths ####
macdir <- paste0(figures_dir, "/")

#### Install packages #####
library(conflicted)
library(data.table)
library(tidyverse)

#### Load mortality data ####
est = list(TWFE = read_rds("03_gen/05_results/sdid_twfe_dynamic.rds"),
           `E-TWFE` = read_rds("03_gen/05_results/sdid_etwfe_dynamic.rds"),
           `SA-DD` = read_rds("03_gen/05_results/sdid_sadd_dynamic.rds")) %>%
  rbindlist(., idcol = "estimator", use.names = T)


est = est %>% filter(score %in% c(-1:0) | estimator != "E-TWFE")
#### Plot the point estimates ####
ggplot(est) +
  geom_errorbar(data = est,
                aes(ymax = estimate + std.error*1.95,
                    ymin = estimate - std.error*1.95,
                    x = as.numeric(score), color = estimator),
                width = 0.25, position = position_dodge(width = 0.35)) +

  geom_point(aes(y = estimate, x = as.numeric(score), color = estimator),
             size = 1.50, position = position_dodge(width = 0.35)) +

  scale_color_manual(values = c("E-TWFE" = "black",
                                "SA-DD" = "#1c4966",
                                "TWFE"= "orange")) +
  labs(y = "Estimate and 95% CIs", x = "Years to Treatment") +
  geom_vline(aes(xintercept = -0.5), linetype = "dashed") +
  geom_hline(aes(yintercept = 0)) +

  theme(panel.background = element_blank(),
        strip.background = element_blank(),
        plot.background = element_blank(),
        axis.line.x = element_line(),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key = element_rect(fill = "transparent", color = "transparent"),
        panel.grid = element_blank()) +
  ggpubr::grids("y", linetype = "dashed")

#### Save the plot ####
ggsave(paste0(macdir, "sdid_dynamic.png"), width = 6, height = 3.5)

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()
#### ..................................................................... ####
#### Estimates across the distribution ####
#### ..................................................................... ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Add the directory paths ####
macdir <- paste0(figures_dir, "/")
mac_eth <- paste0(figures_dir, "/")

#### Load packages ####
library(conflicted)
library(tidyverse)
library(mmtable2)
library(data.table)
library(kableExtra)
library(texreg)

#### solve conflicts ####
conflict_prefer("year", "lubridate")
conflict_prefer("filter", "dplyr")
conflict_prefer("month", "lubridate")
conflict_prefer("isoweek", "lubridate")

#### Load the estimates ###
est = list(`TWFE`  = read_rds("03_gen/05_results/twfe.rds"),
           `E-TWFE`  = read_rds("03_gen/05_results/etwfe.rds"),
           `SADD`  = read_rds("03_gen/05_results/sadd.rds"),
           `SDiD-TWFE` = read_rds("03_gen/05_results/sdid.rds"),
           `SDiD-ETWFE` = read_rds("03_gen/05_results/sdid_etwfe.rds"),
           `SDiD-SADD` = read_rds("03_gen/05_results/sdid_sadd.rds"))

#### Put all estimates together ####
est = rbindlist(est, idcol = "estimator", use.names = T, fill = T)
est = rename(est, measure = var) %>% mutate(BIC = BIC/1000)

#### Change the name of the facets ####
est$measure = gsub("25th", "25th Percentile", est$measure)
est$measure = gsub("50th", "50th Percentile", est$measure)
est$measure = gsub("75th", "75th Percentile", est$measure)
est$measure = gsub("avg", "Average AQI", est$measure)
est$measure = gsub("max", "Maximum AQI", est$measure)

#### Organize the estimators ####
est$estimator = factor(est$estimator, levels = c("SDiD-SADD", "SDiD-ETWFE",
                                                 "SDiD-TWFE", "SADD", "E-TWFE",
                                                 "TWFE"))

#### Organize the facets ####
est$measure = factor(est$measure, levels = c("Average AQI",
                                             "25th Percentile", "50th Percentile",
                                             "75th Percentile", "Maximum AQI"))
#### Determine the percentage change ####
est = mutate(est, change = 100*(estimate/pt_value))

#### Plot the coefficients for all estimators ####
ggplot(est %>% filter(spec == "(2)", measure != "min")) +
  geom_errorbar(aes(y = estimator, xmax = estimate + std.error*1.645,
                    xmin = estimate - std.error*1.645),
                width = 0.5, color = "black") +

  geom_point(aes(y = estimator, x = estimate, shape = estimator ), color = "black") +

  facet_wrap(~measure, scales = "free_x", ncol = 5) +
  geom_vline(aes(xintercept = 0), color = "orange", linetype = "dashed") +
  guides(color = "none", shape = "none") +
  theme(axis.line.x = element_line(),
        axis.text.y = element_text(hjust = 0),
        legend.title = element_blank(),
        legend.key = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "italic", size = 9),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.y = element_blank()) +

  labs(y = "", x = "Point Estimate and 90% CIs")

#### Save the plot ####
ggsave(paste0(mac_eth, "est_dist.png"), width = 9, height = 3)

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

### ____________________________________________________________________ ####
#### Figure 5: TNCs in New York City####
#### ____________________________________________________________________ ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores")))

#### Add the directory paths ####
macdir <- paste0(figures_dir, "/")

#### Load the packages ####
library(conflicted)
library(data.table)
library(tidyverse)
library(ggthemes)

#### Solve the conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data on ride hailing use frequency ####
freq = list(`NYC-MS 2019` = read_rds("03_gen/nycmb_freq_2019.rds"),
            `NYC-MS 2018` = read_rds("03_gen/nycmb_freq_2018.rds"),
            `NYC-MS 2017`= read_rds("03_gen/nycmb_freq_2017.rds")) %>%
  rbindlist(., idcol = "Survey")

#### Load the data on the substitution of travel modes ####
subst = list(`NYC-MS 2019` = read_rds("03_gen/nycmb_subst_2019.rds"),
             `NYC-MS 2017`= read_rds("03_gen/nycmb_subst_2017.rds")) %>%
  rbindlist(., idcol = "Survey")

#### Bind both data sets together ###
data = list(freq = select(freq, survey = Survey, var = freq, value = share),
            subst = select(subst, survey = Survey, var = subst, value = share)) %>%
  rbindlist(., idcol = "group")

#### Round the values ####
data = mutate(data, value = round(value*100, 1))

#### Change the title of the facets ####
data$group = gsub("freq", "How often de you use\nride-hailing apps like Uber?", data$group)
data$group = gsub("subst", "Before shifting to ride-hailing\nhow did you travel?", data$group)

#### Only keep the survey year ####
data$survey = gsub("NYC-MS ", "", data$survey)

#### Organize the facets ####
data$group = factor(data$group, levels = unique(data$group))

#### Aggregate to total population ####
ggplot(data) +
  geom_bar(aes(y = reorder(var, -value), x = value,
               fill = survey), width = 0.1, stat = "identity",
           position = position_dodge(width = 0.5)) +
  facet_wrap(~group, scales ="free") +

  geom_point(aes(y = reorder(var, -value), color = survey,
                 x = value, group = survey, shape = survey),
             position = position_dodge(width = 0.5),
             size = 2) +

  geom_text(aes(y = reorder(var, -value), x = value, group = survey,
                label = paste0(value, "%")),
            position = position_dodge(width = 0.5), size = 2.5,
            hjust = -0.25, vjust = 0.25) +

  labs(x = "Share of Answers", y = "", title = "") +
  xlim(0,80) +
  scale_fill_manual(values = c("black",  "#1c4966", "orange")) +
  scale_color_manual(values = c("black", "#1c4966", "orange")) +

  theme(plot.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "italic") ,
        panel.background = element_blank(),
        legend.background = element_blank(),
        axis.line.x = element_line(),
        strip.background = element_blank(),
        legend.title = element_blank(),
        legend.margin = margin(t = -15, b = 0))

#### Save the plots ####
ggsave(paste0(macdir, "nycms_freq.png"),width = 7, height = 3.5)

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()



#### _____________________________________________________________________ ####
#### Figure 6: Fuel efficiency, gasoline costs, and range of Toyota Corolla models 2005-2017####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Load the packages ####
library(NatParksPalettes)
library(conflicted)
library(data.table)
library(tidyverse)
library(fixest)
library(readxl)
library(broom)

#### Add the directory paths ####
macdir <- paste0(figures_dir, "/")

#### Clear the space ####
conflict_prefer("filter", "dplyr")

#### Load the regressions data ####
data <- read_excel("02_data/camry_epa.xlsx")

#### Filter the data to the preferred specification ####
data = mutate(data, Model = paste(Model, `Engine Size`,
                                  `Cylinders`, `Fuel Type`))

#### Construct data for average efficiency ####
plot = data %>% group_by(year = `Model-Year`) %>%
  summarise(`Miles per Gallon` = mean(`City MPG`),
            `Gas Cost ($)` = mean(`Annual Gas Cost ($)`),
            `Range Miles` = mean(`Range (miles)`)) %>%
  gather(., var, value, -year)

plot$var = factor(plot$var, levels = unique(plot$var))

#### Make the plot for the average ####
ggplot(plot %>% filter(year %in% c(2005:2016))) +
  geom_line(aes(x = year, y = value, group = 1, color = var)) +
  geom_point(aes(x = year, y = value, group = 1, color = var)) +
  facet_wrap(~var, scales = "free")+
  scale_color_manual(values = c("orange", "navy",  "#49326b")) +
  #scale_color_manual(values = c("orange", "#49326b", "navy")) +
  guides(color = "none") + labs(y = "", x = "") +

  theme(plot.background = element_blank(),
        panel.background = element_blank(),
        legend.position.inside = c(0.925, 0.8),
        strip.text = element_text(hjust = 0),
        legend.position = "bottom",
        strip.background = element_blank(),
        legend.title = element_blank(),
        axis.line.x = element_line(),
        legend.text = element_text(size = 9),
        panel.grid = element_blank())

#### Save the plot for CAMRY fuel efficiency ####
ggsave(file = paste0(macdir, "camry_fuel_efficiency.png"), width = 7, height = 2.5)

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### _____________________________________________________________________ ####
#### Figure 7: Effects on the share of gasoline and clean vehicles in California ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Add the directory paths ####
macdir <- paste0(figures_dir, "/")

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

#### Extract the state and only keep California ####
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
est = list(`Hybrid/Electric` = feols(clean   ~ sunab(enter_year, year)|year + fips,
                                     data = test, cluster = ~cbsa_id,
                                     weights = ~pop),

           `Gasoline` = feols(Gasoline   ~ sunab(enter_year, year)|year + fips,
                              data = test, cluster = ~cbsa_id,
                              weights = ~pop))



#### Extract the fitted statistics ####
sum = lapply(est, function(x)
  x = data.frame(tidy(x), N.Obs = nobs(x), R2 = r2(x)[2])) %>%
  rbindlist(., idcol = "spec") %>%
  mutate(e = gsub(".*:", "", term) %>% as.numeric()) %>%
  filter(e >= -5)


####
#### Plot the point estimates ####
ggplot(sum) +
  geom_ribbon( aes(ymax = estimate + std.error*1.975,
                   ymin = estimate - std.error*1.975,
                   x = as.numeric(e), fill = spec),
               width = 0.25, position = position_dodge(width = 0.35),
               alpha = 0.5) +

  geom_line( aes(y = estimate,
                 x = as.numeric(e), color = spec),
             linetype = "dashed") +

  #geom_point(aes(y = estimate, x = as.numeric(e), color = spec),
  #          size = 1.50, position = position_dodge(width = 0.35)) +

  scale_fill_manual(values = c("Hybrid/Electric" = "#1c4966",
                               "Gasoline"= "orange")) +

  scale_color_manual(values = c("Hybrid/Electric" = "#1c4966",
                                "Gasoline"= "orange")) +
  labs(y = "Estimate and 95% CIs", x = "Years to Treatment") +
  geom_vline(aes(xintercept = -1), linetype = "dashed") +
  geom_hline(aes(yintercept = 0)) +

  theme(panel.background = element_blank(),
        strip.background = element_blank(),
        plot.background = element_blank(),
        axis.line.x = element_line(),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key = element_rect(fill = "transparent", color = "transparent"),
        panel.grid = element_blank()) +
  ggpubr::grids("y", linetype = "dashed")

#### Save the plot ####
ggsave(paste0(macdir, "share_gas_dynamic.png"), width = 8, height = 4.5)

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()
