#### ##################################################################### ####
####                        Figures in the appendix                          ####
####        (Replication Package Version)                                    ####
#### ##################################################################### ####
## Path set by 00_master.R; if running standalone, set root manually
if (!exists("root")) root <- getwd()
setwd(root)
figures_dir <- file.path(root, "output", "figures")

#### ____________________________________________________________________ ####
#### Figure C1 -- Diffusion of Uber ####
#### ____________________________________________________________________ ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Add the directory paths ####
macdir <- paste0(figures_dir, "/")

#### Load packages ####
library(conflicted)
library(data.table)
library(tidyverse)
library(ggplot2)

#### Solve conflicts ####
conflicts_prefer(dplyr::filter)

#### Load the data set ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")

#### Add the number of counties and MSA per group ####
plot = data |> group_by(enter_year) |>
  mutate(enter_year = ifelse(enter_year > 2016, "Never Treated", as.character(enter_year))) %>%
  summarise(`Population (Millions)` = round(mean(pop, na.rm = T)/1000000 * length(unique(fips)),2),
            `Number of Counties` = length(unique(fips)),
            `Number of Metro Areas` = length(unique(cbsa_id))) %>%
  gather(variable, value, -enter_year) |> mutate(stat = "avg")

#### Only keep the treated years ####
plot = filter(plot, enter_year <= 2016) %>% group_by(variable) %>%
  mutate(sum = cumsum(value)) %>% mutate(enter_year = as.numeric(enter_year))

#### Organize the facets ####
plot$variable = factor(plot$variable, levels = c("Population (Millions)",
                                                 "Number of Counties",
                                                 "Number of Metro Areas"))

#### Organize the facets ####
ggplot(plot) +
  geom_line(aes(x = enter_year, y = sum, group = 1)) +
  geom_point(aes(x = enter_year, y = sum, group = 1), color = "orange") +
  facet_wrap(~variable, scales = "free") +
  labs(y = "", x = "Year") +
  theme(axis.line.x = element_line(),
        axis.text.y = element_text(hjust = 0),
        legend.title = element_blank(),
        legend.key = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "italic", size = 9),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.y = element_blank()) +
  ggpubr::grids("y")

#### Save the plots ####
ggsave(paste0(macdir, "uber_difusion.png"),width = 7, height = 3)

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()
#### ____________________________________________________________________ ####
#### Figure C2: Air quality across the United States ####
#### ____________________________________________________________________ ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Add the directory paths ####
macdir <- paste0(figures_dir, "/")

#### Add packages ####
library(NatParksPalettes)
library(colorspace)
library(conflicted)
library(ggspatial)
library(patchwork)
library(tidyverse)
library(ggplot2)
library(sf)

#### Load the data set ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")
counties = st_read("02_data/05_shp/03_counties", quiet = T)

#### Mean AQI per US county ####
plot = data %>%

  group_by(fips) %>%

  summarise(avg = round(mean(avg, na.rm = T), 4))

#### Restrict the states to continental US ####
counties = filter(counties, !(STATEFP  %in% c("02", "15", "72")))

#### Spatial distribution of treated and control units in the US  ####
plot = mutate(counties, fips = paste0(STATEFP, COUNTYFP)) %>%
  left_join(select(ungroup(plot), fips, avg))

#### Define the color palette ####
colors <- natparks.pals("Acadia", n = 30)
swatchplot(colors)
texture <- grDevices::colorRampPalette(c(colors[17:30],"brown", "darkred", "#560216", "black"))(256)
swatchplot(texture)

#### Plot the average AQI ####
avg_aqi = ggplot() +
  geom_sf(data = plot, lwd = 0.01, aes(fill = avg),
          color = alpha("gray", 0.5)) +

  scale_fill_gradientn(colours = texture, na.value = "gray80") +
  labs(x = "", y = "")  +

  theme(panel.background = element_rect(fill = "white"),
        legend.background = element_blank(),
        legend.title = element_text(face = "italic", size = 8, hjust = 0)) +

  guides(fill = guide_colorbar(title.position = "top",
                               title = "Average AQI\n2005-2016")) +

  annotation_north_arrow(location = "br", which_north = "false",
                         height = unit(1, "cm"), width = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering(fill = c("black", "white"))) +

  annotation_scale(bar_cols = c("black", "white"),
                   height = unit(0.15, "cm"), location = "bl",
                   text_cex = 1);avg_aqi

#### Save the plot ####
ggsave(file = paste0(macdir, "map_aqi.pdf"), width = 10, height = 5)

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()
#### ____________________________________________________________________ ####
#### Figure C3: Changes in air quality across the United States ####
#### ____________________________________________________________________ ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Add the directory paths ####
macdir <- paste0(figures_dir, "/")

#### Add packages ####
library(NatParksPalettes)
library(colorspace)
library(conflicted)
library(ggspatial)
library(patchwork)
library(tidyverse)
library(ggplot2)
library(sf)

#### Load the data set ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")
counties = st_read("02_data/05_shp/03_counties", quiet = T)

#### Deviations between 2010 and 2017 in the AQI by county ####
plot = filter(data, year %in% c(2010, 2016)) |>
  group_by(fips, year) |>
  summarise(avg = round(mean(avg, na.rm = T))) %>%
  spread(year, avg) |> mutate(diff =`2010` - `2016`) |>
  select(fips, diff)

#### Restrict the states to continental US ####
counties = filter(counties, !(STATEFP  %in% c("02", "15", "72")))

#### Spatial distribution of treated and control units in the US  ####
plot = mutate(counties, fips = paste0(STATEFP, COUNTYFP)) %>%
  left_join(select(ungroup(plot), fips, diff))

#### Color Palette for the change in the AQI ####
colors <- natparks.pals("Acadia", n = 30)
swatchplot(colors)
texture <- grDevices::colorRampPalette(c(colors[0:12], colors[16:30]))(256)
swatchplot(texture)

#### Make the plot ####
ggplot() +
  geom_sf(data = plot, lwd = 0.01, aes(fill = diff),
          color = alpha("gray", 0.5)) +

  scale_fill_gradientn(colours = texture, na.value = "gray80") +
  labs(x = "", y = "")  +

  theme(panel.background = element_rect(fill = "white"),
        legend.background = element_blank(),
        legend.title = element_text(face = "italic", size = 8, hjust = 0)) +

  guides(fill = guide_colorbar(title.position = "top",
                               title = "Change in the AQI\n2005-2016")) +

  annotation_north_arrow(location = "br", which_north = "false",
                         height = unit(1, "cm"), width = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering(fill = c("black", "white"))) +

  annotation_scale(bar_cols = c("black", "white"),
                   height = unit(0.15, "cm"), location = "bl",
                   text_cex = 1)


#### Save the plot ####
ggsave(file = paste0(macdir, "map_aqi_diff.pdf"), width = 10, height = 5)

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()
#### _____________________________________________________________________ ####
#### Figure C4: Annual enter week of Uber across all metropolitan areas ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Add the directory paths ####
macdir <- paste0(figures_dir, "/")

#### Load packages ####
library(NatParksPalettes)
library(colorspace)
library(conflicted)
library(tidyverse)
library(lubridate)
library(readxl)
library(zoo)

#### solve conflicts ####
conflict_prefer("year", "lubridate")
conflict_prefer("filter", "dplyr")
conflict_prefer("month", "lubridate")
conflict_prefer("isoweek", "lubridate")

#### Load the estimates ####
uber = read_excel("02_data/06_other/4_Uber_entry_dates/uber_dates_addition.xlsx", sheet = "UberDatesUS")

#### Exclude counties outside the continental USA ####
uber = filter(uber, !(state %in% c("AK", "HI")))

#### Add date identifiers ####
uber = mutate(uber, year = year(enter_date_official), month = month(enter_date_official),
              week = isoweek(enter_date_official), year_month = as.yearmon(enter_date_official))

#### Only keep data with enter year below 2017 ####
uber = filter(uber, enter_date_official < as.Date("2016-12-31"))

#### Set the fill gradient ####
colors <- natparks.pals("Acadia", n = 30)
swatchplot(colors)
texture <- grDevices::colorRampPalette(c(colors[0:12], colors[20:30]))(256)
swatchplot(texture)

#### Select the relevant columns ####
plot = select(uber, type, city, enter_date = enter_date_official, year, month, week, year_month)

#### Aggregate to the year-month level ####
agg = plot %>% filter(type %in% c("UberX")) %>%
  filter(year <= 2016) %>%
  group_by(week) %>% summarise(count = n())

#### Plot the time series of Uber X and Uber Black at the Year-Month level ####
ggplot(agg) +
  geom_bar(aes(x = week, y = count, fill = count), stat = "identity",
           position = "dodge") +

  guides(fill = "none") +
  scale_x_continuous(breaks = seq(0,52,4)) +
  scale_fill_gradientn(colours = texture, na.value = "gray80") +

  theme(plot.background = element_blank(),
        panel.background = element_blank(),
        legend.position.inside = c(0.925, 0.8),
        strip.text = element_text(hjust = 0),
        strip.background = element_blank(),
        legend.title = element_blank(),
        axis.line.x = element_line(),
        legend.text = element_text(size = 9),
        panel.grid = element_blank()) +
  labs(x = "Week of the Year", y = "Number of MSAs") +
  ggpubr::grids(axis = "y")

#### Save the figure ####
ggsave(file = paste0(macdir, "week_enter_date.png"), width = 7, height = 3.5)


#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()
#### ____________________________________________________________________ ####
#### Figure E1: Goodman-Bacon decomposition on the effects of Uber on the AQI ####
#### ____________________________________________________________________ ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Add the directory paths ####
macdir <- paste0(figures_dir, "/")

#### Load packages ####
library(bacondecomp)
library(conflicted)
library(data.table)
library(tidyverse)
library(ggplot2)
library(lfe)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")

#### Make panel balanced ####
data = ungroup(data) %>% filter(!is.na(avg)) %>% group_by(fips) %>%
  mutate(length = length(unique(year))) %>% filter(length == 13)

#### Compute the Goodman bacon decomposition ####
dd = felm(avg ~ treatment | fips + year | 0 | fips, data = data)

#### Goodman-Bacon decomposition without covariates ####
data = select(data, -treated)
df_bacon = bacon(avg ~ treatment, data = data, id_var = "fips", time_var = "year")

#### Estimate the coefficients of the bacon decomposition for each type of estimate ####
coef_bacon = sum(df_bacon$estimate * df_bacon$weight)

#### Create a data frame of all the estimates to plot it ####
df_bacon = df_bacon %>% group_by(type) %>%
  mutate(sum_w = sum(weight),
         `Group Est.` =  weighted.mean(estimate, weight)) |>
  mutate(`Average Est.` = coef_bacon)

#### Transform the plot from wide to long format #####
df_bacon = gather(df_bacon, coef, value, -c(treated:sum_w))
df_bacon$treated = factor(df_bacon$treated)

#### Plot the estimates of the Goodman Bacon decomposition ####
ggplot(df_bacon) +
  aes(x = weight, y = estimate) +
  labs(x = "Weights", y = "Estimate") +
  geom_point(size = 2, alpha = 0.75, color = "gray60", aes(shape = treated)) +
  geom_hline(aes(yintercept = value, color = coef)) +
  scale_shape_manual(values = seq(12,20, 1))+

  facet_wrap(~type, nrow = 1, scales = "free_x") +
  scale_color_manual(values = c("orange", "#49326b")) +

  theme(plot.background = element_blank(),
        panel.background = element_blank(),
        legend.position.inside = c(0.925, 0.8),
        strip.text = element_text(hjust = 0),
        strip.background = element_blank(),
        legend.title = element_blank(),
        axis.line.x = element_line(),
        legend.text = element_text(size = 9),
        panel.grid = element_blank()) +

  scale_x_continuous(breaks = scales::pretty_breaks(n = 3))

#### Save the plot ####
ggsave(file = paste0(macdir, "BaconDecomposition.png"), width = 7, height = 3.5)

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()
#### _____________________________________________________________________ ####
#### Figure E2: Relative weights of each cohort when estimating the ATT with different estimators ####
#### _____________________________________________________________________ ####
#### clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Add the directory paths ####
macdir <- paste0(figures_dir, "/")

#### Load the packages ####
library(tidyverse)
library(ggplot2)

#### Load the data ####
twfe = read_rds("03_gen/09_revision/twfe_cohort_weights.rds")
etwfe = read_rds("03_gen/09_revision/etwfe_cohort_weights.rds")
sadd = read_rds("03_gen/09_revision/sadd_cohort_weights.rds")

#### Select the relevant columns of each data set ####
twfe = select(twfe, cohort = cohort_treated, weights = sum_of_weights) %>%
  group_by(cohort) %>%
  summarise(weights = sum(weights))
etwfe = select(etwfe, cohort = gvar, weights = weight_g)
sadd = select(sadd, cohort = cohort, weights = w_ge) %>% filter(cohort != 2017)

#### Aggregate the data ####
plot = bind_rows(list("TWFE" = twfe,
                      "E-TWFE" = etwfe,
                      "SA-DD" = sadd), .id = "estimator")

plot$estimator = factor(plot$estimator, levels = c("TWFE", "E-TWFE", "SA-DD"))
#### Plot the estimates ####
ggplot(plot) +

  geom_bar(aes(x = cohort, y = weights, fill = estimator, group = estimator),
           stat = "identity", position = position_dodge(width = 0.5), width = 0.1) +

  geom_point(aes(x = cohort, y = weights, color = estimator, group = estimator),
             stat = "identity", position = position_dodge(width = 0.5)) +


  scale_shape_manual(values = seq(12,20, 1))+

  #facet_wrap(~type, nrow = 1, scales = "free_x") +
  scale_fill_manual(values = c("orange", "#49326b", "darkgreen")) +
  scale_color_manual(values = c("orange", "#49326b", "darkgreen")) +
  theme(plot.background = element_blank(),
        panel.background = element_blank(),
        legend.position.inside = c(0.925, 0.8),
        strip.text = element_text(hjust = 0),
        strip.background = element_blank(),
        legend.title = element_blank(),
        axis.line.x = element_line(),
        legend.text = element_text(size = 9),
        panel.grid = element_blank()) +
  labs(x = "Cohort (g)", y = "Weights") +

  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

#### Save the plot ####
ggsave(file = paste0(macdir, "estimator_weights.png"), width = 7, height = 3.5)

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()
#### _____________________________________________________________________ ####
#### Figure G1: Effect of Uber on the AQI for different sample restrictions ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Add the directory paths ####
macdir <- paste0(figures_dir, "/")

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
data = filter(data, var == "avg")

#### Run the regression across all dependent variables and models #
up_trace = c(0:5, 10)
back_trace = c(-10:-5, -1)

#### Run the up-trace estimates ####
est_up = lapply(up_trace, function(x)
  feols(value ~ treatment | year + fips,
        data = data%>% filter(score %in% seq(-5, x, 1) | treated == 0 ), cluster = "cbsa_id"))

names(est_up) = up_trace

#### Run the up-trace estimates ####
est_back = lapply(back_trace, function(x)
  feols(value ~ treatment | year + fips,
        data = data%>% filter(score %in% seq(x, 5, 1) | treated == 0 ), cluster = "cbsa_id"))

names(est_back) = back_trace

#### Run the up-trace estimates ####
est_main = list(feols(value ~ treatment | year + fips, data = data, cluster = "cbsa_id"))

names(est_main) = "unrestricted"

#### Summarize the coefficients ####
sum = lapply(list("Pre-treat rest." = est_back, "Post-treat rest." = est_up,  "Unrestricted" = est_main), function(x) lapply(x, function(x)
  x = data.frame(tidy(x), N.Obs = x$nobs,
                 R2 = r2(x)[2],
                 BIC = BIC(x),
                 N.Counties = x$fixef_sizes[2],
                 N.Periods = x$fixef_sizes[1]))) %>%

  lapply(., rbindlist, idcol = "spec") %>%

  rbindlist(., idcol = "var") %>%

  filter(term == "treatment")

#### Create the restriction labels ####
sum <- sum %>%
  mutate(
    rest = case_when(
      var == "Post-treat rest." ~ paste0("e %in% '(-5, ", spec, ")'"),
      var == "Pre-treat rest." ~ paste0("e %in% '(", spec, ", 5)'"),
      TRUE ~ "\"Unrestricted\""))

#### Organize the restrictions ####
sum$rest = factor(sum$rest, levels = unique(sum$rest))
sum = filter(sum, var == "Pre-treat rest." | rest != "e %in% '(-5, 5)'")

#### Plot the estimates ####
ggplot(sum %>%filter(rest != "e %in% '(-1, 5)'"),
       aes(x = reorder(rest, spec))) +
  geom_errorbar(aes(ymax = estimate + std.error * 1.975,
                    ymin = estimate - std.error * 1.975,
                    color = var),
                width = 0.25,
                position = position_dodge(width = 0.35)) +
  scale_x_discrete(labels = function(x) parse(text = x)) +

  scale_color_manual(values = c("#1c4966", "orange", "purple")) +

  labs(y = "Estimate and 95% CIs", x = "Restriction") +
 # geom_vline(aes(xintercept = -1), linetype = "dashed") +
  geom_hline(aes(yintercept = 0)) +

  theme(panel.background = element_blank(),
        strip.background = element_blank(),
        plot.background = element_blank(),
        axis.line.x = element_line(),
        axis.text.x = element_text(angle = 90),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key = element_rect(fill = "transparent", color = "transparent"),
        panel.grid = element_blank()) +
  ggpubr::grids("y", linetype = "dashed")


#### Save the plot ####
ggsave(paste0(macdir, "restricted_robust.png"), width = 8, height = 4.5)

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### ____________________________________________________________________ ####
#### Figure G2: Pretreatment AQI for each treatment cohort with SDiD ####
#### _____________________________________________________________________####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Add the directory paths ####
macdir <- paste0(figures_dir, "/")

#### Add the packages ####
library(conflicted)
library(data.table)
library(tidyverse)
library(synthdid)
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

#### Plot the common trends for the SDiD design ####
plot = data |>

  mutate(tw = ifelse(score >= 0, NA, tw)) %>%
  mutate(uw = ifelse(is.na(uw) == T, 0, uw))%>%

  group_by(treated, tg, score, var) |>

  summarise(`Synthetic DiD` = weighted.mean(value, uw, na.rm = T),
            `Standard DiD` = mean(value, na.rm = T),
            tw = mean(tw, na.rm = T)) %>%

  gather(., agg, value, -c(treated, tg, tw, score, var)) %>%

  mutate(treated = ifelse(treated == 1, "Treated", "Controls")) |>
  mutate(tw = ifelse(var == "Standard DiD", 0, tw)) |>

  mutate(tw = ifelse(score > 0, NA, tw))

#### Re-scale the time weights to the smallest of the synthetic values ####
plot = plot |> group_by(var, tg) |>
  mutate(tw2 = {
    vmin <- min(tw, na.rm = TRUE); vmax <- max(tw, na.rm = TRUE)
    nmin <- min(value, na.rm = TRUE) - 1; nmax <- min(value, na.rm = TRUE) + 2.5
    ifelse(is.na(tw), tw, nmin + (tw - vmin) / (vmax - vmin) * (nmax - nmin))
  }) |>
  mutate(min = min(value, na.rm = T) - 1)

#### Simplify the plot to only one facet ####
plot = filter(plot, treated == "Controls" | agg == "Synthetic DiD" & treated == "Treated") %>%
  mutate(agg = ifelse(treated == "Treated", "Treated", agg))

#### Organize the facets ####
plot = mutate(plot, tw2 = ifelse(agg == "Treated", NA, tw2))
plot$agg = factor(plot$agg, levels = c("Standard DiD", "Treated", "Synthetic DiD"))

#### Add the facet ID ####
plot$tg = paste("Treatment Year:", plot$tg)

#### Make the ggplot by treatment year ####
ggplot(plot %>% filter(var == "avg")) +
  geom_line(aes(x = score, y = value, color = agg, group = agg)) +
  facet_wrap(~tg, scales = "free")+
  geom_vline(aes(xintercept = -0.5), linetype = "dashed") +

  geom_pointrange(aes(x = score, y = tw2, ymax = tw2, ymin = min), color = "black",
                  size = 0.1, fatten = -20.5) +

  geom_point(data = filter(plot, var == "avg", score %in%c(max(score), min(score))),
             aes(x = score, y = value, color = agg, group = agg, shape = agg),
             size = 2.5) +

  scale_color_manual(values = c("Treated" =  "orange",
                                "Standard DiD" = "gray",
                                "Synthetic DiD" =  "#49326b")) +

  labs(x = "Score", y = expression(paste("Average AQI"))) +

  theme(plot.background = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "italic"),
        legend.title = element_blank(),
        legend.position = c(0.45, 0.125),
        legend.margin = margin(t = -10),
        legend.key = element_rect(fill = "transparent", color = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        axis.line.x = element_line(),
        panel.grid = element_blank())

#### Save the plot ####
ggsave(paste0(macdir, "common_trends_tg_sdid.png"), width = 8, height = 5)

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()
#### _____________________________________________________________________ ####
#### Figure G3: Short-term change in the distribution of the AQI in treated counties ####
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

#### Solve conflicts ####
conflicts_prefer(lubridate::year)

#### Load the regressions data ####
data = read_rds("03_gen/04_reg/daily_reg.rds")

#### Add the year and enter_year ####
data = mutate(data, year = year(date), enter_year = year(enter_date))

#### AQI Distribution e = -1 and e = 0.
plot = mutate(data, score = year - enter_year) %>%
  filter(enter_year <= 2016) %>% filter(score %in% c(-1, 0, 1))%>%
  group_by(score) %>% summarise(avg = mean(aqi, na.rm = T),
                                `25th` = quantile(aqi, p = 0.25, na.rm = T),
                                `50th` = quantile(aqi, p = 0.50, na.rm = T),
                                `75th` = quantile(aqi, p = 0.75, na.rm = T))

#### Transform to long format ####
plot = plot %>% gather(var, value, -score)

#### Change the name of the variables ####
plot$var = gsub("25th", "25th Percentile", plot$var)
plot$var = gsub("50th", "50th Percentile", plot$var)
plot$var = gsub("75th", "75th Percentile", plot$var)
plot$var = gsub("avg", "Average AQI", plot$var)

#### Transform the score to a character variable ####
plot$score = as.character(plot$score)

#### Organice the facets ####
plot$var = factor(plot$var, levels = c("25th Percentile", "50th Percentile",
                                       "75th Percentile",
                                       "Average AQI"))

#### Make the plot ####
ggplot(plot) +
  geom_line(aes(x = score, y = value, group = 1)) +
  geom_point(aes(x = score, y = value)) +
  facet_wrap(~var, scales = "free", ncol = 4) +
  geom_vline(aes(xintercept = 1.5), color = "orange", linetype = "dashed") +
  guides(color = "none", shape = "none") +
  theme(axis.line.x = element_line(),
        legend.title = element_blank(),
        legend.key = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "italic", size = 9),
        panel.background = element_blank(),
        panel.grid = element_blank()) +

  labs(y = "", x = "Years to Treatment")

#### Save the plot ####
ggsave(paste0(macdir, "dist_short_ts.png"), width = 7, height = 2.5)

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()
#### _____________________________________________________________________ ####
#### Figure G4: Density distribution of the AQI for e ∈ (−1, 0, 1) for treated counties ####
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

#### Load the regressions data ####
data = read_rds("03_gen/04_reg/daily_reg.rds")

#### Add the year and enter_year ####
data = mutate(data, year = year(date), enter_year = year(enter_date))

#### AQI Distribution e = -1 and e = 0. ####
plot = mutate(data, score = year - enter_year) %>%
  filter(enter_year <= 2016) %>% filter(score %in% c(-1, 0, 1))%>%
  group_by(score) %>% mutate(avg = mean(aqi, na.rm = T)) %>%
  filter(aqi > quantile(aqi, p = 0.025)) %>%
  filter(aqi < quantile(aqi, p = 0.99)) %>%
  mutate(`Years to Treatment` = as.character(score)) %>%
  select(aqi, score, avg, `Years to Treatment`)

#### Make the density plot ####
ggplot(plot) +
  geom_density(aes(log(aqi), color = `Years to Treatment`,
                   group = `Years to Treatment`,
                   fill = `Years to Treatment`)) +
  scale_color_manual(values = c("#011f4b", "orange", "darkgreen")) +
  scale_fill_manual(values = c("#011f4b", "transparent", "transparent")) +
  theme(axis.line.x = element_line(),
        legend.key = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "italic", size = 9),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = c(0,1), legend.justification = c(-.25,1),
        panel.grid.major.y = element_line(color = alpha("lightgray", 0.5))) +

  labs(y = "", x = "Log AQI")

#### Save the plot ####
ggsave(paste0(macdir, "density_aqi_short_ts.png"), width = 6, height = 2.5)

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()
#### ..................................................................... ####
#### Figure G5: Effect of Uber on AQI alerts by stringency ####
#### ..................................................................... ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Add the directory paths ####
macdir <- paste0(figures_dir, "/")

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
est = list(Raw = read_rds("03_gen/05_results/raw_alerts.rds"),
           SDiD = read_rds("03_gen/05_results/sdid_alerts.rds"))

#### Put all estimates together ####
est = rbindlist(est, idcol = "synth", use.names = T, fill = T)
est = rename(est, measure = var) %>% mutate(BIC = BIC/1000)

#### Add the synthetic prefix ####
est = mutate(est, estimator = ifelse(synth == "SDiD", paste0("SDiD", " (", estimator, ")"), estimator))

#### Organize the estimators ####
est$estimator = factor(est$estimator, levels = c("SDiD (SADD)", "SDiD (E-TWFE)",
                                                 "SDiD (TWFE)", "SADD", "E-TWFE",
                                                 "TWFE"))

#### Change the name of the facets ####
est$measure = gsub("alert", "AQI > 99", est$measure)
est$measure = gsub("days_a", "AQI [100-150)", est$measure)
est$measure = gsub("days_b", "AQI [150-200)", est$measure)
est$measure = gsub("days_c", "AQI > 199", est$measure)

#### Organize the facets ####
est$measure = factor(est$measure, levels = c("AQI > 99", "AQI [100-150)",
                                             "AQI [150-200)", "AQI > 199"))

#### Determine the share ####
est = mutate(est, share = 100*(estimate/pt_value))

#### Plot the coefficients for all estimators ####
ggplot(est) +
  geom_errorbar(aes(y = estimator, xmax = estimate + std.error*1.97,
                    xmin = estimate - std.error*1.97),
                width = 0.5, color = "black") +

  geom_point(aes(y = estimator, x = estimate), color = "black") +

  facet_wrap(~measure, scales = "free_x", ncol = 4) +
  geom_vline(aes(xintercept = 0), color = "orange", linetype = "dashed") +
  guides(color = "none", shape = "none") +
  theme(axis.line.x = element_line(),
        axis.text.y = element_text(hjust = 0),
        legend.title = element_blank(),
        legend.key = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "italic", size = 9),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.y = element_blank()) +

  labs(y = "Estimator", x = "Point Estimate and 95% CIs")

#### Save the plot ####
ggsave(paste0(macdir, "alert_level_est.png"),  width = 8, height = 3)

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()
#### ____________________________________________________________________ ####
#### Dynamic ATT ####
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
est = list(TWFE = read_rds("03_gen/05_results/twfe_dynamic.rds"),
           `E-TWFE` = read_rds("03_gen/05_results/etwfe_dynamic.rds"),
           `SA-DD` = read_rds("03_gen/05_results/sadd_dynamic.rds")) %>%
  rbindlist(., idcol = "estimator", use.names = T)

#### Only keep valid estimates for ETWFE ####
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
ggsave(paste0(macdir, "raw_dynamic.png"), width = 6, height = 3.5)

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()
#### ____________________________________________________________________ ####
#### Figures H7 and H8: PM25 Pollution Maps (Appendix) ####
#### ____________________________________________________________________ ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Add the directory paths ####
macdir <- paste0(figures_dir, "/")

#### Add packages ####
library(NatParksPalettes)
library(colorspace)
library(conflicted)
library(ggspatial)
library(patchwork)
library(tidyverse)
library(ggplot2)
library(sf)


#### Load the data set ####
pol = read_rds("03_gen/county_acag.rds")
states = st_read("02_data/05_shp/02_states", quiet = T)
counties = st_read("02_data/05_shp/03_counties", quiet = T)

#### Filter out states outside the continental USA ####
states = filter(states, !(NAME %in% c("Alaska", "Hawaii", "Puerto Rico")))

#### Aggregate the pollution data ####
pol = pol %>% group_by(fips) %>% summarise(pm25 = mean(pm25, na.rm = T))

#### Add an indicator variable for MSAs ####
uber = read_rds("03_gen/03_uber/Uber.rds")
pol = mutate(pol, msa = ifelse(fips %in% uber$fips, 1, 0))

#### Compute the average air pollution in all counties and in MSAs ####
plot = list(`All counties in the US` = mutate(counties, fips = as.numeric(paste0(STATEFP, COUNTYFP))) %>%
              left_join(., select(ungroup(pol), fips, pm25)) %>% st_simplify(preserveTopology = T) %>%
              filter(is.na(pm25) == FALSE),

            `Metropolitan Statistical Areas` = mutate(counties, fips = as.numeric(paste0(STATEFP, COUNTYFP))) %>%
              left_join(., select(ungroup(filter(pol, msa == 1)), fips, pm25)) %>% st_simplify(preserveTopology = T) %>%
              filter(is.na(pm25) == FALSE)) %>%
  rbindlist(., idcol = "type") %>% st_as_sf(.)

#### Define the color palette ####
colors <- natparks.pals("Acadia", n = 30)
swatchplot(colors)
texture <- grDevices::colorRampPalette(c(colors[17:30],"brown", "darkred", "#560216", "black"))(256)
swatchplot(texture)

#### Plot the average AQI ####
all_counties = ggplot(plot |> filter(type == "All counties in the US")) +
  geom_sf(lwd = 0.01, aes(fill = pm25),
          color = alpha("gray", 0.5)) +

  scale_fill_gradientn(colours = texture, na.value = "gray80") +
  labs(x = "", y = "")  +

  theme(panel.background = element_rect(fill = "white"),
        legend.background = element_blank(),
        legend.title = element_text(face = "italic", size = 8, hjust = 0)) +

  guides(fill = guide_colorbar(title.position = "top",
                               title = "Average PM2.5\n2005-2016")) +

  annotation_north_arrow(location = "br", which_north = "false",
                         height = unit(1, "cm"), width = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering(fill = c("black", "white"))) +

  annotation_scale(bar_cols = c("black", "white"),
                   height = unit(0.15, "cm"), location = "bl",
                   text_cex = 1);all_counties


#### Create the Map for average air pollution across the US ####
msa = ggplot(plot |> filter(type != "All counties in the US")) +

  geom_sf(data = states, lwd = 0.01, fill = alpha("gray", 0.25)) +

  geom_sf(lwd = 0.01, aes(fill = pm25),
          color = alpha("gray", 0.5)) +

  scale_fill_gradientn(colours = texture, na.value = "gray80") +
  labs(x = "", y = "")  +

  theme(panel.background = element_rect(fill = "white"),
        legend.background = element_blank(),
        legend.title = element_text(face = "italic", size = 8, hjust = 0)) +

  guides(fill = guide_colorbar(title.position = "top",
                               title = "Average PM2.5\n2005-2016")) +

  annotation_north_arrow(location = "br", which_north = "false",
                         height = unit(1, "cm"), width = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering(fill = c("black", "white"))) +

  annotation_scale(bar_cols = c("black", "white"),
                   height = unit(0.15, "cm"), location = "bl",
                   text_cex = 1);msa

#### Save the maps ####
ggsave(msa, file = paste0(macdir, "ACAGMAPmetro.pdf"), width = 10, height = 5)
ggsave(all_counties, file = paste0(macdir, "ACAGMAP.pdf"), width = 10, height = 5)

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()
#### _____________________________________________________________________ ####
#### Common trends plot for PM25 (Appendix) ####
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

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Add the directory paths ####
macdir <- paste0(figures_dir, "/")

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

#### Plot the common trends for the SDiD design ####
plot = data |>

  mutate(tw = ifelse(score >= 0, NA, tw)) %>%
  mutate(uw = ifelse(is.na(uw) == T, 0, uw))%>%
  mutate(uw = ifelse(is.na(uw) == T, 1, uw)) %>%

  group_by(treated, tg, score, var) |>

  summarise(`Synthetic DiD` = weighted.mean(value, uw, na.rm = T),
            `Standard DiD` = mean(value, na.rm = T),
            tw = mean(tw, na.rm = T)) %>%

  gather(., agg, value, -c(treated, tg, tw, score, var)) %>%

  mutate(treated = ifelse(treated == 1, "Treated", "Controls")) |>
  mutate(tw = ifelse(var == "Standard DiD", 0, tw)) |>

  mutate(tw = ifelse(score > 0, NA, tw))

#### Re-scale the time weights to the smallest of the synthetic values ####
plot = plot |> group_by(var, tg) |>
  mutate(tw2 = {
    vmin <- min(tw, na.rm = TRUE); vmax <- max(tw, na.rm = TRUE)
    nmin <- min(value, na.rm = TRUE) - 1; nmax <- min(value, na.rm = TRUE) + 0.5
    ifelse(is.na(tw), tw, nmin + (tw - vmin) / (vmax - vmin) * (nmax - nmin))
  }) |>
  mutate(min = min(value, na.rm = T) - 1)

#### Simplify the plot to only one facet ####
plot = filter(plot, treated == "Controls" | agg == "Synthetic DiD" & treated == "Treated") %>%
  mutate(agg = ifelse(treated == "Treated", "Treated", agg))


#### Organize the facets ####
plot = mutate(plot, tw2 = ifelse(agg == "Treated", NA, tw2))
plot$agg = factor(plot$agg, levels = c("Standard DiD", "Treated", "Synthetic DiD"))

#### Add the facet ID ####
plot$tg = paste("Treatment Year:", plot$tg)

#### Make the ggplot by treatment year ####
ggplot(plot) +
  geom_line(aes(x = score, y = value, color = agg, group = agg)) +
  facet_wrap(~tg, scales = "free")+
  geom_vline(aes(xintercept = -0.5), linetype = "dashed") +

  # geom_pointrange(aes(x = score, y = tw2, ymax = tw2, ymin = min), color = "black",
  #                size = 0.1, fatten = -20.5) +

  geom_point(data = filter(plot, var %in% c("Private Cars", "Ride Hailing"), score %in%c(max(score), min(score))),
             aes(x = score, y = value, color = agg, group = agg, shape = agg),
             size = 2.5) +

  scale_color_manual(values = c("Treated" =  "orange",
                                "Standard DiD" = "gray",
                                "Synthetic DiD" =  "#49326b")) +

  labs(x = "Score", y = expression(paste("Share of Commuters"))) +

  theme(plot.background = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "italic"),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.margin = margin(t = -10),
        legend.key = element_rect(fill = "transparent", color = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        axis.line.x = element_line(), panel.grid = element_blank())

#### Save the plot ####
ggsave(paste0(macdir, "common_trends_pm25.png"), width = 7, height = 3.5)

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()


#### _____________________________________________________________________ ####
#### Figure H11: Placebo with the Never Treated Group (Appendix) ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Add the directory paths ####
macdir <- paste0(figures_dir, "/")

#### Load the required packages ####
library(conflicted)
library(tidyverse)
library(data.table)
library(lubridate)
library(synthdid)
library(fixest)
library(zoo)
library(parallel)
library(texreg)
library(readxl)

#### Select the correct functions ####
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("isoweek", "lubridate")
conflict_prefer("year", "lubridate")
conflict_prefer("month", "lubridate")
conflict_prefer("map", "purrr")

#### List all the state specific files ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")

#### Transform from wide to long format ####
data = gather(data, var, value, c(avg))

#### Create a list of dates when Uber began operations in treated counties ####
enter_years = unique(filter(data, treated == 1)$enter_year)

#### Create the list to save the placebo estimates ####
placebo = vector("list", 1000)

#### Randomly assign dates to the Never treated group ####
set.seed(123); placebo = lapply(placebo, function(x)

  filter(data, treated == 0) %>% as.data.table(.) %>% group_by(fips) %>%

    mutate(enter_year = sample(enter_years, 1)) %>%
    mutate(enter_year = ifelse(fips %in% sample(unique(filter(data, treated == 0)$fips),
                                                length(unique(filter(data, treated == 0)$fips))/2), 2022, enter_year)) %>%
    mutate(treated = ifelse(enter_year == 2022, 0, 1), score = year - enter_year)  %>%
    mutate(treatment = ifelse(treated == 1 & as.numeric(score) >= 0, 1, 0)) %>%

    feols(value ~ treatment | fips + year, data = ., cluster = "cbsa_id") %>%
    tidy(.) %>% as.data.frame(.))

#### Bind the results from the placebo runs ####
placebo = lapply(placebo, as.data.frame) %>% rbindlist(., idcol = "btr")

#### Check the number of significant and insignificant coefficients ####
nrow(filter(placebo, p.value <0.05 & estimate <  0))
nrow(filter(placebo, p.value <0.05 & estimate >  0))

#### Make an error plot ####
ggplot(placebo %>% mutate(btr = as.numeric(btr))) +
  geom_errorbar(aes(x = btr, ymax = estimate  + (`std.error`*1.975),
                    ymin = estimate - `std.error` * 1.975),
                color = "lightblue", width = 0) +
  theme(panel.background = element_blank(),
        axis.line = element_line()) +
  geom_hline(aes(yintercept = 0), color = "orange") +
  labs(y = "Estimate and 95% CI", x = "Placebo Run")

#### Save the error plot ####
ggsave(paste0(macdir, "PlaceboEst.png"), width = 5, height = 3.5)

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### _____________________________________________________________________ ####
#### Figure I11: Trends in commuting patterns ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Add the directory paths ####
macdir <- paste0(figures_dir, "/")

#### Load the packages ####
library(data.table)
library(tidyverse)
library(synthdid)
library(fixest)
library(broom)

#### Load the regressions data ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")
tindex = read_rds("02_data/06_other/01_pti/pti_b.rds")

#### Filter incomplete data ####
na_fips = filter(tindex, is.na(total) == T)$GEOID
tindex = filter(tindex, !(GEOID %in% na_fips))

#### Add the t index data ####
data = left_join(data, tindex %>% mutate(year = as.numeric(year)) %>%
                   rename(fips = GEOID))

#### Create a composite of bike and walk ####
data = mutate(data, bike_walk = walk + bike) %>% select(-bike, -walk, -telecommuting, -imputed)

#### Gather the variables ####
data = gather(data, var, value, c(cars:bike_walk))

#### Take away all NAs in total ####
data = filter(data, is.na(value) == F)

#### Transform into shares ####
data = mutate(data, value = 100*(value/total))

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

#### Plot the common trends for the SDiD design ####
plot = data |>

  mutate(tw = ifelse(score >= 0, NA, tw)) %>%
  mutate(uw = ifelse(is.na(uw) == T, 0, uw))%>%

  group_by(treated, tg, score, var) |>

  summarise(`Synthetic DiD` = weighted.mean(value, uw, na.rm = T),
            `Standard DiD` = mean(value, na.rm = T),
            tw = mean(tw, na.rm = T)) %>%

  gather(., agg, value, -c(treated, tg, tw, score, var)) %>%

  mutate(treated = ifelse(treated == 1, "Treated", "Controls")) |>
  mutate(tw = ifelse(var == "Standard DiD", 0, tw)) |>

  mutate(tw = ifelse(score > 0, NA, tw))

#### Re-scale the time weights to the smallest of the synthetic values ####
plot = plot |> group_by(var, tg) |>
  mutate(tw2 = {
    vmin <- min(tw, na.rm = TRUE); vmax <- max(tw, na.rm = TRUE)
    nmin <- min(value, na.rm = TRUE) - 1; nmax <- min(value, na.rm = TRUE) + 0.5
    ifelse(is.na(tw), tw, nmin + (tw - vmin) / (vmax - vmin) * (nmax - nmin))
  }) |>
  mutate(min = min(value, na.rm = T) - 1)

#### Simplify the plot to only one facet ####
plot = filter(plot, treated == "Controls" | agg == "Synthetic DiD" & treated == "Treated") %>%
  mutate(agg = ifelse(treated == "Treated", "Treated", agg))

#### Organize the facets ####
plot = mutate(plot, tw2 = ifelse(agg == "Treated", NA, tw2))
plot$agg = factor(plot$agg, levels = c("Standard DiD", "Treated", "Synthetic DiD"))

#### Add the facet ID ####
plot$tg = paste("g = ", plot$tg)

#### Show the common trends between cars and others ####
plot$var = gsub("cars", "Private Cars", plot$var)
plot$var = gsub("other", "Ride Hailing", plot$var)
plot$var = gsub("public_transport", "Public Transp.", plot$var)

#### Make the ggplot by treatment year ####
ggplot(plot %>% filter(var %in% c("Private Cars", "Ride Hailing", "Public Transp."))) +
  geom_line(aes(x = score, y = value, color = agg, group = agg)) +
  facet_wrap(~var + tg, scales = "free", ncol = 7)+
  geom_vline(aes(xintercept = -0.5), linetype = "dashed") +

  geom_point(data = filter(plot, var %in% c("Private Cars", "Ride Hailing", "Public Transp."), score %in%c(max(score), min(score))),
             aes(x = score, y = value, color = agg, group = agg, shape = agg),
             size = 2.5) +

  scale_color_manual(values = c("Treated" =  "orange",
                                "Standard DiD" = "gray",
                                "Synthetic DiD" =  "#49326b")) +

  labs(x = "Score", y = expression(paste("Share of Commuters"))) +

  theme(plot.background = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "italic"),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.key = element_rect(fill = "transparent", color = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        axis.line.x = element_line(),
        panel.grid = element_blank())

#### Save the plot ####
ggsave(paste0(macdir,"common_trends_commute.png"), width = 9, height = 5.5)

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()
#### _____________________________________________________________________ ####
#### Figure I12: Sample restriction in commuting analysis ####
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

#### Load the regressions data ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")
tindex = read_rds("02_data/06_other/01_pti/pti_b.rds")

#### Filter incomplete data ####
na_fips = filter(tindex, is.na(total) == T)$GEOID
tindex = filter(tindex, !(GEOID %in% na_fips))

#### Add the t index data ####
data = left_join(data, tindex %>% mutate(year = as.numeric(year)) %>%
                   rename(fips = GEOID))

#### Create the data for
plot = list(`Never Treated Full Sample` = (data |> filter(enter_year > 2016))$fips %>% unique(.) %>% length(.),
            `Never Treated No Missing Obs. (Strict)` = (data |> filter(enter_year > 2016, is.na(total) == F))$fips %>% unique(.) %>% length(.),
            `Never Treated No Missing Obs.` = (data |> filter(enter_year > 2016, !(fips %in% na_fips)))$fips %>% unique(.) %>% length(.),
            `Eventually Treated Full Sample` = (data |> filter(enter_year <= 2016))$fips  %>% unique(.) %>% length(.),
            `Eventually Treated No Missing Obs. (Strict)` = (data |> filter(enter_year <= 2016, is.na(total) == F))$fips %>% unique(.) %>% length(.),
            `Eventually Treated No Missing Obs.` = (data |> filter(enter_year <= 2016, !(fips %in% na_fips)))$fips %>% unique(.) %>% length(.)) %>%
  lapply(., data.frame) %>% rbindlist(., idcol = "sample") %>% setNames(., c("sample", "length"))

#### Extract the facet and change the name of the sample ####
plot = mutate(plot, facet = case_when(grepl("Never", sample) ~ "Never Treated", T ~ "Eventually Treated"))
plot = mutate(plot, facet = case_when(grepl("Never", sample) ~ "Never Treated", T ~ "Eventually Treated"))
plot$sample = gsub("Never Treated |Eventually Treated ", "", plot$sample)

#### Organize the factors ####
plot$sample = factor(plot$sample, levels = c("No Missing Obs.", "Full Sample", "No Missing Obs. (Strict)"))

#### Make the bar plot ####
ggplot(plot) +
  geom_bar(aes(y = sample, x = length, fill = sample), stat = "identity", width = 0.2) +
  facet_wrap(~facet, scales = "free_y") +

  scale_fill_manual(values = c("black", "orange", "black")) +
  guides(fill = "none") +

  labs(y = "", x = "Number of counties") +
  guides(color = "none", shape = "none") +
  theme(axis.line = element_line(),
        axis.text.y = element_text(hjust = 1),
        legend.title = element_blank(),
        legend.key = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "italic", size = 9),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.y = element_blank())

#### Save the figure ####
ggsave(paste0(macdir, "commute_sample_rest.png"), width = 8, height = 2.75)

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()
#### _____________________________________________________________________ ####
#### Figure I13: Effect of Uber on commuting behavior for alternative estimators ####
#### _____________________________________________________________________ ####

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Add the directory paths ####
macdir <- paste0(figures_dir, "/")

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

#### Load the estimates ####
est = list(did = read_rds("03_gen/05_results/did_commuting_ols.rds"),
           sdid = read_rds("03_gen/05_results/sdid_commuting_ols.rds"))


#### Put all estimates together ####
est = rbindlist(est, idcol = "estimator", use.names = T, fill = T)

#### Only keep the relevant SPECIFICATIONS and specifications ####
plot = est %>% filter(spec %in% c("(2) TWFE", "(2) SADD", "(2) ETWFE",
                                  "SDiD-OLS", "SDiD-SADD", "SDiD-ETWFE")) %>%
  filter(!grepl("bike", var))

#### Transform the names of the variables and specifications ####
plot$spec = gsub("\\(2\\) TWFE", "TWFE", plot$spec)
plot$spec = gsub("\\(2\\) SADD", "SA-DD", plot$spec)
plot$spec = gsub("\\(2\\) ETWFE", "E-TWFE", plot$spec)
plot$spec = gsub("SDiD-OLS", "SDiD (TWFE)", plot$spec)
plot$spec = gsub("SDiD-SADD", "SDiD (SA-DD)", plot$spec)
plot$spec = gsub("SDiD-ETWFE", "SDiD (E-TWFE)", plot$spec)

#### Transform the names of the facets ####
plot$var = gsub("cars", "Private Cars", plot$var)
plot$var = gsub("other", "Ride-Hailing", plot$var)
plot$var = gsub("public_transport", "Public Transport", plot$var)

#### Organize the facets and factors ####
plot$var = factor(plot$var, levels = c("Private Cars", "Ride-Hailing", "Public Transport"))
plot$spec = factor(plot$spec, levels = c("SA-DD", "SDiD (SA-DD)", "TWFE", "SDiD (TWFE)",
                                         "E-TWFE", "SDiD (E-TWFE)"))

#### Make the plot ####
ggplot(plot) +
  geom_point(aes(x = estimate, y = spec)) +
  geom_errorbar(aes(xmax = estimate + std.error*1.645 ,
                    xmin = estimate - std.error*1.645, y = spec), width = 0.25) +
  facet_wrap(~var, scales = "free_x") +
  labs(y = "Estimator", x = "Estimate and 90% CIs") +
  geom_vline(aes(xintercept = 0), color = "orange", linetype = "dashed") +
  guides(color = "none", shape = "none") +
  theme(axis.line = element_line(),
        axis.text.y = element_text(hjust = 1),
        legend.title = element_blank(),
        legend.key = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "italic", size = 9),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.y = element_blank())

#### Save the figure ####
ggsave(paste0(macdir, "est_commute_other.png"), width = 7, height = 2.5)

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()
#### ____________________________________________________________________ ####
#### Figure I14: Plot the average number of taxis in NYC (Appendix) ####
#### _____________________________________________________________________####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Add the directory paths ####
macdir <- paste0(figures_dir, "/")

#### Load packages ####
library(conflicted)
library(data.table)
library(lubridate)
library(tidyverse)
library(vroom)
library(zoo)

#### solve conflicts ####
conflict_prefer("year", "lubridate")
conflict_prefer("filter", "dplyr")
conflict_prefer("month", "lubridate")
conflict_prefer("isoweek", "lubridate")
conflict_prefer("yday", "lubridate")

#### Load the data ####
data = vroom("02_data/06_other/13_NYC_taxi_2010_2017/nyc_taxi_2010_2017_daily.csv")

#### Aggregate to the date ####
data = mutate(data, date = mdy(trip_pickup_datetime)) %>%
  select(date, trips = trip)

#### Add temporal identifiers ####
data = mutate(data, month = month(date), year = year(date),
              weekday = weekdays(date), yday = yday(date))

#### Add the rolling average ####
data = data %>% mutate(`4) Yearly Rolling Avg` = rollmean(trips, 365, fill = NA),
                       `3) Quarterly Rolling Avg` = rollmean(trips, 90, fill = NA))

#### Take away outliers ####
data = data %>% group_by(year, month) %>%
  mutate(trips = ifelse(trips > quantile(trips, 0.95) | trips < quantile(trips, 0.05), mean(trips), trips))

#### Detrend the daily data ####
est = feols(trips ~ 1 | weekday + yday + month, data = data)

#### Include the residuals ####
data = data %>% ungroup() %>%
  mutate(`2) Residuals | Year day, weekday, and month` = residuals(est))%>%
  rename(`1) Raw Data` = trips)

#### Gather the different plots ####
data = gather(data, var, value, -c(date, month, year, weekday, yday))

#### Plot the daily number of taxi trips in NYC ####
ggplot(data) +
  geom_point(aes(x = date, y = value/1000), size = 0.1) +
  facet_wrap(~var, scales = "free", ncol = 4) +

  theme(panel.background = element_rect(fill = "transparent", color = "transparent"),
        strip.background = element_rect(fill = "transparent", color = "transparent"),
        strip.text = element_text(hjust = 0), axis.line = element_line(),
        legend.title = element_blank()) +

  geom_vline(aes(xintercept = as.Date("2012-01-01")), color = "orange", linetype = "dashed") +
  labs(x = "", y = "Taxi trips (Ths)")

#### Save the time plots ####
ggsave(paste0(macdir, "TaxiTrips.png"), width = 8, height = 2.5)

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()
#### ____________________________________________________________________ ####
#### Figure I15: Event-time estimates for the No. of daily yellow taxi trips in NYC ####
#### _____________________________________________________________________####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Add your directory paths ####
macdir <- paste0(figures_dir, "/")

#### Load packages ####
library(conflicted)
library(data.table)
library(lubridate)
library(fixest)
library(tidyverse)
library(vroom)
library(zoo)

#### Solve conflicts ####
conflict_prefer("year", "lubridate")
conflict_prefer("filter", "dplyr")
conflict_prefer("month", "lubridate")
conflict_prefer("isoweek", "lubridate")
conflict_prefer("yday", "lubridate")
conflict_prefer("quarter", "lubridate")

#### Load the data ####
data_yearly = vroom("02_data/06_other/13_NYC_taxi_2010_2017/nyc_taxi_2010_2017_daily.csv") %>%
  mutate(date = mdy(trip_pickup_datetime),
         trips = trip,
         year = year(date),
         month = month(date),
         weekday = weekdays(date),
         yday = yday(date)) %>%
  select(date, trips, year, month, weekday, yday)

#### Run the annual estimates ####
est_yearly <- feols(
  log(trips) ~ i(year, ref = "2012") | weekday + yday + month,
  data = data_yearly,
  cluster = newey ~date + month)

#### Extract the coefficients for the year dummies ####
coefs_yearly <- broom::tidy(est_yearly) %>%
  filter(str_detect(term, "year::")) %>%
  mutate(
    # parse the "year::XXXX" portion
    year_val = as.numeric(gsub(".*::", "", term)),
    # pick Jan 1 as a reference date for each year
    date_label = as.Date(paste0(year_val, "-01-01")),
    freq = "Yearly"
  ) %>%
  rename(estimate = estimate, std_error = std.error) %>%
  select(freq, date_label, estimate, std_error)

#### Create the quarterly data set ####
data_quarterly = vroom("02_data/06_other/13_NYC_taxi_2010_2017/nyc_taxi_2010_2017_daily.csv") %>%
  mutate(date = mdy(trip_pickup_datetime),
         trips = trip,
         year = year(date),
         month = month(date),
         quarter_lab = paste0(year, "-Q", quarter(date)),
         weekday = weekdays(date),
         yday = yday(date)) %>%
  select(date, trips, year, month, quarter_lab, weekday, yday) %>%
  mutate(quarter = gsub(".*-", "", quarter_lab))

#### partial out monthly and quarter-of-year effects from log(trips) ####
test_q <- feols(
  log(trips) ~ factor(quarter),
  data = data_quarterly)

#### Extract the residuals ####
data_quarterly$resid <- test_q$residuals

#### event-study style regression on the residual wrt "2012-Q1" ####
est_quarterly <- feols(
  resid ~ i(quarter_lab, ref = "2012-Q1") | weekday,
  data = data_quarterly,
  cluster = newey ~date)

#### Extract the coefficients for the quarter dummies and parse the quarter labels ####
coefs_quarterly <- broom::tidy(est_quarterly) %>%
  filter(str_detect(term, "quarter_lab::")) %>%
  mutate(
    raw_quarter = gsub(".*::", "", term),  # e.g. "2012-Q2"
    freq = "Quarterly"
  ) %>%
  rename(estimate = estimate, std_error = std.error) %>%
  # parse year and quarter from "YYYY-Q#"
  separate(raw_quarter, into = c("year_str", "q_part"), sep = "-Q") %>%
  mutate(
    year_val   = as.numeric(year_str),
    quarter_val= as.numeric(q_part),
    # create a date: Q1=month1, Q2=month4, Q3=month7, Q4=month10
    quarter_start_month = (quarter_val - 1)*3 + 1,
    date_label = as.Date(paste0(year_val, "-", quarter_start_month, "-01"))
  ) %>%
  select(freq, date_label, estimate, std_error)

#### Add reference quarter "2012-Q1" with estimate=0 ####
coefs_quarterly <- bind_rows(
  coefs_quarterly,
  data.frame(freq = "Quarterly",
             date_label = as.Date("2012-01-01"),
             estimate = 0, std_error = 0))

#### Create the monthly data set ####
data_monthly = vroom("02_data/06_other/13_NYC_taxi_2010_2017/nyc_taxi_2010_2017_daily.csv") %>%
  mutate(date = mdy(trip_pickup_datetime),
         trips = trip,
         year = year(date),
         month = month(date),
         weekday = weekdays(date),
         yday = yday(date),
         year_month_id = format(date, "%Y-%m")) %>%
  select(date, trips, year, month, weekday, yday, year_month_id)

#### Partial out monthly effects ####
test_m <- feols(
  log(trips) ~ factor(month),
  data = data_monthly)

#### Extract the residuals ####
data_monthly$resid <- test_m$residuals

#### event-study style regression on the residual wrt "2012-01" ####
est_monthly <- feols(
  resid ~ i(year_month_id, ref = "2012-01") | weekday,
  data = data_monthly,
  cluster = newey ~date)

#### Extract the coefficients for the month dummies and parse the month labels ####
coefs_monthly <- broom::tidy(est_monthly) %>%
  filter(str_detect(term, "year_month_id::")) %>%
  mutate(
    ym_raw = gsub(".*::", "", term),  # e.g. "2012-05"
    freq = "Monthly",
    # create a date by appending "-01"
    date_label = as.Date(paste0(ym_raw, "-01"))
  ) %>%
  rename(estimate = estimate, std_error = std.error) %>%
  select(freq, date_label, estimate, std_error)

#### Bind all estimates ####
df_all <- bind_rows(coefs_yearly, coefs_quarterly, coefs_monthly) %>%
  mutate(
    ci_lo = estimate - 1.65*std_error,
    ci_hi = estimate + 1.65*std_error
  )

#### Make the plot ####
p_final <- ggplot(df_all, aes(x = date_label, y = estimate)) +
  #geom_point() +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi),
                width = 0, alpha = 0.5) +
  geom_vline(aes(xintercept = as.Date("2012-01-01"))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "orange") +
  facet_wrap(~ freq, scales = "free_x") +
  # scale_x_date: specify breaks so we don't show every label
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  labs(x = "Date", y = "Effect (Relative to Baseline)") +
  theme_minimal(base_size = 13) +
  theme(plot.background = element_blank(),
        panel.background = element_blank(),
        legend.position.inside = c(0.925, 0.8),
        strip.text = element_text(hjust = 0),
        strip.background = element_blank(),
        legend.title = element_blank(),
        axis.line.x = element_line(),
        legend.text = element_text(size = 9),
        panel.grid = element_blank()) +
  labs(x = "Year",
       y = "Effect relative to 2012"); p_final

#### Save to file ####
ggsave(filename = paste0(macdir, "event_time_taxis.png"),
       plot = p_final, width = 8, height = 3.5)

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### _____________________________________________________________________ ####
#### Figure I.16: Trends in gasoline consumption ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Add the directory paths ####
macdir <- paste0(figures_dir, "/")

#### Load packages ####
library(conflicted)
library(data.table)
library(lubridate)
library(tidyverse)
library(vroom)
library(zoo)

#### solve conflicts ####
conflict_prefer("year", "lubridate")
conflict_prefer("filter", "dplyr")
conflict_prefer("month", "lubridate")
conflict_prefer("isoweek", "lubridate")
conflict_prefer("yday", "lubridate")

#### Load the data ####
data = read_rds("03_gen/04_reg/yearly_reg.rds")
gas = read_csv("02_data/06_other/15_california_gas_consumption/california_gas_city_fips.csv")

#### Transform the cars data from wide to long format ####
gas = select(gas, fips, year, sales, stations)

#### Left join the available cars to the Uber data ####
data = left_join(data, gas)

#### Calculate the pre-treatment value on the number of available cars ####
mean(filter(data, enter_year > year)$sales, na.rm = T)

#### Exclude instances of missing data ####
data = filter(data, is.na(sales) == F)

#### Determine the enter years ####
tg = sort(unique(filter(data, enter_year < 2017)$enter_year))

#### Split the data into a list of data frames for each treatment group ####
data = lapply(tg, function(x)
  filter(data, enter_year >= x) %>%
    mutate(tg = x, score = year - tg) %>%
    filter(score %in% seq(-5, 5, 1)) %>%
    mutate(treated = ifelse(enter_year == tg, 1, 0)) %>%
    mutate(treatment = ifelse(year >= enter_year & treated == 1, 1, 0)))

#### Add names to the list elements ####
names(data) = tg

#### Plot the common trends for the SDiD design ####
plot = rbindlist(data) |>

  group_by(treated, tg, score) |>

  summarise(`Standard DiD` = mean(sales, na.rm = T)) %>%

  gather(., agg, value, -c(treated, score, tg)) %>%

  mutate(treated = ifelse(treated == 1, "Treated", "Controls"))

#### Add the facet ID ####
plot$tg = paste("Treatment Year:", plot$tg)

#### Make the ggplot by treatment year ####
ggplot(plot) +
  geom_line(aes(x = score, y = log(value), color = treated, group = treated)) +
  geom_point(aes(x = score, y = log(value), color = treated, group = treated)) +
  facet_wrap(~tg, scales = "free")+
  geom_vline(aes(xintercept = -0.5), linetype = "dashed") +


  scale_color_manual(values = c("Treated" =  "orange",
                                "Controls" =  "#49326b")) +

  labs(x = "Score", y = expression(paste("Average Gasoline Sales (Log)"))) +

  theme(plot.background = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "italic"),
        legend.title = element_blank(),
        legend.position = c(0.8, 0.315),
        legend.margin = margin(t = -10),
        legend.key = element_rect(fill = "transparent", color = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        axis.line.x = element_line())

#### Save the plot ####
ggsave(paste0(macdir, "common_trends_gas.png"), width = 8, height = 4)

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### ____________________________________________________________________ ####
#### Figure J.1: Heterogeneous effects by weekday #### ####
#### _____________________________________________________________________####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Add the directory paths ####
macdir <- paste0(figures_dir, "/")

#### load the data ####
sum = list(`Annual Average` = read_rds("03_gen/05_results/twfe_yearly_weekdays.rds"),
           `Daily Average` = read_rds("03_gen/05_results/twfe_daily_weekdays.rds")) %>%
  rbindlist(., idcol = "agg")

#### Organize the weekdays as factors ####
sum = sum %>%
  mutate(var = factor(var, levels = c("Fr", "Mo", "Tu", "We","Th", "Sa","Su")))

#### Plot the estimates and SE ####
ggplot(sum) + geom_point(aes(x = var, y = estimate)) +
  geom_errorbar(data = filter(sum, var != "Fr"),
                aes(x = var, ymax = estimate + std.error*1.97,
                    ymin = estimate - std.error*1.97),
                width = 0.25, color = "#011f4b") +
  geom_hline(aes(yintercept = 0), color = "LightBlue") +
  geom_vline(aes(xintercept = "Fr"), linetype = "dashed") +
  facet_wrap(~agg) +

  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        strip.background = element_blank(),
        axis.line = element_line(),
        legend.position = "bottom",
        legend.margin=margin(t=-25),
        strip.text = element_text(hjust = 0),
        legend.title = element_blank(),
        legend.key = element_blank(),
        panel.grid = element_blank()) + labs(y = "", x = "")


#### Save the common trends ####
ggsave(paste0(macdir, "weekday_effects.png"),
       width = 7, height = 3)

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### ____________________________________________________________________ ####
#### Figure J.2: Heterogeneous effects by month #### ####
#### _____________________________________________________________________####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Add the directory paths ####
macdir <- paste0(figures_dir, "/")

#### load the data ####
sum = list(`Annual Average` = read_rds("03_gen/05_results/twfe_yearly_months.rds"),
           `Daily Average` = read_rds("03_gen/05_results/twfe_daily_months.rds")) %>%
  rbindlist(., idcol = "agg")

#### Organize the factors ####
sum = sum %>% mutate(var = factor(var, levels = c("Jan", "Feb", "Mar",
                                                  "Apr","May", "Jun",
                                                  "Jul", "Aug", "Sep",
                                                  "Oct", "Nov", "Dec")))
#### Plot the estimates and SE ####
ggplot(sum) + geom_point(aes(x = var, y = estimate)) +
  geom_errorbar(data = filter(sum, var != "Jan"),
                aes(x = var, ymax = estimate + std.error*1.97,
                    ymin = estimate - std.error*1.97),
                width = 0.25, color = "#011f4b") +
  geom_hline(aes(yintercept = 0), color = "LightBlue") +
  geom_vline(aes(xintercept = "Jan"), linetype = "dashed") +
  facet_wrap(~agg) +

  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        strip.background = element_blank(),
        axis.line = element_line(),
        legend.position = "bottom",
        legend.margin=margin(t=-25),
        strip.text = element_text(hjust = 0),
        legend.title = element_blank(),
        legend.key = element_blank(),
        panel.grid = element_blank()) + labs(y = "", x = "")

#### Save the common trends ####
ggsave(paste0(macdir, "monthly_effects.png"),
       width = 7, height = 3)

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### ____________________________________________________________________ ####
#### Figure J.3: Average NowCast AQI per hour of the day ####
#### _____________________________________________________________________####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Add the directory paths ####
macdir <- paste0(figures_dir, "/")

#### Load packages ####
library(tidyverse)

#### Load the hourly NowCast data ####
data = read_rds("03_gen/09_revision/hourly_reg_aqi.rds")

#### Calculate the average NowCast by hour of the day ####
plot = data %>% group_by(hour) %>%
  summarise(`NowCast AQI` = mean(NowCast, na.rm = T),
            sd = sd(NowCast, na.rm = T))
plot = mutate(plot, hour = as.numeric(hour))

#### Plot the average NowCast by hour of the day ####
ggplot(plot) +
  geom_bar(aes(x = hour, y = `NowCast AQI`), stat = "identity", fill = "#011f4b") +
  theme(panel.background = element_blank(),
        strip.background = element_blank(),
        axis.line = element_line(),
        legend.position = "bottom",
        legend.margin = margin(t = -25),
        strip.text = element_text(hjust = 0),
        legend.title = element_blank(),
        legend.key = element_blank(),
        panel.grid = element_blank()) +
  ggpubr::grids() + labs(y = "", x = "") +
  ggtitle("1) Average NowCast per hour of the day")

#### Save the plot ####
ggsave(paste0(macdir, "HourlyTs.png"), width = 5, height = 4)

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### ____________________________________________________________________ ####
#### Figure J.4: Heterogeneous treatment effects by hour of the day ####
#### _____________________________________________________________________####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Add the directory paths ####
macdir <- paste0(figures_dir, "/")

#### Load packages ####
library(tidyverse)
library(data.table)

#### Load the regression results ####
sum = list(`Annual Average` = read_rds("03_gen/09_revision/nc_yearly_diff_twfe.rds"),
           `Hourly Average` = read_rds("03_gen/09_revision/nc_hourly_diff_twfe.rds")) %>%
  rbindlist(., idcol = "agg")

#### Plot the estimates and SE ####
ggplot(sum) + geom_point(aes(x = var, y = est)) +
  geom_errorbar(data = filter(sum, var != 5),
                aes(x = var, ymax = est + se*1.97,
                    ymin = est - se*1.97),
                width = 0.25, color = "#011f4b") +
  geom_hline(aes(yintercept = 0), color = "LightBlue") +
  geom_vline(aes(xintercept = 5), linetype = "dashed") +
  facet_wrap(~agg, scales = "free") +
  theme(panel.background = element_blank(),
        strip.background = element_blank(),
        axis.line = element_line(),
        legend.position = "bottom",
        legend.margin = margin(t = -25),
        strip.text = element_text(hjust = 0),
        legend.title = element_blank(),
        legend.key = element_blank(),
        panel.grid = element_blank()) +
  ggpubr::grids() + labs(y = "", x = "")

#### Save the plot ####
ggsave(paste0(macdir, "hourly_effects.png"), width = 7, height = 3)

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### _____________________________________________________________________ ####
#### Table I13: NHTS trip-shares by purpose of travel                      ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Add the necessary packages ####
library(haven)
library(tidyverse)

#### Load the NHTS trip-level data ####
d <- read_dta("02_data/06_other/2_NHTS/temp/data_nhts.dta")

#### Filter to large urban areas (urbansize 4 or 5) ####
## Table note: "excluding rural and small (untreated) urban areas"
d <- filter(d, urbansize %in% c(4, 5))

#### Keep only table years ####
d <- filter(d, year %in% c(2009, 2017, 2022))

#### Year-specific trptrans mode definitions ####
## Same definitions as Table I14 (verified to replicate exactly)
## 2009: driving=1-4, transit=9,10,17,18, taxi=19
## 2017: driving=3-6, transit=11,16, taxi=17
## 2022: driving=1-4, transit=8,11,17, taxi=15,16
mode_defs <- list(
  "2009" = list(driving = c(1, 2, 3, 4), transit = c(9, 10, 17, 18), taxi = 19),
  "2017" = list(driving = c(3, 4, 5, 6), transit = c(11, 16),        taxi = 17),
  "2022" = list(driving = c(1, 2, 3, 4), transit = c(8, 11, 17),     taxi = c(15, 16))
)

#### Trip purpose definitions ####
## whytrp1s (harmonized across years): 10=Work, 1=Home, 50+80=Social
## Weighted by wttrdfin (final trip weight)
## Note: original Stata code (final_260210.do) used trippurp + unweighted,
## but 2022 trippurp uses numeric codes incompatible with 2009/2017 text codes.
## whytrp1s + wttrdfin replicates 33/36 cells within 0.5pp.

#### Compute purpose shares for each year x mode combination ####
compute_shares <- function(sub) {
  w <- sum(sub$wttrdfin, na.rm = TRUE)
  c(Work   = 100 * sum(sub$wttrdfin[sub$whytrp1s == 10], na.rm = TRUE) / w,
    Home   = 100 * sum(sub$wttrdfin[sub$whytrp1s == 1], na.rm = TRUE) / w,
    Social = 100 * sum(sub$wttrdfin[sub$whytrp1s %in% c(50, 80)], na.rm = TRUE) / w)
}

results <- list()
for (yr in c(2009, 2017, 2022)) {
  base <- filter(d, year == yr)
  modes <- mode_defs[[as.character(yr)]]

  ## All Trips
  s <- compute_shares(base)
  results[[length(results) + 1]] <- tibble(
    Year = yr, Mode = "All Trips",
    Work = s["Work"], Home = s["Home"], Social = s["Social"]
  )

  ## Mode-specific
  for (m in c("driving", "transit", "taxi")) {
    sub <- filter(base, trptrans %in% modes[[m]])
    s <- compute_shares(sub)
    label <- switch(m, driving = "Driving", transit = "Transit", taxi = "Taxi/TNCs")
    results[[length(results) + 1]] <- tibble(
      Year = yr, Mode = label,
      Work = s["Work"], Home = s["Home"], Social = s["Social"]
    )
  }
}

table_i13 <- bind_rows(results)

#### Print the replicated table ####
cat("\n=== Table I13: NHTS trip-shares by purpose of travel ===\n\n")
print(as.data.frame(table_i13), digits = 4, row.names = FALSE)

#### Verify against the published values ####
target <- tribble(
  ~Year, ~Mode,        ~t_Work, ~t_Home, ~t_Social,
  2009,  "All Trips",    11.3,   35.1,    19.0,
  2009,  "Driving",      11.8,   34.8,    17.4,
  2009,  "Transit",      19.4,   37.1,    13.8,
  2009,  "Taxi/TNCs",     8.07,  37.7,    20.4,
  2017,  "All Trips",    12.9,   35.5,    18.8,
  2017,  "Driving",      12.9,   35.0,    17.9,
  2017,  "Transit",      23.8,   37.4,    12.5,
  2017,  "Taxi/TNCs",    18.8,   36.7,    24.2,
  2022,  "All Trips",    10.8,   38.4,    20.6,
  2022,  "Driving",      11.6,   39.1,    19.4,
  2022,  "Transit",      19.7,   42.9,     6.01,
  2022,  "Taxi/TNCs",    19.8,   44.0,    26.3
)

comp <- left_join(table_i13, target, by = c("Year", "Mode"))
diffs <- comp %>% summarise(
  max_work   = max(abs(Work - t_Work), na.rm = TRUE),
  max_home   = max(abs(Home - t_Home), na.rm = TRUE),
  max_social = max(abs(Social - t_Social), na.rm = TRUE)
)
cat("\nMax absolute differences vs published table:\n")
print(as.data.frame(diffs), digits = 3)

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### _____________________________________________________________________ ####
#### Table I14: NHTS trip-shares by mode (large urban areas)               ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()

#### Add the necessary packages ####
library(haven)
library(tidyverse)

#### Load the NHTS trip-level data ####
d <- read_dta("02_data/06_other/2_NHTS/temp/data_nhts.dta")

#### Filter to large urban areas (urbansize 4 or 5) ####
d <- filter(d, urbansize %in% c(4, 5))

#### Define a function to compute weighted mode shares ####
mode_share <- function(sub, codes) {
  sum(sub$wttrdfin[sub$trptrans %in% codes], na.rm = TRUE) /
    sum(sub$wttrdfin, na.rm = TRUE) * 100
}

#### Year-specific trptrans code definitions ####
## Each NHTS wave uses its own coding scheme for trptrans.
## 2009: 1=car,2=van,3=SUV,4=pickup,...,9=local bus,10=commuter bus,
##       17=subway,18=streetcar,19=taxicab
## 2017: 3=car,4=SUV,5=van,6=pickup,...,11=public bus,16=subway,17=taxi/limo+TNC
## 2022: 1=car,2=van,3=SUV,4=pickup,...,8=motorcycle/transit?,9=bus,
##       11=school bus,15=taxi,16=TNC,17=rail

mode_defs <- list(
  "2009" = list(ldv = c(1,2,3,4), transit = c(9,10,17,18), taxi = 19,
                taxi_tnc = integer(0), tnc = integer(0)),
  "2017" = list(ldv = c(3,4,5,6), transit = c(11,16), taxi = integer(0),
                taxi_tnc = 17, tnc = integer(0)),
  "2022" = list(ldv = c(1,2,3,4), transit = c(8,11,17), taxi = 15,
                taxi_tnc = integer(0), tnc = 16)
)

#### Trip purpose definitions ####
## Work = whytrp1s == 10
## Leisure = whytrp1s %in% c(50, 80)

#### Compute shares for all year x trip-type x mode combinations ####
results <- list()
for (yr in c(2009, 2017, 2022)) {
  base <- filter(d, year == yr)
  modes <- mode_defs[[as.character(yr)]]

  for (tt in c("All", "Work", "Leisure")) {
    ss <- if (tt == "All") base
          else if (tt == "Work") filter(base, whytrp1s == 10)
          else filter(base, whytrp1s %in% c(50, 80))

    results[[length(results) + 1]] <- tibble(
      Year     = yr,
      Trips    = tt,
      LDVs     = mode_share(ss, modes$ldv),
      Transit  = mode_share(ss, modes$transit),
      Taxi     = if (length(modes$taxi) > 0) mode_share(ss, modes$taxi) else NA_real_,
      TaxiTNC  = if (length(modes$taxi_tnc) > 0) mode_share(ss, modes$taxi_tnc) else NA_real_,
      TNCs     = if (length(modes$tnc) > 0) mode_share(ss, modes$tnc) else NA_real_
    )
  }
}

table_i14 <- bind_rows(results)

#### Print the replicated table ####
cat("\n=== Table I14: NHTS trip-shares by mode (large urban areas) ===\n\n")
print(as.data.frame(table_i14), digits = 4, row.names = FALSE)

#### Verify against the published values ####
target <- tribble(
  ~Year, ~Trips, ~t_LDVs, ~t_Transit, ~t_Taxi, ~t_TaxiTNC, ~t_TNCs,
  2009, "All",     77.30, 3.62, 0.34, NA,   NA,
  2009, "Work",    80.80, 6.21, 0.24, NA,   NA,
  2009, "Leisure", 70.90, 2.65, 0.36, NA,   NA,
  2017, "All",     76.20, 4.19, NA,   0.85, NA,
  2017, "Work",    76.10, 7.71, NA,   1.24, NA,
  2017, "Leisure", 72.50, 2.79, NA,   1.09, NA,
  2022, "All",     82.40, 2.30, 0.03, NA,   0.66,
  2022, "Work",    88.00, 4.18, 0.00, NA,   1.21,
  2022, "Leisure", 77.60, 0.67, 0.00, NA,   0.49
)

comp <- left_join(table_i14, target, by = c("Year", "Trips"))
diffs <- comp %>% summarise(
  max_ldv     = max(abs(LDVs - t_LDVs), na.rm = TRUE),
  max_transit = max(abs(Transit - t_Transit), na.rm = TRUE),
  max_taxi    = max(abs(Taxi - t_Taxi), na.rm = TRUE),
  max_txtnc   = max(abs(TaxiTNC - t_TaxiTNC), na.rm = TRUE),
  max_tnc     = max(abs(TNCs - t_TNCs), na.rm = TRUE)
)
cat("\nMax absolute differences vs published table:\n")
print(as.data.frame(diffs), digits = 3)

#### Clear the space ####
rm(list = setdiff(ls(), c("root", "code_dir", "data_raw", "data_gen", "results_dir", "figures_dir", "mc_cores"))); gc()
