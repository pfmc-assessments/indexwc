library(dplyr)
library(nwfscSurvey)
library(indexwc)
library(ggplot2)
library(png)
library(grid)
library(here)

configuration <- configuration

savedir <- here::here("additional_runs")

####################################################################
#longspine thornyhead

#filter for sp and source
sp <- "longspine thornyhead"

configuration_sp <- configuration |>
  dplyr::filter(species == sp, source == "NWFSC.Combo")

pulled_data <- nwfscSurvey::pull_catch(
  common_name = sp,
survey = "NWFSC.Combo")

data_filtered <- format_data(pulled_data) |>
  dplyr::filter(depth <= configuration_sp$min_depth[1], depth >= configuration_sp$max_depth[1],
                latitude >= configuration_sp$min_latitude[1], latitude <= configuration_sp$max_latitude[1],
                year >= configuration_sp$min_year[1], year <= configuration_sp$max_year[1])

pred_grid <- sdmTMB::replicate_df(california_current_grid,
                                  time_name = "year",
                                  time_values = unique(data_filtered$year))

pred_grid$fyear <- as.factor(pred_grid$year)

#add depth_scaled and depth_scaled_squared
pred_grid$neg_depth <- -pred_grid$depth
mean_neg_depth <- mean(pred_grid$neg_depth)
sd_neg_depth <- sd(pred_grid$neg_depth)
pred_grid$depth_scaled <- - (pred_grid$neg_depth - mean_neg_depth) / sd_neg_depth
pred_grid$depth_scaled_squared <- pred_grid$depth_scaled^2

#original configuration (before it was changed mid-January 2026)
fit_1 <- run_sdmtmb(
  dir_main = NULL,
  data = data_filtered,
  family = sdmTMB::tweedie(),
  formula = configuration_sp$formula[1],
  n_knots = configuration_sp$knots[1],
  share_range = FALSE,
  anisotropy = TRUE,
  spatial = "on",
  spatiotemporal = list("iid","iid")
)

diagnostics_1 <- indexwc::diagnose(dir = here::here("additional_runs", "longspine_thornyhead", "wcgbts", "tweedie", "fit_1", "diagnostics"), fit = fit_1, prediction_grid = pred_grid)
diagnostics_1$sanity

index_1 <- indexwc::calc_index_areas(data = fit_1$data, fit = fit_1, prediction_grid = pred_grid, dir = here::here("additional_runs", "longspine_thornyhead", "wcgbts", "tweedie", "fit_1", "indices"))


fit_2 <- run_sdmtmb(
  dir_main = NULL,
  data = data_filtered,
  family = sdmTMB::delta_gamma(),
  formula = configuration_sp$formula[1],
  n_knots = configuration_sp$knots[1],
  share_range = FALSE,
  anisotropy = TRUE,
  spatial = "on",
  spatiotemporal = list("iid","iid")
)
#only change is distribution family

diagnostics_2 <- indexwc::diagnose(dir = here::here("additional_runs", "longspine_thornyhead", "wcgbts", "delta_gamma", "fit_2", "diagnostics"), fit = fit_2, prediction_grid = pred_grid)
diagnostics_2$sanity
#hessian_ok is FALSE

#index_2 <- indexwc::calc_index_areas(data = fit_2$data, fit = fit_2, prediction_grid = pred_grid, dir = here::here("additional_runs", "longspine_thornyhead", "wcgbts", "delta_gamma", "fit_2", "indices"))

fit_3 <- run_sdmtmb(
  dir_main = NULL,
  data = data_filtered,
  family = sdmTMB::delta_lognormal(),
  formula = configuration_sp$formula[1],
  n_knots = configuration_sp$knots[1],
  share_range = FALSE,
  anisotropy = TRUE,
  spatial = "on",
  spatiotemporal = list("iid","iid")
)
#only change is distribution family

diagnostics_3 <- indexwc::diagnose(dir = here::here("additional_runs", "longspine_thornyhead", "wcgbts", "delta_lognormal", "fit_3", "diagnostics"), fit = fit_3, prediction_grid = pred_grid)
diagnostics_3$sanity
#hessian_ok is FALSE

#index_3 <- indexwc::calc_index_areas(data = fit_3$data, fit = fit_3, prediction_grid = pred_grid, dir = here::here("additional_runs", "longspine_thornyhead", "wcgbts", "delta_lognormal", "fit_3", "indices"))

##################################################
#original configuration as it was in the 2024 stock assessment priorization
fit_4 <- run_sdmtmb(
  dir_main = NULL,
  data = data_filtered,
  family = sdmTMB::delta_lognormal(),
  formula = "catch_weight ~ 0 + fyear + pass_scaled + depth_scaled + depth_scaled_squared",
  n_knots = configuration_sp$knots[1],
  share_range = FALSE,
  anisotropy = TRUE,
  spatial = "on",
  spatiotemporal = list("off","off")
)

diagnostics_4 <- indexwc::diagnose(dir = here::here("additional_runs", "longspine_thornyhead", "wcgbts", "delta_lognormal", "fit_4", "diagnostics"), fit = fit_4, prediction_grid = pred_grid)
diagnostics_4$sanity

index_4 <- indexwc::calc_index_areas(data = fit_4$data, fit = fit_4, prediction_grid = pred_grid, dir = here::here("additional_runs", "longspine_thornyhead", "wcgbts", "delta_lognormal", "fit_4", "indices"))
#better qq plot, best model aic, CHOSEN MODEL

fit_5 <- run_sdmtmb(
  dir_main = NULL,
  data = data_filtered,
  family = sdmTMB::delta_lognormal(),
  formula = "catch_weight ~ 0 + fyear + pass_scaled + depth_scaled + depth_scaled_squared",
  n_knots = configuration_sp$knots[1],
  share_range = TRUE,
  anisotropy = TRUE,
  spatial = "on",
  spatiotemporal = list("off","off")
)
#share_ranged changed to true
diagnostics_5 <- indexwc::diagnose(dir = here::here("additional_runs", "longspine_thornyhead", "wcgbts", "delta_lognormal", "fit_5", "diagnostics"), fit = fit_5, prediction_grid = pred_grid)
diagnostics_5$sanity
#qq does not look great

fit_6 <- run_sdmtmb(
  dir_main = NULL,
  data = data_filtered,
  family = sdmTMB::delta_lognormal(),
  formula = "catch_weight ~ 0 + fyear + pass_scaled",
  n_knots = configuration_sp$knots[1],
  share_range = FALSE,
  anisotropy = TRUE,
  spatial = "on",
  spatiotemporal = list("off","off")
)
#share_ranged changed to false and took out depth
diagnostics_6 <- indexwc::diagnose(dir = here::here("additional_runs", "longspine_thornyhead", "wcgbts", "delta_lognormal", "fit_6", "diagnostics"), fit = fit_6, prediction_grid = pred_grid)
diagnostics_6$sanity
#qq plot still not great, is it better?

fit_7 <- run_sdmtmb(
  dir_main = NULL,
  data = data_filtered,
  family = sdmTMB::delta_lognormal(),
  formula = "catch_weight ~ 0 + fyear + pass_scaled",
  n_knots = configuration_sp$knots[1],
  share_range = TRUE,
  anisotropy = TRUE,
  spatial = "on",
  spatiotemporal = list("off","off")
)
#share_ranged changed to true and took out depth

diagnostics_7 <- indexwc::diagnose(dir = here::here("additional_runs", "longspine_thornyhead", "wcgbts", "delta_lognormal", "fit_7", "diagnostics"), fit = fit_7, prediction_grid = pred_grid)
diagnostics_7$sanity

########################################################
#lingcod south

savedir <- here::here("additional_runs")

#filter for sp and source
sp <- "lingcod"

configuration_sp <- configuration |>
  dplyr::filter(species == sp, source == "NWFSC.Combo")

#changing to lingcod south
configuration_sp$min_latitude <- 31.9
configuration_sp$max_latitude <- 40.1667

pulled_data <- nwfscSurvey::pull_catch(
  common_name = sp,
  survey = "NWFSC.Combo")

data_filtered <- format_data(pulled_data) |>
  dplyr::filter(depth <= configuration_sp$min_depth[1], depth >= configuration_sp$max_depth[1],
                latitude >= configuration_sp$min_latitude[1], latitude <= configuration_sp$max_latitude[1],
                year >= configuration_sp$min_year[1], year <= configuration_sp$max_year[1])

pred_grid <- sdmTMB::replicate_df(california_current_grid,
                                  time_name = "year",
                                  time_values = unique(data_filtered$year))

pred_grid$fyear <- as.factor(pred_grid$year)

#configuration of lingcod north used here
fit_1 <- run_sdmtmb(
  dir_main = NULL,
  data = data_filtered,
  family = sdmTMB::delta_gamma(),
  formula = configuration_sp$formula[1],
  n_knots = configuration_sp$knots[1],
  share_range = configuration_sp$share_range[1],
  anisotropy = configuration_sp$anisotropy[1],
  spatial = "on",
  spatiotemporal = list("iid","iid")
)

diagnostics_1 <- indexwc::diagnose(dir = here::here("additional_runs", "lingcod", "wcgbts", "delta_gamma", "fit_1", "diagnostics"), fit = fit_1, prediction_grid = pred_grid)
diagnostics_1$sanity

index_1 <- indexwc::calc_index_areas(data = fit_1$data, fit = fit_1, prediction_grid = pred_grid, dir = here::here("additional_runs", "lingcod", "wcgbts", "delta_gamma", "fit_1", "indices"))
#CHOSEN MODEL

fit_2 <- run_sdmtmb(
  dir_main = NULL,
  data = data_filtered,
  family = sdmTMB::delta_lognormal(),
  formula = configuration_sp$formula[1],
  n_knots = configuration_sp$knots[1],
  share_range = configuration_sp$share_range[1],
  anisotropy = configuration_sp$anisotropy[1],
  spatial = "on",
  spatiotemporal = list("iid","iid")
)

diagnostics_2 <- indexwc::diagnose(dir = here::here("additional_runs", "lingcod", "wcgbts", "delta_lognormal", "fit_2", "diagnostics"), fit = fit_2, prediction_grid = pred_grid)
diagnostics_2$sanity

index_2 <- indexwc::calc_index_areas(data = fit_2$data, fit = fit_2, prediction_grid = pred_grid, dir = here::here("additional_runs", "lingcod", "wcgbts", "delta_lognormal", "fit_2", "indices"))

###########################################################

#greenspotted rockfish

#filter for sp and source
sp <- "greenspotted rockfish"

configuration_sp <- configuration |>
  dplyr::filter(species == sp, source == "NWFSC.Combo")

pulled_data <- nwfscSurvey::pull_catch(
  common_name = sp,
  survey = "NWFSC.Combo")

data_filtered <- format_data(pulled_data) |>
  dplyr::filter(depth <= configuration_sp$min_depth[1], depth >= configuration_sp$max_depth[1],
                latitude >= configuration_sp$min_latitude[1], latitude <= configuration_sp$max_latitude[1],
                year >= configuration_sp$min_year[1], year <= configuration_sp$max_year[1])

pred_grid <- sdmTMB::replicate_df(california_current_grid,
                                  time_name = "year",
                                  time_values = unique(data_filtered$year))

pred_grid$fyear <- as.factor(pred_grid$year)

#original configuration
fit_1 <- run_sdmtmb(
  dir_main = NULL,
  data = data_filtered,
  family = sdmTMB::delta_lognormal(), #configuration_sp$family[1] using this instead does not work
  formula = configuration_sp$formula[1],
  n_knots = configuration_sp$knots[1],
  share_range = configuration_sp$share_range[1],
  anisotropy = configuration_sp$anisotropy[1],
  spatial = "on",
  spatiotemporal = list("iid", "iid") #c(configuration_sp$spatiotemporal1, configuration_sp$spatiotemporal2) using this instead does not work
)

diagnostics_1 <- indexwc::diagnose(dir = here::here("additional_runs", "greenspotted_rockfish", "wcgbts", "delta_lognormal", "fit_1", "diagnostics"), fit = fit_1, prediction_grid = pred_grid)
diagnostics_1$sanity

index_1 <- indexwc::calc_index_areas(data = fit_1$data, fit = fit_1, prediction_grid = pred_grid, dir = here::here("additional_runs", "greenspotted_rockfish", "wcgbts", "delta_lognormal", "fit_1", "indices"))
#CHOSEN MODEL

fit_1.5 <- run_sdmtmb(
  dir_main = NULL,
  data = data_filtered,
  family = sdmTMB::delta_gamma(),
  formula = configuration_sp$formula[1],
  n_knots = configuration_sp$knots[1],
  share_range = configuration_sp$share_range[1],
  anisotropy = configuration_sp$anisotropy[1],
  spatial = "on",
  spatiotemporal = list("iid", "iid")
)

diagnostics_1.5 <- indexwc::diagnose(dir = here::here("additional_runs", "greenspotted_rockfish", "wcgbts", "delta_gamma", "fit_1.5", "diagnostics"), fit = fit_1.5, prediction_grid = pred_grid)
diagnostics_1.5$sanity
#hessian_ok is FALSE

#index_1.5 <- indexwc::calc_index_areas(data = fit_1.5$data, fit = fit_1.5, prediction_grid = pred_grid, dir = here::here("additional_runs", "greenspotted_rockfish", "wcgbts", "delta_gamma", "fit_1.5", "indices"))


fit_2 <- run_sdmtmb(
  dir_main = NULL,
  data = data_filtered,
  family = sdmTMB::delta_lognormal(),
  formula = configuration_sp$formula[1],
  n_knots = configuration_sp$knots[1],
  share_range = TRUE,
  anisotropy = configuration_sp$anisotropy[1],
  spatial = "on",
  spatiotemporal = list("iid", "iid")
)
#the only change is share_range is now true

diagnostics_2 <- indexwc::diagnose(dir = here::here("additional_runs", "greenspotted_rockfish", "wcgbts", "delta_lognormal", "fit_2", "diagnostics"), fit = fit_2, prediction_grid = pred_grid)
diagnostics_2$sanity

index_2 <- indexwc::calc_index_areas(data = fit_2$data, fit = fit_2, prediction_grid = pred_grid, dir = here::here("additional_runs", "greenspotted_rockfish", "wcgbts", "delta_lognormal", "fit_2", "indices"))

fit_3 <- run_sdmtmb(
  dir_main = NULL,
  data = data_filtered,
  family = sdmTMB::delta_lognormal(),
  formula = configuration_sp$formula[1],
  n_knots = configuration_sp$knots[1],
  share_range = TRUE,
  anisotropy = configuration_sp$anisotropy[1],
  spatial = "on",
  spatiotemporal = list("iid", "off")
)
#the only change is share_range is now true and spatiotemporal2 is off

diagnostics_3 <- indexwc::diagnose(dir = here::here("additional_runs", "greenspotted_rockfish", "wcgbts", "delta_lognormal", "fit_3", "diagnostics"), fit = fit_3, prediction_grid = pred_grid)
diagnostics_3$sanity

index_3 <- indexwc::calc_index_areas(data = fit_3$data, fit = fit_3, prediction_grid = pred_grid, dir = here::here("additional_runs", "greenspotted_rockfish", "wcgbts", "delta_lognormal", "fit_3", "indices"))

fit_4 <- run_sdmtmb(
  dir_main = NULL,
  data = data_filtered,
  family = sdmTMB::delta_lognormal(),
  formula = configuration_sp$formula[1],
  n_knots = configuration_sp$knots[1],
  share_range = TRUE,
  anisotropy = configuration_sp$anisotropy[1],
  spatial = "on",
  spatiotemporal = list("off", "off")
)
#the only change is share_range is now true and spatiotemporal2 is off and spatiotemportal1 is off
#ran, but gradients check failed will try with other families

diagnostics_4 <- indexwc::diagnose(dir = here::here("additional_runs", "greenspotted_rockfish", "wcgbts", "delta_lognormal", "fit_4", "diagnostics"), fit = fit_4, prediction_grid = pred_grid)
diagnostics_4$sanity

index_4 <- indexwc::calc_index_areas(data = fit_4$data, fit = fit_4, prediction_grid = pred_grid, dir = here::here("additional_runs", "greenspotted_rockfish", "wcgbts", "delta_lognormal", "fit_4", "indices"))

fit_5 <- run_sdmtmb(
  dir_main = NULL,
  data = data_filtered,
  family = sdmTMB::delta_gamma(),
  formula = configuration_sp$formula[1],
  n_knots = configuration_sp$knots[1],
  share_range = TRUE,
  anisotropy = configuration_sp$anisotropy[1],
  spatial = "on",
  spatiotemporal = list("off", "off")
)
#the only change was distribution family
diagnostics_5 <- indexwc::diagnose(dir = here::here("additional_runs", "greenspotted_rockfish", "wcgbts", "delta_gamma", "fit_5", "diagnostics"), fit = fit_5, prediction_grid = pred_grid)
diagnostics_5$sanity

index_5 <- indexwc::calc_index_areas(data = fit_5$data, fit = fit_5, prediction_grid = pred_grid, dir = here::here("additional_runs", "greenspotted_rockfish", "wcgbts", "delta_gamma", "fit_5", "indices"))


fit_6 <- run_sdmtmb(
  dir_main = NULL,
  data = data_filtered,
  family = sdmTMB::delta_lognormal(),
  formula = configuration_sp$formula[1],
  n_knots = configuration_sp$knots[1],
  share_range = TRUE,
  anisotropy = FALSE,
  spatial = "on",
  spatiotemporal = list("off", "off")
)
#back to lognormal, now also turning off anisotropy

diagnostics_6 <- indexwc::diagnose(dir = here::here("additional_runs", "greenspotted_rockfish", "wcgbts", "delta_lognormal", "fit_6", "diagnostics"), fit = fit_6, prediction_grid = pred_grid)
diagnostics_6$sanity

index_6 <- indexwc::calc_index_areas(data = fit_6$data, fit = fit_6, prediction_grid = pred_grid, dir = here::here("additional_runs", "greenspotted_rockfish", "wcgbts", "delta_lognormal", "fit_6", "indices"))

############################################################################
#greenstriped rockfish

#filter for sp and source
sp <- "greenstriped rockfish"

configuration_sp <- configuration |>
  dplyr::filter(species == sp, source == "NWFSC.Combo")

pulled_data <- nwfscSurvey::pull_catch(
  common_name = sp,
  survey = "NWFSC.Combo")

data_filtered <- format_data(pulled_data) |>
  dplyr::filter(depth <= configuration_sp$min_depth[1], depth >= configuration_sp$max_depth[1],
                latitude >= configuration_sp$min_latitude[1], latitude <= configuration_sp$max_latitude[1],
                year >= configuration_sp$min_year[1], year <= configuration_sp$max_year[1])

pred_grid <- sdmTMB::replicate_df(california_current_grid,
                                  time_name = "year",
                                  time_values = unique(data_filtered$year))

pred_grid$fyear <- as.factor(pred_grid$year)

#original configuration
fit_1 <- run_sdmtmb(
  dir_main = NULL,
  data = data_filtered,
  family = sdmTMB::delta_gamma(),
  formula = configuration_sp$formula[1],
  n_knots = configuration_sp$knots[1],
  share_range = configuration_sp$share_range[1],
  anisotropy = configuration_sp$anisotropy[1],
  spatial = "on",
  spatiotemporal = list("iid", "iid")
)

diagnostics_1 <- indexwc::diagnose(dir = here::here("additional_runs", "greenstriped_rockfish", "wcgbts", "delta_gamma", "fit_1", "diagnostics"), fit = fit_1, prediction_grid = pred_grid)
diagnostics_1$sanity

index_1 <- indexwc::calc_index_areas(data = fit_1$data, fit = fit_1, prediction_grid = pred_grid, dir = here::here("additional_runs", "greenstriped_rockfish", "wcgbts", "delta_gamma", "fit_1", "indices"))
#CHOSEN MODEL

fit_2 <- run_sdmtmb(
  dir_main = NULL,
  data = data_filtered,
  family = sdmTMB::delta_lognormal(),
  formula = configuration_sp$formula[1],
  n_knots = configuration_sp$knots[1],
  share_range = configuration_sp$share_range[1],
  anisotropy = configuration_sp$anisotropy[1],
  spatial = "on",
  spatiotemporal = list("iid", "iid")
)

diagnostics_2 <- indexwc::diagnose(dir = here::here("additional_runs", "greenstriped_rockfish", "wcgbts", "delta_lognormal", "fit_2", "diagnostics"), fit = fit_2, prediction_grid = pred_grid)
diagnostics_2$sanity
#hessian_ok is FALSE

#index_2 <- indexwc::calc_index_areas(data = fit_2$data, fit = fit_2, prediction_grid = pred_grid, dir = here::here("additional_runs", "greenstriped_rockfish", "wcgbts", "delta_lognormal", "fit_2", "indices"))

###########################################################################
#rex sole

#filter for sp and source
sp <- "rex sole"

configuration_sp <- configuration |>
  dplyr::filter(species == sp, source == "NWFSC.Combo")

pulled_data <- nwfscSurvey::pull_catch(
  common_name = sp,
  survey = "NWFSC.Combo")

data_filtered <- format_data(pulled_data) |>
  dplyr::filter(depth <= configuration_sp$min_depth[1], depth >= configuration_sp$max_depth[1],
                latitude >= configuration_sp$min_latitude[1], latitude <= configuration_sp$max_latitude[1],
                year >= configuration_sp$min_year[1], year <= configuration_sp$max_year[1])

pred_grid <- sdmTMB::replicate_df(california_current_grid,
                                  time_name = "year",
                                  time_values = unique(data_filtered$year))

pred_grid$fyear <- as.factor(pred_grid$year)

#original configuration
fit_1 <- run_sdmtmb(
  dir_main = NULL,
  data = data_filtered,
  family = sdmTMB::delta_gamma(),
  formula = configuration_sp$formula[1],
  n_knots = configuration_sp$knots[1],
  share_range = configuration_sp$share_range[1],
  anisotropy = configuration_sp$anisotropy[1],
  spatial = "on",
  spatiotemporal = list("iid", "iid")
)

diagnostics_1 <- indexwc::diagnose(dir = here::here("additional_runs", "rex_sole", "wcgbts", "delta_gamma", "fit_1", "diagnostics"), fit = fit_1, prediction_grid = pred_grid)
diagnostics_1$sanity
#hessian_ok is FALSE

#index_1 <- indexwc::calc_index_areas(data = fit_1$data, fit = fit_1, prediction_grid = pred_grid, dir = here::here("additional_runs", "rex_sole", "wcgbts", "delta_gamma", "fit_1", "indices"))

fit_2 <- run_sdmtmb(
  dir_main = NULL,
  data = data_filtered,
  family = sdmTMB::delta_lognormal(),
  formula = configuration_sp$formula[1],
  n_knots = configuration_sp$knots[1],
  share_range = configuration_sp$share_range[1],
  anisotropy = configuration_sp$anisotropy[1],
  spatial = "on",
  spatiotemporal = list("iid", "iid")
)

diagnostics_2 <- indexwc::diagnose(dir = here::here("additional_runs", "rex_sole", "wcgbts", "delta_lognormal", "fit_2", "diagnostics"), fit = fit_2, prediction_grid = pred_grid)
diagnostics_2$sanity
#hessian_ok is FALSE

#index_2 <- indexwc::calc_index_areas(data = fit_2$data, fit = fit_2, prediction_grid = pred_grid, dir = here::here("additional_runs", "rex_sole", "wcgbts", "delta_lognormal", "fit_2", "indices"))

fit_3 <- run_sdmtmb(
  dir_main = NULL,
  data = data_filtered,
  family = sdmTMB::delta_gamma(),
  formula = configuration_sp$formula[1],
  n_knots = configuration_sp$knots[1],
  share_range = TRUE,
  anisotropy = configuration_sp$anisotropy[1],
  spatial = "on",
  spatiotemporal = list("iid", "iid")
)
#original configuration except share_range is TRUE

diagnostics_3 <- indexwc::diagnose(dir = here::here("additional_runs", "rex_sole", "wcgbts", "delta_gamma", "fit_3", "diagnostics"), fit = fit_3, prediction_grid = pred_grid)
diagnostics_3$sanity

index_3 <- indexwc::calc_index_areas(data = fit_3$data, fit = fit_3, prediction_grid = pred_grid, dir = here::here("additional_runs", "rex_sole", "wcgbts", "delta_gamma", "fit_3", "indices"))
#CHOSEN MODEL

###########################################################################
#splitnose rockfish

#filter for sp and source
sp <- "splitnose rockfish"

configuration_sp <- configuration |>
  dplyr::filter(species == sp, source == "NWFSC.Combo")

pulled_data <- nwfscSurvey::pull_catch(
  common_name = sp,
  survey = "NWFSC.Combo")

data_filtered <- format_data(pulled_data) |>
  dplyr::filter(depth <= configuration_sp$min_depth[1], depth >= configuration_sp$max_depth[1],
                latitude >= configuration_sp$min_latitude[1], latitude <= configuration_sp$max_latitude[1],
                year >= configuration_sp$min_year[1], year <= configuration_sp$max_year[1])

pred_grid <- sdmTMB::replicate_df(california_current_grid,
                                  time_name = "year",
                                  time_values = unique(data_filtered$year))

pred_grid$fyear <- as.factor(pred_grid$year)

#original configuration
fit_1 <- run_sdmtmb(
  dir_main = NULL,
  data = data_filtered,
  family = sdmTMB::delta_gamma(),
  formula = configuration_sp$formula[1],
  n_knots = configuration_sp$knots[1],
  share_range = configuration_sp$share_range[1],
  anisotropy = configuration_sp$anisotropy[1],
  spatial = "on",
  spatiotemporal = list("iid", "off")
)

diagnostics_1 <- indexwc::diagnose(dir = here::here("additional_runs", "splitnose_rockfish", "wcgbts", "delta_gamma", "fit_1", "diagnostics"), fit = fit_1, prediction_grid = pred_grid)
diagnostics_1$sanity

fit_2 <- run_sdmtmb(
  dir_main = NULL,
  data = data_filtered,
  family = sdmTMB::delta_gamma(),
  formula = configuration_sp$formula[1],
  n_knots = configuration_sp$knots[1],
  share_range = configuration_sp$share_range[1],
  anisotropy = configuration_sp$anisotropy[1],
  spatial = "on",
  spatiotemporal = list("iid", "iid")
)
#add iid for catch rate model

diagnostics_2 <- indexwc::diagnose(dir = here::here("additional_runs", "splitnose_rockfish", "wcgbts", "delta_gamma", "fit_2", "diagnostics"), fit = fit_2, prediction_grid = pred_grid)
diagnostics_2$sanity
#qq looks better

index_2 <- indexwc::calc_index_areas(data = fit_2$data, fit = fit_2, prediction_grid = pred_grid, dir = here::here("additional_runs", "splitnose_rockfish", "wcgbts", "delta_gamma", "fit_2", "indices"))


fit_3 <- run_sdmtmb(
  dir_main = NULL,
  data = data_filtered,
  family = sdmTMB::delta_lognormal(),
  formula = configuration_sp$formula[1],
  n_knots = configuration_sp$knots[1],
  share_range = configuration_sp$share_range[1],
  anisotropy = configuration_sp$anisotropy[1],
  spatial = "on",
  spatiotemporal = list("iid", "iid")
)

diagnostics_3 <- indexwc::diagnose(dir = here::here("additional_runs", "splitnose_rockfish", "wcgbts", "delta_lognormal", "fit_3", "diagnostics"), fit = fit_3, prediction_grid = pred_grid)
diagnostics_3$sanity
#similar to delta_gamma fit, but qq and aic are improved

index_3 <- indexwc::calc_index_areas(data = fit_3$data, fit = fit_3, prediction_grid = pred_grid, dir = here::here("additional_runs", "splitnose_rockfish", "wcgbts", "delta_lognormal", "fit_3", "indices"))
#chosen model


###############################################################################
#shortspine thornyhead

#filter for sp and source
sp <- "shortspine thornyhead"

configuration_sp <- configuration |>
  dplyr::filter(species == sp, source == "NWFSC.Combo")

pulled_data <- nwfscSurvey::pull_catch(
  common_name = sp,
  survey = "NWFSC.Combo")

data_filtered <- format_data(pulled_data) |>
  dplyr::filter(depth <= configuration_sp$min_depth[1], depth >= configuration_sp$max_depth[1],
                latitude >= configuration_sp$min_latitude[1], latitude <= configuration_sp$max_latitude[1],
                year >= configuration_sp$min_year[1], year <= configuration_sp$max_year[1])

pred_grid <- sdmTMB::replicate_df(california_current_grid,
                                  time_name = "year",
                                  time_values = unique(data_filtered$year))

pred_grid$fyear <- as.factor(pred_grid$year)

#add depth_scaled and depth_scaled_squared
pred_grid$neg_depth <- -pred_grid$depth
mean_neg_depth <- mean(pred_grid$neg_depth)
sd_neg_depth <- sd(pred_grid$neg_depth)
pred_grid$depth_scaled <- - (pred_grid$neg_depth - mean_neg_depth) / sd_neg_depth
pred_grid$depth_scaled_squared <- pred_grid$depth_scaled^2

#original configuration
fit_1 <- run_sdmtmb(
  dir_main = NULL,
  data = data_filtered,
  family = sdmTMB::delta_lognormal(),
  formula = "catch_weight ~ 0 + fyear + pass_scaled + depth_scaled + depth_scaled_squared",
  n_knots = configuration_sp$knots[1],
  share_range = configuration_sp$share_range[1],
  anisotropy = configuration_sp$anisotropy[1],
  spatial = "on",
  spatiotemporal = list("iid","iid")
)

diagnostics_1 <- indexwc::diagnose(dir = here::here("additional_runs", "shortspine_thornyhead", "wcgbts", "delta_lognormal", "fit_1", "diagnostics"), fit = fit_1, prediction_grid = pred_grid)
diagnostics_1$sanity

index_1 <- indexwc::calc_index_areas(data = fit_1$data, fit = fit_1, prediction_grid = pred_grid, dir = here::here("additional_runs", "shortspine_thornyhead", "wcgbts", "delta_lognormal", "fit_1", "indices"))
#chosen model
##################################################################
####################################################################
#longspine thornyhead

#filter for sp and source
sp <- "Dover sole"

configuration_sp <- configuration |>
  dplyr::filter(species == sp, source == "NWFSC.Combo")

pulled_data <- nwfscSurvey::pull_catch(
  common_name = sp,
  survey = "NWFSC.Combo")

data_filtered <- format_data(pulled_data) |>
  dplyr::filter(depth <= configuration_sp$min_depth[1], depth >= configuration_sp$max_depth[1],
                latitude >= configuration_sp$min_latitude[1], latitude <= configuration_sp$max_latitude[1],
                year >= configuration_sp$min_year[1], year <= configuration_sp$max_year[1])

pred_grid <- sdmTMB::replicate_df(california_current_grid,
                                  time_name = "year",
                                  time_values = unique(data_filtered$year))

pred_grid$fyear <- as.factor(pred_grid$year)

#original configuration
fit_1 <- run_sdmtmb(
  dir_main = NULL,
  data = data_filtered,
  family = sdmTMB::delta_gamma(),
  formula = configuration_sp$formula[1],
  n_knots = configuration_sp$knots[1],
  share_range = configuration_sp$share_range[1],
  anisotropy = configuration_sp$anisotropy[1],
  spatial = "on",
  spatiotemporal = list("iid", "iid")
)

diagnostics_1 <- indexwc::diagnose(dir = here::here("additional_runs", "Dover_sole", "wcgbts", "delta_gamma", "fit_1", "diagnostics"), fit = fit_1, prediction_grid = pred_grid)
diagnostics_1$sanity

index_1 <- indexwc::calc_index_areas(data = fit_1$data, fit = fit_1, prediction_grid = pred_grid, dir = here::here("additional_runs", "Dover_sole", "wcgbts", "delta_gamma", "fit_1", "indices"))

fit_2 <- run_sdmtmb(
  dir_main = NULL,
  data = data_filtered,
  family = sdmTMB::delta_gamma(),
  formula = configuration_sp$formula[1],
  n_knots = configuration_sp$knots[1],
  share_range = TRUE,
  anisotropy = configuration_sp$anisotropy[1],
  spatial = "on",
  spatiotemporal = list("iid", "iid")
)
#changed share_range to TRUE

diagnostics_2 <- indexwc::diagnose(dir = here::here("additional_runs", "Dover_sole", "wcgbts", "delta_gamma", "fit_2", "diagnostics"), fit = fit_2, prediction_grid = pred_grid)
diagnostics_2$sanity

index_2 <- indexwc::calc_index_areas(data = fit_2$data, fit = fit_2, prediction_grid = pred_grid, dir = here::here("additional_runs", "Dover_sole", "wcgbts", "delta_gamma", "fit_2", "indices"))


fit_3 <- run_sdmtmb(
  dir_main = NULL,
  data = data_filtered,
  family = sdmTMB::delta_gamma(),
  formula = configuration_sp$formula[1],
  n_knots = configuration_sp$knots[1],
  share_range = TRUE,
  anisotropy = configuration_sp$anisotropy[1],
  spatial = "on",
  spatiotemporal = list("off", "off")
)
#changed share_range to TRUE  and spatiotemporal to off

diagnostics_3 <- indexwc::diagnose(dir = here::here("additional_runs", "Dover_sole", "wcgbts", "delta_gamma", "fit_3", "diagnostics"), fit = fit_3, prediction_grid = pred_grid)
diagnostics_3$sanity

index_3 <- indexwc::calc_index_areas(data = fit_3$data, fit = fit_3, prediction_grid = pred_grid, dir = here::here("additional_runs", "Dover_sole", "wcgbts", "delta_gamma", "fit_3", "indices"))
#####################################################
#rosethorn

#filter for sp and source
sp <- "rosethorn rockfish"

configuration_sp <- configuration |>
  dplyr::filter(species == sp, source == "NWFSC.Combo")

pulled_data <- nwfscSurvey::pull_catch(
  common_name = sp,
  survey = "NWFSC.Combo")

data_filtered <- format_data(pulled_data) |>
  dplyr::filter(depth <= configuration_sp$min_depth[1], depth >= configuration_sp$max_depth[1],
                latitude >= configuration_sp$min_latitude[1], latitude <= configuration_sp$max_latitude[1],
                year >= configuration_sp$min_year[1], year <= configuration_sp$max_year[1])

pred_grid <- sdmTMB::replicate_df(california_current_grid,
                                  time_name = "year",
                                  time_values = unique(data_filtered$year))

pred_grid$fyear <- as.factor(pred_grid$year)

fit_1 <- run_sdmtmb(
  dir_main = NULL,
  data = data_filtered,
  family = sdmTMB::delta_gamma(),
  formula = configuration_sp$formula[1],
  n_knots = configuration_sp$knots[1],
  share_range = configuration_sp$share_range[1],
  anisotropy = configuration_sp$anisotropy[1],
  spatial = "on",
  spatiotemporal = list("iid", "off")
)

diagnostics_1 <- indexwc::diagnose(dir = here::here("additional_runs", "rosethorn_rockfish", "wcgbts", "delta_gamma", "fit_1", "diagnostics"), fit = fit_1, prediction_grid = pred_grid)
diagnostics_1$sanity

index_1 <- indexwc::calc_index_areas(data = fit_1$data, fit = fit_1, prediction_grid = pred_grid, dir = here::here("additional_runs", "rosethorn_rockfish", "wcgbts", "delta_gamma", "fit_1", "indices"))
