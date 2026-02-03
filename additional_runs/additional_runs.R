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
  dplyr::filter(depth <= configuration$min_depth[1], depth >= configuration$max_depth[1],
                latitude >= configuration$min_latitude[1], latitude <= configuration$max_latitude[1],
                year >= configuration$min_year[1], year <= configuration$max_year[1])

pred_grid <- sdmTMB::replicate_df(california_current_grid,
                                  time_name = "year",
                                  time_values = unique(data_filtered$year))

pred_grid$fyear <- as.factor(pred_grid$year)

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

#have not run this yet
##################################################
fit_4 <- run_sdmtmb(
  dir_main = NULL,
  data = data_filtered,
  family = sdmTMB::tweedie(),
  formula = configuration_sp$formula[1],
  n_knots = configuration_sp$knots[1],
  share_range = TRUE,
  anisotropy = TRUE,
  spatial = "on",
  spatiotemporal = list("iid","iid")
)

diagnostics_4 <- indexwc::diagnose(dir = here::here("additional_runs", "longspine_thornyhead", "wcgbts", "tweedie", "fit_4", "diagnostics"), fit = fit_4, prediction_grid = pred_grid)
diagnostics_4$sanity

index_4 <- indexwc::calc_index_areas(data = fit_4$data, fit = fit_4, prediction_grid = pred_grid, dir = here::here("additional_runs", "longspine_thornyhead", "wcgbts", "tweedie", "fit_4", "indices"))

fit_delta_gamma <- run_sdmtmb(
  dir_main = savedir,
  data = data_filtered,
  family = sdmTMB::delta_gamma(),
  formula = configuration_sp$formula[1],
  n_knots = configuration_sp$knots[1],
  share_range = TRUE,
  anisotropy = configuration_sp$anisotropy[1],
  spatial = "on",
  spatiotemporal = "off"
)
#failed

fit_delta_lognormal <- run_sdmtmb(
  dir_main = savedir,
  data = data_filtered,
  family = sdmTMB::delta_lognormal(),
  formula = configuration_sp$formula[1],
  n_knots = configuration_sp$knots[1],
  share_range = TRUE,
  anisotropy = FALSE,
  spatial = "on",
  spatiotemporal = "off"
)

#worked, trying now with the other distribution families
fit_tweedie <- run_sdmtmb(
  dir_main = savedir,
  data = data_filtered,
  family = sdmTMB::tweedie(),
  formula = configuration_sp$formula[1],
  n_knots = configuration_sp$knots[1],
  share_range = TRUE,
  anisotropy = FALSE,
  spatial = "on",
  spatiotemporal = "off"
)

fit_delta_gamma <- run_sdmtmb(
  dir_main = savedir,
  data = data_filtered,
  family = sdmTMB::delta_gamma(),
  formula = configuration_sp$formula[1],
  n_knots = configuration_sp$knots[1],
  share_range = TRUE,
  anisotropy = FALSE,
  spatial = "on",
  spatiotemporal = "off"
)

#delta_gamma model chosen

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
  dplyr::filter(depth <= configuration$min_depth[1], depth >= configuration$max_depth[1],
                latitude >= configuration$min_latitude[1], latitude <= configuration$max_latitude[1],
                year >= configuration$min_year[1], year <= configuration$max_year[1])

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
  dplyr::filter(depth <= configuration$min_depth[1], depth >= configuration$max_depth[1],
                latitude >= configuration$min_latitude[1], latitude <= configuration$max_latitude[1],
                year >= configuration$min_year[1], year <= configuration$max_year[1])

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
  dplyr::filter(depth <= configuration$min_depth[1], depth >= configuration$max_depth[1],
                latitude >= configuration$min_latitude[1], latitude <= configuration$max_latitude[1],
                year >= configuration$min_year[1], year <= configuration$max_year[1])

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


