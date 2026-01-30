library(dplyr)
library(nwfscSurvey)
library(indexwc)
library(ggplot2)
library(png)
library(grid)
library(here)

configuration <- configuration

savedir <- here::here("additional_runs")

pred_grid <- sdmTMB::replicate_df(california_current_grid,
                                  time_name = "year",
                                  time_values = unique(data$year))
pred_grid$fyear <- as.factor(pred_grid$year)

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


fit_tweedie <- run_sdmtmb(
  dir_main = savedir,
  data = data_filtered,
  family = sdmTMB::tweedie(),
  formula = configuration_sp$formula[1],
  n_knots = configuration_sp$knots[1],
  share_range = configuration_sp$share_range[1],
  anisotropy = configuration_sp$anisotropy[1],
  spatial = "on",
  spatiotemporal = "off"
)
#failed

fit_delta_gamma <- run_sdmtmb(
  dir_main = savedir,
  data = data_filtered,
  family = sdmTMB::delta_gamma(),
  formula = configuration_sp$formula[1],
  n_knots = configuration_sp$knots[1],
  share_range = configuration_sp$share_range[1],
  anisotropy = configuration_sp$anisotropy[1],
  spatial = "on",
  spatiotemporal = "off"
)
#failed

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
configuration_sp$max_latitude <- 49

pulled_data <- nwfscSurvey::pull_catch(
  common_name = sp,
  survey = "NWFSC.Combo")

data_filtered <- format_data(pulled_data) |>
  dplyr::filter(depth <= configuration_sp$min_depth[1], depth >= configuration_sp$max_depth[1],
                latitude >= configuration_sp$min_latitude[1], latitude <= configuration_sp$max_latitude[1],
                year >= configuration_sp$min_year[1], year <= configuration_sp$max_year[1])


fit_delta_gamma <- run_sdmtmb(
  dir_main = savedir,
  data = data_filtered,
  family = sdmTMB::delta_gamma(),
  formula = configuration_sp$formula[1],
  n_knots = configuration_sp$knots[1],
  share_range = configuration_sp$share_range[1],
  anisotropy = configuration_sp$anisotropy[1],
  spatial = "on",
  spatiotemporal = "iid"
)
#failed

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

#check qq

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
#going with lognormal

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


fit_1 <- run_sdmtmb(
  dir_main = savedir,
  data = data_filtered,
  family = sdmTMB::delta_lognormal(), #configuration_sp$family[1] this doesn't work for some reason
  formula = configuration_sp$formula[1],
  n_knots = configuration_sp$knots[1],
  share_range = configuration_sp$share_range[1],
  anisotropy = configuration_sp$anisotropy[1],
  spatial = "on",
  spatiotemporal = "iid" #c("iid","iid") and #c(configuration_sp$spatiotemporal1, configuration_sp$spatiotemporal2) does not work for some reason
)
#failed

fit_2 <- run_sdmtmb(
  dir_main = savedir,
  data = data_filtered,
  family = sdmTMB::delta_lognormal(),
  formula = configuration_sp$formula[1],
  n_knots = configuration_sp$knots[1],
  share_range = TRUE,
  anisotropy = configuration_sp$anisotropy[1],
  spatial = "on",
  spatiotemporal = list("iid", "iid")
)
#the only thing I have changed here is share_rage is now true
#also failed

fit_3 <- run_sdmtmb(
  dir_main = savedir,
  data = data_filtered,
  family = sdmTMB::delta_lognormal(),
  formula = configuration_sp$formula[1],
  n_knots = configuration_sp$knots[1],
  share_range = TRUE,
  anisotropy = configuration_sp$anisotropy[1],
  spatial = "on",
  spatiotemporal = list("iid", "off")
)
#the only thing I have changed here is share_range is now true and spatiotemporal2 is off
#also failed

fit_4 <- run_sdmtmb(
  dir_main = savedir,
  data = data_filtered,
  family = sdmTMB::delta_lognormal(),
  formula = configuration_sp$formula[1],
  n_knots = configuration_sp$knots[1],
  share_range = TRUE,
  anisotropy = configuration_sp$anisotropy[1],
  spatial = "on",
  spatiotemporal = list("off", "off")
)
#the only thing I have changed here is share_range is now true and spatiotemporal2 is off and spatiotemportal1 is off
#ran, but gradients check failed will try with other families

fit_5 <- run_sdmtmb(
  dir_main = savedir,
  data = data_filtered,
  family = sdmTMB::delta_gamma(),
  formula = configuration_sp$formula[1],
  n_knots = configuration_sp$knots[1],
  share_range = TRUE,
  anisotropy = configuration_sp$anisotropy[1],
  spatial = "on",
  spatiotemporal = list("off", "off")
)
#the only thing I changed was distribution family
#failed

fit_6 <- run_sdmtmb(
  dir_main = savedir,
  data = data_filtered,
  family = sdmTMB::delta_lognormal(),
  formula = configuration_sp$formula[1],
  n_knots = configuration_sp$knots[1],
  share_range = TRUE,
  anisotropy = FALSE,
  spatial = "on",
  spatiotemporal = list("off", "off")
)
#back to lognormal, now also turing off ansotropy, but my guess is that we will want to retain that model even if gradients are on bounds?
#failed


###############################################
#test

sp <- "arrowtooth flounder"

configuration_sp <- configuration |>
  dplyr::filter(species == sp, source == "NWFSC.Combo")

pulled_data <- nwfscSurvey::pull_catch(
  common_name = sp,
  survey = "NWFSC.Combo")

data_filtered <- format_data(pulled_data) |>
  dplyr::filter(depth <= configuration$min_depth[1], depth >= configuration$max_depth[1],
                latitude >= configuration$min_latitude[1], latitude <= configuration$max_latitude[1],
                year >= configuration$min_year[1], year <= configuration$max_year[1])

fit <- run_sdmtmb(
  dir_main = savedir,
  data = data_filtered,
  family = sdmTMB::delta_gamma(),
  formula = configuration_sp$formula[1],
  n_knots = configuration_sp$knots[1],
  share_range = configuration_sp$share_range[1],
  anisotropy = configuration_sp$anisotropy[1],
  spatial = "on",
  spatiotemporal = "iid"
)

output <- indexwc::calc_index_areas(data = data, fit = fit, prediction_grid = pred_grid, dir = here::here("additional_runs", "arrowtooth_flounder", "wcgbts", "delta_gamma", "index"))

#not working
diagnostics <- indexwc::diagnose(dir = NULL, fit = fit, prediction_grid = NULL)

#finally got it working, EXCEPT FOR DIAGNOSTICS
####################################################
