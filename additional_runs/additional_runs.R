library(dplyr)
library(nwfscSurvey)
library(indexwc)
library(ggplot2)
library(png)
library(grid)

savedir <- here::here("additional_runs")

#filter for sp and source
sp <- "longspine thornyhead"

#load configuration rda file!!
configuration <- configuration |>
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
  formula = configuration$formula[1],
  n_knots = configuration$knots[1],
  share_range = configuration$share_range[1],
  anisotropy = configuration$anisotropy[1],
  spatial = "on",
  spatiotemporal = "off"
)
#failed

fit_delta_gamma <- run_sdmtmb(
  dir_main = savedir,
  data = data_filtered,
  family = sdmTMB::delta_gamma(),
  formula = configuration$formula[1],
  n_knots = configuration$knots[1],
  share_range = configuration$share_range[1],
  anisotropy = configuration$anisotropy[1],
  spatial = "on",
  spatiotemporal = "off"
)
#failed

fit_delta_gamma <- run_sdmtmb(
  dir_main = savedir,
  data = data_filtered,
  family = sdmTMB::delta_gamma(),
  formula = configuration$formula[1],
  n_knots = configuration$knots[1],
  share_range = TRUE,
  anisotropy = configuration$anisotropy[1],
  spatial = "on",
  spatiotemporal = "off"
)
#failed

fit_delta_lognormal <- run_sdmtmb(
  dir_main = savedir,
  data = data_filtered,
  family = sdmTMB::delta_lognormal(),
  formula = configuration$formula[1],
  n_knots = configuration$knots[1],
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
  formula = configuration$formula[1],
  n_knots = configuration$knots[1],
  share_range = TRUE,
  anisotropy = FALSE,
  spatial = "on",
  spatiotemporal = "off"
)

fit_delta_gamma <- run_sdmtmb(
  dir_main = savedir,
  data = data_filtered,
  family = sdmTMB::delta_gamma(),
  formula = configuration$formula[1],
  n_knots = configuration$knots[1],
  share_range = TRUE,
  anisotropy = FALSE,
  spatial = "on",
  spatiotemporal = "off"
)

#delta_gamma model chosen

########################################################



