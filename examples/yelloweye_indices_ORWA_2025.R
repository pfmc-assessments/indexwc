library(dplyr)
library(nwfscSurvey)
library(indexwc)
library(here)

#load boundaries_data, california_current_grid, and configuration rda files that are in the data folder

configuration_all <- configuration

#####################################################
############   OPTION 1  ###########################
####################################################

#===============================================================================
# set min lat to 42 to have estimate that is just OR & WA
# like was in 2017 yelloweye assessment
#===============================================================================
savedir <- here::here("2025/yelloweye_split_42_point")

wcgbt_species_list <- c(
  "yelloweye rockfish"
)

configuration_sub <- configuration_all |>
  dplyr::filter(source == "NWFSC.Combo",
                species %in% wcgbt_species_list,
                formula == "catch_weight ~ 0 + fyear + pass_scaled")

configuration_sub$min_latitude <- 42.0

for(sp in wcgbt_species_list){
  configuration <- configuration_sub |>
    dplyr::filter(species == sp)

  for(run in 1:nrow(configuration)) {
    data <- configuration[run, ] |>
      # Row by row ... do stuff then ungroup
      dplyr::rowwise() |>
      # Pull the data based on the function found in fxn column
      dplyr::mutate(
        data_raw = list(format_data(eval(parse(text = fxn)))),
        data_filtered = list(data_raw |>
                               dplyr::filter(
                                 depth <= min_depth, depth >= max_depth,
                                 latitude >= min_latitude, latitude <= max_latitude,
                                 year >= min_year, year <= max_year
                               ))
      ) |>
      dplyr::ungroup()

    best <- data |>
      dplyr::mutate(
        # Evaluate the call in family
        family = purrr::map(family, .f = ~ eval(parse(text = .x))),
        # Run the model on each row in data
        results = purrr::pmap(
          .l = list(
            dir_main = savedir,
            data = data_filtered,
            formula = formula,
            family = family,
            anisotropy = anisotropy,
            n_knots = knots,
            share_range = share_range,
            spatiotemporal = purrr::map2(spatiotemporal1, spatiotemporal2, list)
          ),
          .f = indexwc::run_sdmtmb
        )
      )
  }
}

###################################################
# Option 1 with anisotropy = FALSE
# set min lat to 42 to have estimate that is just OR & WA
# like was in 2017 yelloweye assessment
###################################################
savedir <- here::here("2025/yelloweye_split_42_point_anisotropy")

wcgbt_species_list <- c(
  "yelloweye rockfish"
)

configuration_sub <- configuration_all |>
  dplyr::filter(source == "NWFSC.Combo",
                species %in% wcgbt_species_list,
                formula == "catch_weight ~ 0 + fyear + pass_scaled")

configuration_sub$min_latitude <- 42.0
configuration_sub$anisotropy <- FALSE

for(sp in wcgbt_species_list){
  configuration <- configuration_sub |>
    dplyr::filter(species == sp)

  for(run in 1:nrow(configuration)) {
    data <- configuration[run, ] |>
      # Row by row ... do stuff then ungroup
      dplyr::rowwise() |>
      # Pull the data based on the function found in fxn column
      dplyr::mutate(
        data_raw = list(format_data(eval(parse(text = fxn)))),
        data_filtered = list(data_raw |>
                               dplyr::filter(
                                 depth <= min_depth, depth >= max_depth,
                                 latitude >= min_latitude, latitude <= max_latitude,
                                 year >= min_year, year <= max_year
                               ))
      ) |>
      dplyr::ungroup()

    best <- data |>
      dplyr::mutate(
        # Evaluate the call in family
        family = purrr::map(family, .f = ~ eval(parse(text = .x))),
        # Run the model on each row in data
        results = purrr::pmap(
          .l = list(
            dir_main = savedir,
            data = data_filtered,
            formula = formula,
            family = family,
            anisotropy = anisotropy,
            n_knots = knots,
            share_range = share_range,
            spatiotemporal = purrr::map2(spatiotemporal1, spatiotemporal2, list)
          ),
          .f = indexwc::run_sdmtmb
        )
      )
  }
}


#####################################################
############   OPTION 2  ###########################
####################################################

#===============================================================================
# Yelloweye rockfish ORWA
# Code to run WCGBT index for yelloweye rockfish
# for ORWA (OR & WA) that includes year*area based interactions.  Designed to
# improve estimates along the boundaries (e.g., 42.0).
#===============================================================================

savedir <- here::here("2025/yelloweye_split_42_informed")

configuration <- configuration_all |>
  dplyr::filter(source == "NWFSC.Combo",
                species == "yelloweye rockfish",
                formula == "catch_weight ~ 0 + fyear*split_42 + pass_scaled")


for (run in 1:nrow(configuration)){
  data <- configuration[run, ] |>
    # Row by row ... do stuff then ungroup
    dplyr::rowwise() |>
    # Pull the data based on the function found in fxn column
    dplyr::mutate(
      data_raw = list(format_data(eval(parse(text = fxn)))),
      data_filtered = list(data_raw |>
                             dplyr::filter(
                               depth <= min_depth, depth >= max_depth,
                               latitude >= min_latitude, latitude <= max_latitude,
                               year >= min_year, year <= max_year
                             ) |>
                             dplyr::mutate(split_42 = ifelse(latitude > 42.0, "ORWA", "CA")))
    ) |>
    dplyr::ungroup()

  # Confirm no data in the south in 2007:
  dplyr::filter(data$data_filtered[[1]], catch_weight > 0) |>
    dplyr::group_by(split_42, year) |>
    dplyr::summarise(n = dplyr::n())

  # Find variables that aren't identifiable for presence-absence model
  lm <- lm(formula = as.formula(configuration$formula),
           data = data$data_filtered[[1]])
  #not_identifiable <- names(which(is.na(coef(lm))))
  # Find variables that aren't identifiable for positive model
  lm_pos <- lm(formula = as.formula(configuration$formula),
               data = dplyr::filter(data$data_filtered[[1]], catch_weight>0))
  pos_not_identifiable <- names(which(is.na(coef(lm_pos))))

  # Create variables to be not estimated/ mapped off
  coef_names <- names(coef(lm))
  .map_pos <- coef_names
  .map_pos[coef_names %in% pos_not_identifiable] <- NA
  .map_pos <- factor(.map_pos)
  .start_pos <- rep(0, length(coef_names))
  .start_pos[coef_names %in% pos_not_identifiable] <- -20

  best <- data |>
    dplyr::mutate(
      # Evaluate the call in family
      family = purrr::map(family, .f = ~ eval(parse(text = .x))),
      # Run the model on each row in data
      results = purrr::pmap(
        .l = list(
          dir_main = savedir,
          data = data_filtered,
          formula = formula,
          family = family,
          anisotropy = anisotropy,
          n_knots = knots,
          share_range = share_range,
          spatiotemporal = purrr::map2(spatiotemporal1, spatiotemporal2, list),
          sdmtmb_control = list(
            sdmTMB::sdmTMBcontrol(
              map = list(b_j = .map_pos, b_j2 = .map_pos),
              start = list(b_j = .start_pos, b_j2 = .start_pos),
              newton_loops = 3
            )
          )
        ),
        .f = indexwc::run_sdmtmb
      )
    )
}

###############################################
###with anisotropy off
############################################

savedir <- here::here("2025/yelloweye_split_42_informed_offanisotropy")

configuration <- configuration_all |>
  dplyr::filter(source == "NWFSC.Combo",
                species == "yelloweye rockfish",
                formula == "catch_weight ~ 0 + fyear*split_42 + pass_scaled")


for (run in 1:nrow(configuration)){
  data <- configuration[run, ] |>
    # Row by row ... do stuff then ungroup
    dplyr::rowwise() |>
    # Pull the data based on the function found in fxn column
    dplyr::mutate(
      data_raw = list(format_data(eval(parse(text = fxn)))),
      data_filtered = list(data_raw |>
                             dplyr::filter(
                               depth <= min_depth, depth >= max_depth,
                               latitude >= min_latitude, latitude <= max_latitude,
                               year >= min_year, year <= max_year
                             ) |>
                             dplyr::mutate(split_42 = ifelse(latitude > 42.0, "ORWA", "CA")))
    ) |>
    dplyr::ungroup()

  # Confirm no data in the south in 2007:
  dplyr::filter(data$data_filtered[[1]], catch_weight > 0) |>
    dplyr::group_by(split_42, year) |>
    dplyr::summarise(n = dplyr::n())

  # Find variables that aren't identifiable for presence-absence model
  lm <- lm(formula = as.formula(configuration$formula),
           data = data$data_filtered[[1]])
  #not_identifiable <- names(which(is.na(coef(lm))))
  # Find variables that aren't identifiable for positive model
  lm_pos <- lm(formula = as.formula(configuration$formula),
               data = dplyr::filter(data$data_filtered[[1]], catch_weight>0))
  pos_not_identifiable <- names(which(is.na(coef(lm_pos))))

  # Create variables to be not estimated/ mapped off
  coef_names <- names(coef(lm))
  .map_pos <- coef_names
  .map_pos[coef_names %in% pos_not_identifiable] <- NA
  .map_pos <- factor(.map_pos)
  .start_pos <- rep(0, length(coef_names))
  .start_pos[coef_names %in% pos_not_identifiable] <- -20

  best <- data |>
    dplyr::mutate(
      # Evaluate the call in family
      family = purrr::map(family, .f = ~ eval(parse(text = .x))),
      # Run the model on each row in data
      results = purrr::pmap(
        .l = list(
          dir_main = savedir,
          data = data_filtered,
          formula = formula,
          family = family,
          anisotropy = FALSE,
          n_knots = knots,
          share_range = share_range,
          spatiotemporal = purrr::map2(spatiotemporal1, spatiotemporal2, list),
          sdmtmb_control = list(
            sdmTMB::sdmTMBcontrol(
              map = list(b_j = .map_pos, b_j2 = .map_pos),
              start = list(b_j = .start_pos, b_j2 = .start_pos),
              newton_loops = 3
            )
          )
        ),
        .f = indexwc::run_sdmtmb
      )
    )
}



