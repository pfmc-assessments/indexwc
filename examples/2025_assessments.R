library(dplyr)
library(indexwc)

savedir <- here::here("2025")

configuration_all <- tibble::as_tibble(read.csv(
  file.path("data-raw", "configuration.csv")
))


# Tweaking stuff for wcgbt widow index
configuration_all[which(configuration_all$species == "widow rockfish" &
                        configuration_all$source == "NWFSC.Combo"), "knots"] <- 50

configuration_all[which(configuration_all$species == "widow rockfish" &
                          configuration_all$source == "NWFSC.Combo"), "spatiotemporal2"] <- "iid"


#===============================================================================
# NWFSC Combo All default setting species
#===============================================================================

wcgbt_species_list <- c(
  "sablefish",
  "chilipepper",
  "widow rockfish",
  "yelloweye rockfish",
  "rougheye rockfish",
  "rougheye and blackpotted rockfish"
)

configuration_sub <- configuration_all |>
  dplyr::filter(source == "NWFSC.Combo", species %in% wcgbt_species_list)

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
                               ) |>
         dplyr::mutate(
           split_mendocino = 1,
           split_conception = 1,
           split_monterey = 1,
           split_state = 1))
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


#===============================================================================
# Yellowtail rockfish north - WCGBT
#===============================================================================

configuration <- configuration_all |>
  dplyr::filter(species == "yellowtail rockfish")
# Change the covariates to include a split at cape mendocino
configuration$formula <- "catch_weight ~ 0 + fyear*split_mendocino + pass_scaled"

data <- configuration |>
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
                           dplyr::mutate(split_mendocino = ifelse(latitude > 40.1666667, "N", "S")))
  ) |>
  dplyr::ungroup()

# Confirm no data in the south in 2007:
dplyr::filter(data$data_filtered[[1]], catch_weight > 0) |>
  dplyr::group_by(split_mendocino, year) |>
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


#===============================================================================
# NWFSC Slope
#===============================================================================
nw_slope_species_list <- c(
  "sablefish",
  "chilipepper",
  "rougheye rockfish"
)

configuration_sub <- configuration_all |>
  dplyr::filter(source == "NWFSC.Slope", species %in% nw_slope_species_list)

for(sp in nw_slope_species_list){
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

#===============================================================================
# AFSC Slope
#===============================================================================
af_slope_species_list <- c(
  "rougheye and blackspotted rockfish"
)

configuration_sub <- configuration_all |>
  dplyr::filter(source == "AFSC.Slope", species %in% af_slope_species_list)
configuration_sub$anisotropy <- FALSE

for(sp in af_slope_species_list){
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
                               ) |>
                               dplyr::mutate(
                                 split_mendocino = 1,
                                 split_conception = 1,
                                 split_monterey = 1,
                                 split_state = 1))
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

#===============================================================================
# AFSC Slope - sablefish tweedie
#===============================================================================
savedir <- here::here("2025")
sp <- "sablefish"

configuration <- configuration_all |>
  dplyr::filter(species == sp, source == "AFSC.Slope", family == "sdmTMB::tweedie()")

pulled_data <- nwfscSurvey::pull_catch(
  common_name = sp,
  survey = "AFSC.Slope",
  years = c(1997, 2050)
)

data_filtered <- format_data.nwfscSurvey(pulled_data)

fit <- run_sdmtmb(
  dir_main = savedir,
  data = data_filtered,
  family = sdmTMB::tweedie(link = "log"),
  formula = configuration$formula[1],
  n_knots = configuration$knots[1],
  share_range = configuration$share_range[1],
  anisotropy = configuration$anisotropy[1],
  spatial = "on",
  spatiotemporal = "off"
)

#===============================================================================
# Triennial early
#===============================================================================

savedir <- file.path(here::here(), "2025", "triennial-early")

tri_survey_sp <- c(
  "sablefish",
  "chilipepper",
  "rougheye and blackspotted rockfish",
  "yellowtail rockfish")

configuration_sub <- configuration_all |>
  dplyr::filter(source == "Triennial",
                species %in% tri_survey_sp)
configuration_sub$max_year <- 1992

for(sp in tri_survey_sp ){
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
                               ) |>
                               dplyr::mutate(split_mendocino = ifelse(latitude > 40.1666667, "N", "S")))
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

#===============================================================================
# Triennial late
#===============================================================================
savedir <- file.path(here::here(), "2025", "triennial-late")

tri_late_survey_sp <- c(
  "sablefish",
  "chilipepper",
  "yellowtail rockfish")
# rougheye not included due to the species being listed under two names for the
# late triennial time period

configuration_sub <- configuration_all |>
  dplyr::filter(source == "Triennial",
                species %in% tri_late_survey_sp)
configuration_sub$min_year <- 1995

for(sp in tri_late_survey_sp){
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
                               ) |>
                               dplyr::mutate(split_mendocino = ifelse(latitude > 40.1666667, "N", "S")))
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

#===============================================================================
# Triennial early
#===============================================================================

savedir <- file.path(here::here(), "2025", "triennial-early")

tri_survey_sp <- c(
  "sablefish",
  "chilipepper",
  "rougheye and blackspotted rockfish",
  "yellowtail rockfish")

configuration_sub <- configuration_all |>
  dplyr::filter(source == "Triennial",
                species %in% tri_survey_sp)
configuration_sub$max_year <- 1992

for(sp in tri_survey_sp){
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
                               ) |>
                               dplyr::mutate(split_mendocino = ifelse(latitude > 40.1666667, "N", "S")))
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

#===============================================================================
# Triennial full - rougheye
#===============================================================================
sp <- "rougheye and blackspotted rockfish"

configuration <- configuration_sub |>
  dplyr::filter(species == sp, source == "Triennial")

pulled_data <- nwfscSurvey::pull_catch(
  common_name = c("rougheye and blackspotted rockfish", "rougheye rockfish"),
  survey = "Triennial",
  years = c(1980, 2004)
  )

data_filtered <- format_data.nwfscSurvey(pulled_data) |>
  dplyr::filter(latitude > 42)

fit <- run_sdmtmb(
  dir_main = savedir,
  data = data_filtered,
  family = sdmTMB::delta_lognormal(),
  formula = configuration$formula[1],
  n_knots = configuration$knots[1],
  share_range = configuration$share_range[1],
  anisotropy = configuration$anisotropy[1],
  spatial = "off",
  spatiotemporal = "iid"
)

fit <- run_sdmtmb(
  dir_main = savedir,
  data = data_filtered,
  family = sdmTMB::delta_gamma(),
  formula = configuration$formula[1],
  n_knots = configuration$knots[1],
  share_range = configuration$share_range[1],
  anisotropy = configuration$anisotropy[1],
  spatial = "off",
  spatiotemporal = "iid"
)

#===============================================================================
# Triennial late - rougheye
#===============================================================================
savedir <- file.path(here::here(), "2025", "triennial-late")
sp <- "rougheye and blackspotted rockfish"

configuration_sub <- configuration_all |>
  dplyr::filter(source == "Triennial", species == sp)

pulled_data <- nwfscSurvey::pull_catch(
  common_name = c("rougheye and blackspotted rockfish", "rougheye rockfish"),
  survey = "Triennial",
  years = c(1995, 2004)
)

data_filtered <- format_data.nwfscSurvey(pulled_data)

fit <- run_sdmtmb(
  dir_main = savedir,
  data = data_filtered,
  family = sdmTMB::delta_lognormal(),
  formula = configuration$formula[1],
  n_knots = configuration$knots[1],
  share_range = configuration$share_range[1],
  anisotropy = configuration$anisotropy[1],
  spatial = "off",
  spatiotemporal = "iid"
)

fit <- run_sdmtmb(
  dir_main = savedir,
  data = data_filtered,
  family = sdmTMB::delta_gamma(),
  formula = configuration$formula[1],
  n_knots = configuration$knots[1],
  share_range = configuration$share_range[1],
  anisotropy = configuration$anisotropy[1],
  spatial = "off",
  spatiotemporal = "iid"
)

#===============================================================================
# Yellowtail rockfish north - Triennial full
#===============================================================================
configuration <- configuration_all |>
  dplyr::filter(species == "yellowtail rockfish" & source == "Triennial")

data <- configuration |>
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
                           dplyr::mutate(split_mendocino = ifelse(latitude > 40.1666667, "N", "S")))
  ) |>
  dplyr::ungroup()

# Confirm no data in the south in 2007:
dplyr::filter(data$data_filtered[[1]], catch_weight > 0) |>
  dplyr::group_by(split_mendocino, year) |>
  dplyr::summarise(n = dplyr::n())

# Find variables that aren't identifiable for presence-absence model
lm <- lm(formula = as.formula(configuration$formula),
         data = data$data_filtered[[1]])
#not_identifiable <- names(which(is.na(coef(lm))))
# Find variables that aren't identifiable for positive model
lm_pos <- lm(formula = as.formula(configuration$formula),
             data = dplyr::filter(data$data_filtered[[1]], catch_weight>0))
#pos_not_identifiable <- names(which(is.na(coef(lm_pos))))

best <- data |>
  dplyr::mutate(
    # Evaluate the call in family
    family = purrr::map(family, .f = ~ eval(parse(text = .x))),
    # Run the model on each row in data
    results = purrr::pmap(
      .l = list(
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



