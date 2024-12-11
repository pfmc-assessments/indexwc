# TODO list
# * Only pull data once per species and then combine the pull results with the
#   configuration matrix again
# * Fix how format_data returns an object without the nwfscSurvey class instead
#   it is class(data) > [1] "tbl_df" "tbl" "data.frame"
# * for vessel_year, might want a different level scaling things might not have
#   to give this to grid
configuration <- tibble::as_tibble(read.csv(
  file.path("data-raw", "configuration.csv")
))

configuration <- configuration |>
  dplyr::filter(species == "yellowtail rockfish")

# Change the formula to add year : region interaction
configuration$formula <- "catch_weight ~ 0 + fyear*region + pass_scaled"
configuration$knots <- 400
configuration$spatiotemporal2 <- "off" # based on EW's initial modeling, these worked best

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
        dplyr::mutate(region = ifelse(latitude > 40.1666667, "N", "S")))
  ) |>
  dplyr::ungroup()

# Confirm no data in the south in 2007:
dplyr::filter(data$data_filtered[[1]], catch_weight > 0) |>
  dplyr::group_by(region, year) |>
  dplyr::summarise(n = n())

# Find variables that aren't identifiable for presence-absence model
lm <- lm(formula = as.formula(configuration$formula),
         data = data$data_filtered[[1]])
not_identifiable <- names(which(is.na(coef(lm))))
# Find variables that aren't identifiable for positive model
lm_pos <- lm(formula = as.formula(configuration$formula),
         data = dplyr::filter(data$data_filtered[[1]], catch_weight>0))
pos_not_identifiable <- names(which(is.na(coef(lm_pos))))


.map <- names(coef(lm))
.map[names(coef(lm)) %in% not_identifiable] <- NA
.map <- factor(.map)
.start <- rep(0, length(coef(lm)))
.start[names(coef(lm)) %in% not_identifiable] <- -20

.map_pos <- names(coef(lm_pos))
.map_pos[names(coef(lm_pos)) %in% pos_not_identifiable] <- NA
.map_pos <- factor(.map)
.start_pos <- rep(0, length(coef(lm_pos)))
.start_pos[names(coef(lm_pos)) %in% pos_not_identifiable] <- -20

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
        share_range = FALSE,
        spatiotemporal = purrr::map2(spatiotemporal1, spatiotemporal2, list),
        sdmtmb_control = list(
          sdmTMB::sdmTMBcontrol(
            map = list(b_j = .map, b_j2 = .map_pos),
            start = list(b_j = .start, b_j2 = .start_pos)
          )
        )
      ),
      .f = indexwc::run_sdmtmb
    )
  )




