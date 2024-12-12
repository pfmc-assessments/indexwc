# TODO list
# * Only pull data once per species and then combine the pull results with the
#   configuration matrix again
# * Fix how format_data returns an object without the nwfscSurvey class instead
#   it is class(data) > [1] "tbl_df" "tbl" "data.frame"
# * for vessel_year, might want a different level scaling things might not have
#   to give this to grid

# Things to flag for Chantel:
# 1. spatiotemporal + speed
# 2. move dplyr::mutate(split_conception = ifelse(latitude > 40.1666667, "N", "S"))) to data formatting -- making
# columns available for user
# 3. could automate coefficient mapping
# 4. edit lookup_grid() to add split_conception -- needs to be done for other splits
library(dplyr)
library(indexwc)
configuration <- tibble::as_tibble(read.csv(
  file.path("data-raw", "configuration.csv")
))

configuration <- configuration |>
  dplyr::filter(species == "yellowtail rockfish")

# Change the formula to add year : region interaction
configuration$formula <- "catch_weight ~ 0 + fyear*split_conception + pass_scaled"
configuration$knots <- 400
# configuration$spatiotemporal1 <- "off" # can be turned off just for speed
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
        dplyr::mutate(split_conception = ifelse(latitude > 40.1666667, "N", "S")))
  ) |>
  dplyr::ungroup()

# data("california_current_grid")
# california_current_grid$split_conception <- ifelse(california_current_grid$latitude > 40.1666667, "N", "S")
# usethis::use_data(california_current_grid, overwrite = TRUE)

# Confirm no data in the south in 2007:
dplyr::filter(data$data_filtered[[1]], catch_weight > 0) |>
  dplyr::group_by(split_conception, year) |>
  dplyr::summarise(n = n())

# Find variables that aren't identifiable for presence-absence model
lm <- lm(formula = as.formula(configuration$formula),
         data = data$data_filtered[[1]])
#not_identifiable <- names(which(is.na(coef(lm))))
# Find variables that aren't identifiable for positive model
lm_pos <- lm(formula = as.formula(configuration$formula),
         data = dplyr::filter(data$data_filtered[[1]], catch_weight>0))
pos_not_identifiable <- names(which(is.na(coef(lm_pos))))

# Create variables to be not estimated/ mapped off
# .map <- names(coef(lm))
# .map[names(coef(lm)) %in% not_identifiable] <- NA
# .map <- factor(.map)
# .start <- rep(0, length(coef(lm)))
# .start[names(coef(lm)) %in% not_identifiable] <- -20

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
        share_range = FALSE,
        spatiotemporal = purrr::map2(spatiotemporal1, spatiotemporal2, list),
        sdmtmb_control = list(
          sdmTMB::sdmTMBcontrol(
            map = list(b_j = .map_pos, b_j2 = .map_pos),
            start = list(b_j = .start_pos, b_j2 = .start_pos)
          )
        )
      ),
      .f = indexwc::run_sdmtmb
    )
  )




