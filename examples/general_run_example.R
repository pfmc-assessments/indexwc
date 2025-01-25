# Read in the configuration file within data-raw that define what model set-up
# to apply by species
configuration <- tibble::as_tibble(read.csv(
  file.path("data-raw", "configuration.csv")
))

# Download the data and filter the data based upon species-specific
# depths and latitudes in the configuration file
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
      ))
  ) |>
  dplyr::ungroup()

# Run the model across all species in the configuration file
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

# TODO list
# * Only pull data once per species and then combine the pull results with the
#   configuration matrix again
# * Fix how format_data returns an object without the nwfscSurvey class instead
#   it is class(data) > [1] "tbl_df" "tbl" "data.frame"
# * for vessel_year, might want a different level scaling things might not have
#   to give this to grid

