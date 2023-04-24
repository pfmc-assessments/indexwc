# TODO list
# * Only pull data once per species and then combine the pull results
#   with the configuration matrix again
#
configuration <- tibble::as_tibble(read.csv(
  file.path("data-raw", "configuration.csv")
))

data <- configuration %>%
  # Row by row ... do stuff then ungroup
  dplyr::rowwise() %>%
  # Pull the data based on the function found in fxn column
  dplyr::mutate(
    data_raw = purrr::map2(
      .x = species,
      .y = source,
      .f = ~ rlang::exec(fxn, .x, .y)
    )
  ) %>%
  dplyr::ungroup() %>%
  # Filter the data based on info in min_* and max_* columns
  dplyr::mutate(
    data_filtered = purrr::pmap(
      .l = list(
        data_raw,
        min_depth, max_depth, min_latitude, max_latitude, min_year, max_year
      ),
      .f = filter_data
    )
  )

best1 <- data %>%
  dplyr::filter(class == "nwfscSurvey") %>%
  dplyr::mutate(
    # Evaluate the call in family
    family = purrr::map(family, .f = ~ eval(parse(text = .x))),
    # Run the model on each row in data
    results = purrr::pmap(
      .l = list(
        data = data_filtered,
        formula = formula,
        family = family
      ),
      .f = indexwc::run,
      anisotropy = TRUE
    )
  )
