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

data <- configuration %>%
  # Row by row ... do stuff then ungroup
  dplyr::rowwise() %>%
  # Pull the data based on the function found in fxn column
  dplyr::mutate(
    data_raw = list(format_data(eval(parse(text = fxn)))),
    data_filtered = list(data_raw %>%
      dplyr::filter(
        depth <= min_depth, depth >= max_depth,
        latitude >= min_latitude, latitude <= max_latitude,
        year >= min_year, year <= max_year
      ))
  ) %>%
  dplyr::ungroup()

best <- data[-3, ] %>%
  dplyr::mutate(
    # Evaluate the call in family
    family = purrr::map(family, .f = ~ eval(parse(text = .x))),
    # Run the model on each row in data
    results = purrr::pmap(
      .l = list(
        data = data_filtered,
        formula = formula,
        family = family,
        anisotropy = anisotropy
      ),
      .f = indexwc::run
    )
  )
best2 <- data[c(3), ] %>%
  dplyr::mutate(
    # Evaluate the call in family
    family = purrr::map(family, .f = ~ eval(parse(text = .x))),
    # Run the model on each row in data
    results = purrr::pmap(
      .l = list(
        data = data_filtered,
        formula = formula,
        family = family,
        anisotropy = anisotropy
      ),
      spatiotemporal = list("iid", "off"),
      n_knots = 200,
      .f = indexwc::run
    )
  )

indices <- dir(
  file.path("canary_rockfish", "wcgbts"),
  pattern = "sdmTMB_save",
  recursive = TRUE,
  full.names = TRUE
) %>%
  unlist() %>%
  purrr::map_df(
    .f = function(x) {
      load(x)
      if (exists("index_areas")) {
        return(data.frame(i = x, index_areas))
        } else {
          return(NULL)
        }
    }
  )
gg <- ggplot2::ggplot(
 data = indices %>%
  dplyr::mutate(
    dist = basename(dirname(dirname(i)))
  ) %>%
  dplyr::filter(area == "coastwide"),
  ggplot2::aes(x = year, y = est, lty = dist, col = dist, group = i)
) +
ggplot2::geom_line() +
ggplot2::theme_bw()
ggsave(gg, filename = "indexwc_copper_rockfish.png")
