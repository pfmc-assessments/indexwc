path <- fs::path(tempdir(), "california_current_grid.rda")
url <- "https://github.com/James-Thorson-NOAA/FishStatsUtils/raw/main/data/california_current_grid.rda"

download.file(
  url = url,
  destfile = path
)
load(path)
unlink(path)

california_current_grid <- california_current_grid %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    pass_scaled = 0,
    vessel_year = 0,
    propInTriennial = ifelse(Lat < 34.5, 0, propInTriennial),
    depth = Depth_km * -1000,
    dplyr::across(
      .cols = dplyr::matches("propIn[WTS]"),
      .fns = ~ ifelse(Ngdc_m > -35, 0, .)
    ),
    dplyr::across(
      .cols = dplyr::matches("propIn[WTS]"),
      .fns = ~ 4 * min(.),
      .names = "{gsub('propIn', 'area_km2_', {col})}"
    )
  )

  california_current_grid <- suppressWarnings(sdmTMB::add_utm_columns(
    california_current_grid,
    utm_crs = utm_zone_10,
    ll_names = c("Lon", "Lat"),
    utm_names = c("x", "y")
  )) %>%
  dplyr::rename(
    longitude = "Lon",
    latitude = "Lat"
  )

usethis::use_data(california_current_grid, overwrite = TRUE)
