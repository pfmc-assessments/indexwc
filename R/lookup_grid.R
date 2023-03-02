lookup_grid <- function(x,
                        years,
                        min_longitude,
                        max_depth = Inf,
                        data = california_current_grid) {
  if (missing(min_longitude)) {
    min_longitude <- min(data[["longitude"]])
  }
  column <- dplyr::case_when(
    grepl("WCGBTS", x) ~ "area_km2_WCGBTS",
    grepl("Triennial", x) ~ "area_km2_Triennial",
    # TODO: not sure about this one
    grepl("AFSC Slope", x) ~ "area_km2_Slope98_00"
  ) %>%
    dplyr::sym()

  out <- data %>%
    dplyr::mutate(
      area_km2 = {{column}},
      vessel_year = "0"
    ) %>%
    dplyr::filter(
      area_km2 > 0
    )

  out_truncated <- out %>%
    dplyr::filter(longitude > min_longitude)

  # Transform the coordinates to UTM
  out_utm <- sdmTMB::add_utm_columns(
    out_truncated,
    c("longitude", "latitude"),
    utm_crs = 32610
  ) %>%
    dplyr::select(x, y, area_km2, pass_scaled, vessel_year, longitude, latitude, depth)

  year_grid <- purrr::map_dfr(
    .x = years,
    .f = function(year_i, data) {
      data[["year"]] <- year_i
      data
    },
    data = out_utm
  )

  return(year_grid)
}
