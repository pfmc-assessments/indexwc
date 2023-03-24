lookup_grid <- function(x,
                        years,
                        max_latitude,
                        min_latitude,
                        max_longitude,
                        min_longitude,
                        mean_depth,
                        sd_depth,
                        max_depth = Inf,
                        data = california_current_grid) {
  if (missing(max_latitude)) {
    max_latitude <- max(data[["latitude"]], na.rm = TRUE)
  }
  if (missing(min_latitude)) {
    min_latitude <- min(data[["latitude"]], na.rm = TRUE)
  }
  if (missing(max_longitude)) {
    max_longitude <- max(data[["longitude"]], na.rm = TRUE)
  }
  if (missing(min_longitude)) {
    min_longitude <- min(data[["longitude"]], na.rm = TRUE)
  }
  if (missing(mean_depth)) {
    mean_depth <- mean(data[["depth"]], na.rm = TRUE)
  }
  if (missing(sd_depth)) {
    sd_depth <- sd(data[["depth"]], na.rm = TRUE)
  }
  column <- dplyr::case_when(
    grepl("WCGBTS", x) ~ "area_km2_WCGBTS",
    grepl("Triennial", x) ~ "area_km2_Triennial",
    grepl("AFSC_*\\s*Slope", x) ~ "area_km2_Slope98_00",
    grepl("NWFSC_*\\s*Slope", x) ~ "area_km2_Slope02",
    .default = as.character(x)
  ) %>%
    dplyr::sym()

  out <- dplyr::mutate(
    .data = data,
    area_km2 = {{column}},
    vessel_year = "0",
    depth_scaled = scale(depth, center = mean_depth, scale = sd_depth),
    depth_scaled_squared = depth_scaled^2
  ) %>%
    dplyr::filter(
      area_km2 > 0
    )

  out_truncated <- out %>%
    dplyr::filter(
      latitude > min_latitude & latitude < max_latitude,
      longitude > min_longitude & longitude < max_longitude
    )
  stopifnot(NROW(out_truncated) > 0)
  # Transform the coordinates to UTM
  out_utm <- sdmTMB::add_utm_columns(
    out_truncated,
    c("longitude", "latitude"),
    utm_crs = 32610
  ) %>%
    dplyr::select(
      x,
      y,
      area_km2,
      pass_scaled,
      vessel_year,
      longitude,
      latitude,
      depth,
      depth_scaled,
      depth_scaled_squared
    )

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
