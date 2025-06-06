#' Lookup the prediction grid
#'
#' @param x The raw survey data
#' @param years The years to use for predictions
#' @param max_latitude The maximum latitude for predictions
#' @param min_latitude The minimum latitude for predictions
#' @param max_longitude The maximum longitude for predictions
#' @param min_longitude The minimum longitude for predictions
#' @param mean_depth The mean depth to use
#' @param sd_depth The sd of depth to use
#' @param max_depth Maximum depth, defaults to Inf
#' @param data the name of the grid, defaults to california_current_grid (WCGBTS)
#' @export
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
    area_km2 = {{ column }},
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
      longitude > min_longitude & longitude < max_longitude,
      depth < max_depth
    )
  stopifnot(NROW(out_truncated) > 0)
  # Transform the coordinates to UTM
  out_utm <- suppressWarnings(sdmTMB::add_utm_columns(
    out_truncated,
    c("longitude", "latitude"),
    utm_crs = utm_zone_10
  )) %>%
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
      depth_scaled_squared,
      split_mendocino,
      split_conception,
      split_monterey,
      split_state
    )

  year_grid <- purrr::map_dfr(
    .x = years,
    .f = function(year_i, data) {
      data[["year"]] <- year_i
      data
    },
    data = out_utm
  ) %>%
    dplyr::mutate(
      fyear = as.factor(year)
    )

  return(year_grid)
}
