#' Run [sdmTMB::sdmTMB]
#'
#' @template dir
#' @template data
#' @inheritParams sdmTMB::sdmTMB
#' @param n_knots An integer specifying the number of knots you want in your
#'   mesh that is created by {INLA}. More knots is not always better.
#'
#' @export
#' @author Chantel R. Wetzel
#' @return
#' Nothing is currently returned from `run_sdmtmb` but the workspace is saved
#' to the disk as `sdmTMB_save.RData`.
run_sdmtmb <- function(dir = getwd(),
                       data,
                       family = sdmTMB::delta_gamma(),
                       formula,
                       n_knots = 500) {
  # TODO:
  # * check if random effects in formula and if converged, if yes and no, then
  #   re run the model after changing the formula to not have random effects
  # * document california_current_grid
  # * check licence file to see if I am even able to steal it
  # * think about setting a distance cutoff for the triangulation network
  #   rather than pre-specifying the number of knots
  dir_data <- fs::path(dir, "data")
  dir_index <- fs::path(dir, "index")
  fs::dir_create(c(dir_data, dir_index))

  # Create prediction grid
  ranges <- data %>%
      dplyr::filter(catch_weight > 0) %>%
      dplyr::summarize(dplyr::across(
        dplyr::contains("tude"),
        .fns = list("max" = ~ max(.) + 0.1, "min" = ~ min(.) - 0.1)
      ))
  data_truncated <- data %>%
    dplyr::filter(
      latitude > ranges[["latitude_min"]] & latitude < ranges[["latitude_max"]],
      longitude > ranges[["longitude_min"]] & longitude < ranges[["longitude_max"]]
    )
  grid <- lookup_grid(
    x = data[["survey_name"]][1],
    max_latitude = ranges[["latitude_max"]],
    min_latitude = ranges[["latitude_min"]],
    max_longitude = ranges[["longitude_max"]],
    min_longitude = ranges[["longitude_min"]],
    years = sort(unique(data_truncated$year))
  )
  # TODO: think about vessel_year, might want a different level scaling things
  #       might not have to give this to grid
  data_truncated$vessel_year <- as.factor(data_truncated$vessel_year)

  # plot and save the mesh
  mesh <- sdmTMB::make_mesh(
    data = data_truncated,
    xy_cols = c("x", "y"),
    n_knots = n_knots
  )
  grDevices::png(
    filename = fs::path(dir_data, "mesh.png"),
    width = 7,
    height = 7,
    units = "in",
    res = 300,
    pointsize = 12
  )
  plot(mesh)
  dev.off()

  # Run model
  fit <- sdmTMB::sdmTMB(
    formula = formula,
    time = "year",
    offset = log(data_truncated$effort),
    data = data_truncated,
    mesh = mesh,
    family = family,
    spatial = "on",
    # spatiotemporal = list("iid", "iid"),
    spatiotemporal = list("off", "off"),
    anisotropy = TRUE,
    silent = TRUE,
    do_index = FALSE,
    control = sdmTMB::sdmTMBcontrol(newton_loops = 3),
    # Uncomment to get a coast-wide index
    # do_index = TRUE,
    # predict_args = list(newdata = grid, re_form_iid = NA),
    # index_args = list(area = grid$area_km2)
  )

  gg_index_areas <- get_index_areas(
    data = data_truncated,
    fit = fit,
    prediction_grid = grid,
    dir = dir_index
  )
  index_areas <- gg_index_areas[["data"]]

  # Add diagnostics
  # 1) QQ plot
  # 2) Residuals by year
  diagnostics <- get_diagnostics(
    dir = dir_index,
    fit = fit,
    prediction_grid = grid
  )

  save(
    data,
    data_truncated,
    diagnostics,
    dir,
    mesh,
    grid,
    fit,
    index_areas,
    gg_index_areas,
    file = fs::path(dir_index, "sdmTMB_save.RData")
  )
  return(fit)
}
