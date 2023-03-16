#' Run [sdmTMB::sdmTMB]
#'
#' @template dir
#' @param data A data frame.
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
  minimum_longitude <- data %>%
      dplyr::filter(catch_weight > 0) %>%
      dplyr::summarize(min = min(longitude)) %>%
      dplyr::pull(min) - 0.1
  grid <- lookup_grid(
    x = data[["survey_name"]][1],
    # min_longitude = minimum_longitude,
    years = sort(unique(data$year))
  )
  # TODO: think about vessel_year, might want a different level scaling things
  #       might not have to give this to grid, might be able to use as.factor()
  #       in the formula
  data$vessel_year <- as.factor(data$vessel_year)

  # plot and save the mesh
  mesh <- sdmTMB::make_mesh(
    data = data,
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
    # TODO: fix the formula so the column list works
    # formula = data[["formula"]][1],
    # formula = catch_weight ~ 0 + as.factor(year) + pass_scaled + (1 | vessel_year),
    formula = catch_weight ~ 0 + as.factor(year) + pass_scaled,
    time = "year",
    offset = log(data$effort),
    data = data,
    mesh = mesh,
    family = family,
    spatial = "on",
    spatiotemporal = list("iid", "iid"),
    anisotropy = TRUE,
    silent = TRUE,
    control = sdmTMB::sdmTMBcontrol(
      newton_loops = 1L,
      map = list(ln_H_input = factor(c(1, 2, 1, 2))) # <- force sdmTMB to share anisotropy parameters across the two delta models
    ),
    do_index = FALSE,
    # Uncomment to get a coast-wide index
    # do_index = TRUE,
    # predict_args = list(newdata = grid, re_form_iid = NA),
    # index_args = list(area = grid$area_km2)
  )
  loglike <- logLik(fit)
  aic <- AIC(fit)

  # There is no way to estimate the index with bias correction in sdmTMB::sdmTMB
  # which is why we have to call [sdmTMB::get_index()] even if predictions are
  # specified in [sdmTMB::sdmTMB()].
  boundaries <-  list(
    coastwide = data %>%
      dplyr::filter(catch_weight > 0) %>%
      dplyr::pull(latitude) %>%
      range() %>%
      rev(),
    WA = c(southern_BC, southern_WA),
    OR = c(southern_WA, southern_OR),
    CA = c(southern_OR, southern_CA)
  )
  index_areas <- purrr::map_dfr(
    # Set up the area-specific grids as a list of data frames
    .x = purrr::map2(
      .x = purrr::map(boundaries, 1),
      .y = purrr::map(boundaries, 2),
      .f = filter_grid,
      grid = grid
    ),
    # Anonymous function that does both prediction and get_index
    .f = function(grid, object) {
      prediction <- predict(object, newdata = grid, return_tmb_object = TRUE)
      index <- sdmTMB::get_index(
        obj = prediction,
        bias_correct = TRUE,
        area = grid[["area_km2"]]
      )
      return(index)
    },
    object = fit,
    .id = "area"
  )

    gg_index <- plot_indices(
      data = index_areas,
      save_loc = dir_index
    )
    if (any(grepl("wide", index_areas[["area"]]))) {
      gg_index_coastwide <- plot_indices(
        data = dplyr::filter(index_areas, grepl("wide", area)),
        save_loc = dirname(dir_index)
      )
    }

    # Add diagnostics
    # 1) QQ plot
    # 2) Residuals by year
    diagnositcs <- get_diagnostics(
      dir = dir_index,
      fit = fit,
      prediction_grid = grid
    )

    save(
      data, mesh, grid, fit, index_areas, loglike, aic, gg_index,
      file = fs::path(dir_index, "sdmTMB_save.RData")
    )
}
