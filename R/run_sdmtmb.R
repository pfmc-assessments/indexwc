#' Run [sdmTMB::sdmTMB()]
#'
#' @inheritParams run
#'
#' @author Chantel R. Wetzel
#' @return
#' An object (`list``) of class sdmTMB. This object is what is returned by
#' [sdmTMB::sdmTMB()] when fitting data to the model.
run_sdmtmb <- function(dir_main = getwd(),
                       data,
                       family,
                       formula,
                       n_knots = 500,
                       ...) {
  # TODO:
  # * check if random effects in formula and if converged, if yes and no, then
  #   re run the model after changing the formula to not have random effects
  # * document california_current_grid
  # * check licence file to see if I am even able to steal it
  # * think about setting a distance cutoff for the triangulation network
  #   rather than pre-specifying the number of knots
  dir_data <- fs::path(dir_main, "data")
  dir_index <- fs::path(dir_main, "index")
  fs::dir_create(c(dir_data, dir_index))
  save(data, file = file.path(dir_data, "data.rdata"))
  if (inherits(formula, "character")) {
    if (length(formula) == 3 && formula[1] == "~") {
      formula <- paste(formula[2], formula[1], formula[3])
    }
    formula <- as.formula(paste(formula, collapse = ""))
  }

  # Create prediction grid
  ranges <- data %>%
      dplyr::filter(catch_weight > 0) %>%
      dplyr::summarize(
        dplyr::across(
          dplyr::matches("tude"),
          .fns = list("max" = ~ max(.) + 0.1, "min" = ~ min(.) - 0.1)
        ),
        # depth_min = min(abs(depth), na.rm = TRUE),
        depth_max = min(depth, na.rm = TRUE)
      )
  data_truncated <- data %>%
    dplyr::filter(
      latitude > ranges[["latitude_min"]] & latitude < ranges[["latitude_max"]],
      longitude > ranges[["longitude_min"]] & longitude < ranges[["longitude_max"]],
      depth > ranges[["depth_max"]]
    )
  grid <- lookup_grid(
    x = data[["survey_name"]][1],
    max_latitude = ranges[["latitude_max"]],
    min_latitude = ranges[["latitude_min"]],
    max_longitude = ranges[["longitude_max"]],
    min_longitude = ranges[["longitude_min"]],
    max_depth = abs(ranges[["depth_max"]]),
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
    control = sdmTMB::sdmTMBcontrol(newton_loops = 3),
    ...
  )
  if (any(grepl("depth_scaled", as.character(formula)))) {
    gg_depth <- cowplot::plot_grid(
      nrow = 2,
      visreg_delta(fit, xvar = "depth_scaled", model = 1, gg = TRUE),
      visreg_delta(fit, xvar = "depth_scaled", model = 2, gg = TRUE)
    )
    cowplot::save_plot(
      filename = file.path(dir_index, "depth_scaled.png"),
      plot = gg_depth
    )
    dev.off()
  }
  # Refit the model if the hessian is not positive definite
  if (!fit[["pos_def_hessian"]]) {
    fit <- sdmTMB::run_extra_optimization(fit)
  }
  gg_index_areas <- calc_index_areas(
    data = data_truncated,
    fit = fit,
    prediction_grid = grid,
    dir = dir_index
  )
  index_areas <- gg_index_areas[["data"]]

  # Add diagnostics
  # 1) QQ plot
  # 2) Residuals by year
  diagnostics <- diagnose(
    dir = dir_index,
    fit = fit,
    prediction_grid = grid
  )

  save(
    data,
    data_truncated,
    diagnostics,
    dir_main,
    mesh,
    grid,
    fit,
    index_areas,
    gg_index_areas,
    file = fs::path(dir_index, "sdmTMB_save.RData")
  )
  return(fit)
}
