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
  formula <- format_formula(formula)
  cli::cli_inform(c(
    "*" = "Running sdmTMB for {data[1, 'common_name']}"
  ))

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
    ) %>%
    droplevels()

  grid <- lookup_grid(
    x = data[["survey_name"]][1],
    max_latitude = ranges[["latitude_max"]],
    min_latitude = ranges[["latitude_min"]],
    max_longitude = ranges[["longitude_max"]],
    min_longitude = ranges[["longitude_min"]],
    max_depth = abs(ranges[["depth_max"]]),
    years = sort(unique(data_truncated$year))
  )

  # plot and save the mesh
  mesh <- sdmTMB::make_mesh(
    data = data_truncated,
    xy_cols = c("x", "y"),
    n_knots = n_knots
  )
  plot_mesh(mesh, file_name = fs::path(dir_data, "mesh.png"))

  # Run model
  fit <- sdmTMB::sdmTMB(
    formula = formula,
    time = "year",
    offset = log(data_truncated$effort),
    data = data_truncated,
    mesh = mesh,
    family = family,
    control = sdmTMB::sdmTMBcontrol(newton_loops = 3),
    share_range = FALSE,
    ...
  )
  # Refit the model if the hessian is not positive definite
  if (!fit[["pos_def_hessian"]]) {
    fit <- sdmTMB::run_extra_optimization(fit)
  }
  if (any(grepl("depth_scaled", as.character(formula)))) {
    gg_depth <- suppressMessages(purrr::map(
      .x = which(is_depth_in_formula(formula, family[["delta"]])),
      .f = ~ visreg_delta(fit, xvar = "depth_scaled", model = .x, gg = TRUE)
    ))
    cowplot::save_plot(
      filename = file.path(dir_index, "depth_scaled.png"),
      plot = cowplot::plot_grid(
        plotlist = gg_depth,
        nrow = sum(is_depth_in_formula(formula, family[["delta"]])),
        ncol = 1
      )
    )
    dev.off()
  }
  gg_index_areas <- calc_index_areas(
    data = data_truncated,
    fit = fit,
    prediction_grid = grid,
    dir = dir_index
  )
  index_areas <- gg_index_areas[["data"]]
  write.csv(
    index_areas,
    file = file.path(dir_index, "est_by_area.csv"),
    row.names = FALSE
  )
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
