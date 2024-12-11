#' Run [sdmTMB::sdmTMB()]
#'
#' @param dir_main A string specifying a path where results will be saved. The
#'   default is your current working directory.
#' @inheritParams format_data
#' @inheritParams sdmTMB::sdmTMB
#' @param n_knots An integer specifying the number of knots you want in your
#'   mesh that is created by {fmesher}. More knots is not always better. The
#'   default is to use 500 knots. Future work will look at specifying a
#'   threshold distance between points rather than number of knots.
#' @param share_range Logical, whether or not to share the range between the
#'   spatial and spatiotemporal fields. This defaults to FALSE, but adds extra
#'   parameters. The default in sdmTMB is TRUE, and sharing the range may improve
#'   estimation for data limited applications
#' @param sdmtmb_control Optional list, in the format of [sdmTMB::sdmTMBcontrol()].
#'   By default, this is includes 3 newton loops
#' @param ... Optional arguments passed to [sdmTMB::sdmTMB()]. Note that users
#'   cannot pass `anisotropy` because this are set
#'   internal to this function, where `anisotropy = TRUE` because the coastline
#'   of the western coastline of the U.S.A. is not perpendicular to the country
#'   and three newton loops are specified in the control parameters.
#'
#' @author Chantel R. Wetzel
#' @export
#' @return
#' An list object of class sdmTMB. This object is returned by [sdmTMB::sdmTMB()]
#' when fitting data to the model.
run_sdmtmb <- function(dir_main = getwd(),
                       data,
                       family,
                       formula,
                       n_knots = 500,
                       share_range = FALSE,
                       sdmtmb_control = sdmTMB::sdmTMBcontrol(newton_loops = 3),
                       ...) {
  # Checks
  stopifnot(inherits(family, "family"))
  stopifnot(all(
    c(
      "year", "fyear", "survey_name", "common_name",
      "catch_weight", "effort", "x", "y"
    ) %in%
      colnames(data)
  ))
  # TODO:
  # * check if random effects in formula and if converged, if yes and no, then
  #   re run the model after changing the formula to not have random effects
  # * document california_current_grid
  # * check licence file to see if I am even able to steal it
  # * think about setting a distance cutoff for the triangulation network
  #   rather than pre-specifying the number of knots
  dir_new <- data |>
    dplyr::group_by(survey_name, common_name) |>
    dplyr::count() |>
    dplyr::mutate(
      common_without = format_common_name(common_name),
      survey_without = format_common_name(survey_name),
      directory = fs::path(
        dir_main,
        common_without,
        survey_without,
        format_family(family)
      )
    ) |>
    dplyr::pull(directory)
  stopifnot(length(dir_new) == 1)
  dir_data <- fs::path(dir_new, "data")
  dir_index <- fs::path(dir_new, "index")
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
    control = sdmtmb_control,
    share_range = share_range,
    ...
  )
  # Refit the model if the hessian is not positive definite
  if (!fit[["pos_def_hessian"]]) {
    fit <- sdmTMB::run_extra_optimization(fit)
  }

  # Save model output
  saveRDS(fit, file = fs::path(dir_index, "ignore_fit.rds"))
  write_hessian(fit, fs::path(dir_index, "hess_logical.txt"))
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
  results_by_area <- calc_index_areas(
    data = data_truncated,
    fit = fit,
    prediction_grid = grid,
    dir = dir_index
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
    dir_new,
    mesh,
    grid,
    fit,
    results_by_area,
    file = fs::path(dir_index, "sdmTMB_save.RData")
  )
  return(fit)
}
