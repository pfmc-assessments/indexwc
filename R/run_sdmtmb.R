#' Run [sdmTMB::sdmTMB()]
#'
#' Fits a spatial or spatiotemporal GLMM using [sdmTMB::sdmTMB()]. This function
#' handles data preparation, mesh creation, model fitting, and optionally saves
#' results to a structured directory. The fitted model is returned with minimal
#' attachments for downstream diagnostic and index calculations.
#'
#' @param dir_main A string specifying a path where results will be saved. The
#'   default is your current working directory. A subdirectory structure will be
#'   created based on the species, survey, and model family. If `NULL`, the fitted
#'   object is returned with nothing saved to disk
#' @param data A data frame containing the survey data. Must include columns:
#'   `year`, `fyear`, `survey_name`, `common_name`, `catch_weight`, `effort`,
#'   `x`, `y`, `latitude`, `longitude`, and `depth`.
#' @param family A family object specifying the response distribution and link.
#'   See [sdmTMB::sdmTMB()] for options. Common choices include
#'   `sdmTMB::delta_gamma()` and `sdmTMB::tweedie()`.
#' @param formula A formula specifying the fixed effects structure. Should
#'   reference columns in `data`. See [sdmTMB::sdmTMB()] for details.
#' @param n_knots An integer specifying the number of knots you want in your
#'   mesh that is created by \pkg{fmesher}. More knots is not always better. The
#'   default is to use 500 knots. Future work will look at specifying a
#'   threshold distance between points rather than number of knots.
#' @param share_range Logical, whether or not to share the range between the
#'   spatial and spatiotemporal fields. This defaults to `FALSE`, but adds extra
#'   parameters. The default in sdmTMB is `TRUE`, and sharing the range may
#'   improve estimation for data limited applications.
#' @param sdmtmb_control Optional list, in the format of [sdmTMB::sdmTMBcontrol()].
#'   By default, this includes 3 newton loops.
#' @param ... Optional arguments passed to [sdmTMB::sdmTMB()].
#'
#' @author Chantel R. Wetzel
#' @export
#' @return
#' A list object of class `sdmTMB` returned by [sdmTMB::sdmTMB()] when fitting
#' data to the model. Two additional components are attached for convenience:
#' * `mesh` - The mesh object used for fitting (useful for plotting)
#'
#' The fitted model's data (`fit$data`) contains the truncated data used for
#' fitting. Original data and prediction grid can be accessed from saved files
#' or recreated as needed.
#'
#' @details
#' The function performs the following steps:
#' - Creates a structured directory for outputs
#' - Filters data to positive catch locations and truncates to data extent
#' - Creates a prediction grid using [lookup_grid()]
#' - Builds a mesh with specified number of knots
#' - Fits the model using [sdmTMB::sdmTMB()]
#' - Refits with extra optimization if not converged
#' - Optionally saves fit, data, and mesh plot
#'
#' @seealso
#' * [diagnose()], run comprehensive diagnostics on the fitted model
#' * [calc_index_areas()], calculate abundance indices from the fitted model
#' * [sdmTMB::sdmTMB()], the underlying fitting function
#' * [lookup_grid()], creates the prediction grid
#'
#' @importFrom rlang .data
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
  # Create directory structure
  if(!is.null(dir_main)) {
    dir_new <- data |>
      dplyr::group_by(.data$survey_name, .data$common_name) |>
      dplyr::count() |>
      dplyr::mutate(
        common_without = format_common_name(.data$common_name),
        survey_without = format_common_name(.data$survey_name),
        directory = fs::path(
          dir_main,
          .data$common_without,
          .data$survey_without,
          format_family(family)
        )
      ) |>
      dplyr::pull(.data$directory)
    stopifnot(length(dir_new) == 1)
    dir_data <- fs::path(dir_new, "data")
    fs::dir_create(dir_data)
    save(data, file = file.path(dir_data, "data.rdata"))
  }
  formula <- format_formula(formula)
  cli::cli_inform(c(
    "*" = "Running sdmTMB for {data[1, 'common_name']}"
  ))
  # Create prediction grid
  ranges <- data |>
    dplyr::filter(.data$catch_weight > 0) |>
    dplyr::summarize(
      dplyr::across(
        dplyr::matches("tude"),
        .fns = list("max" = ~ max(.) + 0.1, "min" = ~ min(.) - 0.1)
      ),
      depth_max = min(.data$depth, na.rm = TRUE)
    )
  data_truncated <- data |>
    dplyr::filter(
      .data$latitude > ranges[["latitude_min"]] & .data$latitude < ranges[["latitude_max"]],
      .data$longitude > ranges[["longitude_min"]] & .data$longitude < ranges[["longitude_max"]],
      .data$depth > ranges[["depth_max"]]
    ) |>
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
  # Create and save mesh
  mesh <- sdmTMB::make_mesh(
    data = data_truncated,
    xy_cols = c("x", "y"),
    n_knots = n_knots
  )
  # Fit model
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
  # Refit if hessian not positive definite
  if (!fit[["pos_def_hessian"]]) {
    fit <- sdmTMB::run_extra_optimization(fit)
  }
  # Save model output
  if(!is.null(dir_main)) {
    saveRDS(fit, file = fs::path(dir_data, "fit.rds"))
  }
  # Attach mesh for downstream use
  fit$mesh <- mesh
  return(fit)
}
