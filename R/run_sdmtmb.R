#' Run [sdmTMB::sdmTMB()]
#'
#' Fits a spatial or spatiotemporal GLMM using [sdmTMB::sdmTMB()]. This function
#' handles data preparation, mesh creation, model fitting, and optionally saves
#' results to a structured directory. The fitted model is returned with minimal
#' attachments for downstream diagnostic and index calculations.
#'
#' @param dir A string specifying a path where results will be saved. The
#'   default is your current working directory. A subdirectory structure will be
#'   created based on the species, survey, and model family. If `NULL`, the fitted
#'   object is returned with nothing saved to disk
#' @param dir_main Deprecated. A string specifying a path where results will be saved. The
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
#' @param spatial Estimate spatial random fields? Options are 'on' / 'off'
#'    or TRUE / FALSE. Optionally, a list for delta models, e.g. list('on', 'off').
#'    Default is list('on', 'on') to estimate spatial random fields for a delta
#'    model.
#' @param spatiotemporal Estimate the spatiotemporal random fields as 'iid'
#'   (independent and identically distributed; default), stationary 'ar1'
#'   (first-order autoregressive), a random walk ('rw'), or fixed at 0 'off'.
#'   If a delta model, can be a list. Default is list('iid', 'iid') to estimate
#'   spatialtemporal random fields for a delta model. These settings are
#'   available in the configuration file under spatiotemporal1 for the presence
#'   absence model and spatiotemporal2 for the catch rate model.
#' @param anisotropy Logical: allow for anisotropy (spatial correlation that is
#'   directionally dependent).  This is commonly needed for West Coast groundfish
#'   stocks that have a coastwide range due to the directionality of the coast
#'   line. Default is TRUE.
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
run_sdmtmb <- function(
  data,
  family,
  formula,
  dir = NULL,
  dir_main = lifecycle::deprecated(),
  n_knots = 500,
  spatial = list("on", "on"),
  spatiotemporal = list("iid", "iid"),
  anisotropy = TRUE,
  share_range = FALSE,
  sdmtmb_control = sdmTMB::sdmTMBcontrol(newton_loops = 3),
  ...
) {
  if (lifecycle::is_present(dir_main)) {
    lifecycle::deprecate_warn(
      when = "1.0",
      what = "indexwc::run_sdmtmb(dir_main =)",
      with = "indexwc::run_sdmtmb(dir =)"
    )
    dir <- dir_main
  }
  # Checks
  if (!inherits(family, "family")) {
    family <- eval(rlang::parse_expr(family))
  }
  stopifnot(inherits(family, "family"))
  stopifnot(all(
    c(
      "year",
      "fyear",
      "survey_name",
      "common_name",
      "catch_weight",
      "effort",
      "x",
      "y"
    ) %in%
      colnames(data)
  ))
  nwfscSurvey::check_dir(dir = dir, verbose = TRUE)
  # Create directory structure
  if (!is.null(dir)) {
    dir_new <- data |>
      dplyr::group_by(.data$survey_name, .data$common_name) |>
      dplyr::summarise(
        range = paste0(min(.data$latitude), "-", max(.data$latitude)),
        .groups = "drop_last"
      ) |>
      dplyr::mutate(
        common_without = format_common_name(.data$common_name),
        range_without = range,
        survey_without = format_common_name(.data$survey_name),
        directory = fs::path(
          dir,
          paste0(.data$common_without, "_", range_without),
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
    "*" = "Running sdmTMB for {data[1, 'common_name']} with {family$clean_name} error structure"
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
      .data$latitude > ranges[["latitude_min"]] &
        .data$latitude < ranges[["latitude_max"]],
      .data$longitude > ranges[["longitude_min"]] &
        .data$longitude < ranges[["longitude_max"]],
      .data$depth > ranges[["depth_max"]]
    ) |>
    droplevels()

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
    spatial = spatial,
    spatiotemporal = spatiotemporal,
    anisotropy = anisotropy,
    share_range = share_range,
    control = sdmtmb_control,
    ...
  )
  # Refit if hessian not positive definite
  if (!fit[["pos_def_hessian"]]) {
    fit <- sdmTMB::run_extra_optimization(fit)
  }
  # Attach mesh for downstream use
  fit$mesh <- mesh
  fit$ranges <- ranges
  if (!is.null(dir)) {
    fit$dir <- dir_data
  } else {
    fit$dir <- dir
  }
  # Save model output
  if (!is.null(dir)) {
    saveRDS(fit, file = fs::path(dir_data, "fit.rds"))
  }
  return(fit)
}
