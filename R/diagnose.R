#' Summarize model estimates and plot diagnostics
#'
#' @param dir Directory path where results will be saved. If `NULL`,
#'   results are only returned as a list (not saved to disk).
#' @param fit An object of class `sdmTMB` returned by [sdmTMB::sdmTMB()].
#' @param prediction_grid The prediction grid for the survey that sdmTMB will
#'   use to make model predictions to. Should not be NULL
#'
#' @import sdmTMB
#'
#' @author Chantel R. Wetzel
#' @export
#' @seealso
#' * [run_sdmtmb()], fit the model first
#' * [calc_index_areas()], calculate indices after diagnosing
#' @return A list containing
#' \itemize{
#'   \item `sanity` - Sanity check results as list
#'   \item `model` - The model family name
#'   \item `formula` - The model formula
#'   \item `loglike` - The log-likelihood value
#'   \item `aic` - The AIC value
#'   \item `effects` - A data frame of fixed and random effects, with confidence intervals
#'   \item `qq_plot` - QQ plot ggplot object
#'   \item `residual_maps` - List of residual map ggplot objects (one per model component for delta models)
#'   \item `anisotropy_plot` - Anisotropy plot ggplot object
#'   \item `fixed_effects_plot` - Fixed effects plot ggplot object
#'   \item `density_plot` - Density plots ggplot object
#'   \item `predictions` - Prediction data from the grid (as data frame)
#'   \item `data_with_residuals` - Original data with residuals added (data frame)
#'   \item `date` - Date diagnostics were run (Date)
#'   \item `session_info` - R session information for reproducibility
#' }
#'
#' @examples
#' \dontrun{
#' # Fit a model
#' fit <- run_sdmtmb(
#'   data = data_formatted,
#'   family = sdmTMB::delta_gamma(),
#'   formula = catch_weight ~ 0 + fyear + depth_scaled + depth_scaled_squared
#' )
#'
#' # Run diagnostics without saving
#' diag <- diagnose_model(
#'   fit = fit,
#'   prediction_grid = my_grid,
#'   dir = NULL
#' )
#'
#' # Access diagnostic values
#' diag$aic                    # AIC value
#' diag$loglike                # Log-likelihood
#' diag$sanity                 # Sanity check data frame
#' head(diag$effects)          # Effects table
#'
#' # View plots
#' print(diag$qq_plot)         # QQ plot
#' print(diag$anisotropy_plot) # Anisotropy
#' print(diag$residual_maps[[1]])  # Residuals model 1
#' if (length(diag$residual_maps) > 1) {
#'   print(diag$residual_maps[[2]])  # Residuals model 2 (delta)
#' }
#' print(diag$fixed_effects_plot)  # Fixed effects over time
#' print(diag$density_plot)        # Density maps
#'
#' # Access data
#' head(diag$predictions)       # Predictions on grid
#' head(diag$data_with_residuals)  # Data with residuals
#'
#' # Session information
#' diag$date                    # When diagnostics were run
#' diag$session_info            # Full session info
#'
#' }
diagnose <- function(dir,
                           fit,
                           prediction_grid = NULL) {
  # Handle both indexwc_fit objects and raw sdmTMB objects
  if (inherits(fit, "indexwc_fit")) {
    sdmtmb_fit <- fit$fit

    # Auto-create prediction grid if not provided and we have an indexwc_fit
    if (is.null(prediction_grid)) {
      prediction_grid <- lookup_grid(
        x = fit$metadata$survey_name,
        max_latitude = fit$ranges$latitude_max,
        min_latitude = fit$ranges$latitude_min,
        max_longitude = fit$ranges$longitude_max,
        min_longitude = fit$ranges$longitude_min,
        max_depth = abs(fit$ranges$depth_max),
        years = sort(unique(fit$data$year))
      )
    }
  } else if (inherits(fit, "sdmTMB")) {
    sdmtmb_fit <- fit
    if (is.null(prediction_grid)) {
      cli::cli_abort(c(
        "x" = "prediction_grid is required when fit is a raw sdmTMB object",
        "i" = "Either provide prediction_grid or use an indexwc_fit object from {.fn fit_index}"
      ))
    }
  } else {
    cli::cli_abort(c(
      "x" = "fit must be of class {.cls indexwc_fit} or {.cls sdmTMB}",
      "i" = "Did you use {.fn fit_index} or {.fn sdmTMB::sdmTMB}?"
    ))
  }

  # mesh plot
  filename <- NULL
  if(!is.null(dir)) filename = fs::path(dir, "mesh.png")
  mesh_plot <- plot_mesh(mesh, file_name = filename)

  # Get sanity diagnostics
  sanity_out <- sanity_data(sdmtmb_fit)
  if(!is.null(dir)) {
    utils::write.table(
      sanity_out,
      file = fs::path(dir, "sanity_data_frame.csv"),
      append = FALSE,
      sep = ",",
      row.names = FALSE
    )
  }

  # Diagnostics related to log likelihood
  run_diagnostics <- list()
  run_diagnostics$model <- sdmtmb_fit$family$clean_name
  run_diagnostics$formula <- sdmtmb_fit$formula[[1]]
  run_diagnostics$loglike <- logLik(sdmtmb_fit)
  run_diagnostics$aic <- AIC(sdmtmb_fit)
  if(!is.null(dir)) {
    write.table(
      rbind(
        c("AIC", run_diagnostics$aic),
        c("NLL", -1 * run_diagnostics$loglike)
      ),
      file = file.path(dir, "aic_nll.txt"),
      row.names = FALSE, col.names = FALSE
    )
  }

  # This extracts the fixed and random effects
  all_combos <- tidyr::expand_grid(
    x = seq(length(sdmtmb_fit$formula)),
    y = c("fixed", "ran_pars")
  )
  run_diagnostics[["effects"]] <- purrr::map2_dfr(
    .x = all_combos[["x"]],
    .y = all_combos[["y"]],
    .f = ~ broom::tidy(
      x = sdmtmb_fit,
      model = .x,
      effects = .y,
      conf.int = TRUE,
      silent = TRUE
    ),
    .id = "model"
  ) |>
    dplyr::mutate(
      model = unlist(all_combos[model, "x"])
    )
  if(!is.null(dir)) {
    save(
      run_diagnostics,
      file = file.path(dir, "run_diagnostics_and_estimates.rdata")
    )
  }

  # Calculate residuals
  sdmtmb_fit[["data"]][["residuals"]] <- stats::residuals(
    sdmtmb_fit,
    model = 1,
    type = "mle-mvn"
  )
  if (length(formula(sdmtmb_fit)) > 1) {
    sdmtmb_fit[["data"]][["residuals2"]] <- stats::residuals(
      sdmtmb_fit,
      model = 2,
      type = "mle-mvn"
    )
  }

  # QQ plot
  filename <- NULL
  if(!is.null(dir)) filename <- file.path(dir, "qq.png")
  qqplot <- plot_qq(
    fit = sdmtmb_fit,
    file_name = filename
  )

  # Residual maps by year

  residual_maps_by_year <- purrr::map(
    seq_along(sdmtmb_fit[["formula"]]),
    .f = function(x, y, f_dir) {
      y[["residuals"]] <- y[[
        paste0("residuals", ifelse(x == 1, "", x))
      ]]

      # Set filename (NULL if no dir)
      save_prefix <- NULL
      if (!is.null(f_dir)) {
        save_prefix <- file.path(f_dir, paste0("residuals_", x, "_"))
      }

      map_residuals(
        y,
        save_prefix = save_prefix
      )
    },
    y = sdmtmb_fit[["data"]],
    f_dir = dir
  )

  # Anisotropy plot
  filename <- NULL
  if(!is.null(dir)) filename <- fs::path(dir, "anisotropy.png")
  gg_aniso <- sdmTMB::plot_anisotropy(object = sdmtmb_fit) +
    ggplot2::theme_bw()
  if(!is.null(filename)) {
    suppressMessages(ggplot2::ggsave(
      filename = filename,
      plot = gg_aniso,
      height = 7,
      width = 7
    ))
  }

  # Fixed effects plots
  fixed_effects_plot <- plot_pars_fixed(
    fit = sdmtmb_fit,
    dir = dir
  )

  # Calculate predictions based on the grid
  predictions <- predict(
    sdmtmb_fit,
    newdata = prediction_grid
  )

  # Density plots
  save_prefix <- NULL
  if(!is.null(dir)) save_prefix <- file.path(dir, "density")

  density_plot <- map_density(
    predictions = predictions,
    save_prefix = save_prefix
  )

  # Save data with residuals and predictions
  if(!is.null(dir)) {
    data_with_residuals <- sdmtmb_fit$data
    save(data_with_residuals, file = file.path(dir, "data_with_residuals.rdata"))
    save(predictions, file = file.path(dir, "predictions.rdata"))
  }

  return(list(
    sanity = sanity_out,
    model = run_diagnostics$model,
    formula = run_diagnostics$formula,
    loglike = run_diagnostics$loglike,
    aic = run_diagnostics$aic,
    effects = run_diagnostics$effects,
    mesh_plot = mesh_plot,
    qq_plot = qqplot,
    residual_maps_by_year = residual_maps_by_year,
    anisotropy_plot = gg_aniso,
    fixed_effects_plot = fixed_effects_plot,
    density_plots = density_plot,
    predictions = predictions,
    data_with_residuals = sdmtmb_fit$data,
    date = Sys.Date(),
    session_info = sessionInfo()
  ))
}
