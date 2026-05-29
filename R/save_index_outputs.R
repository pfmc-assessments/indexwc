#' Save index model outputs to disk
#'
#' A convenience wrapper that takes the outputs from [run_sdmtmb()], [diagnose()],
#' and [calc_index_areas()] when run with `dir = NULL` and writes them all to
#' disk in the same directory structure as the original indexwc code.
#'
#' @param fit An sdmTMB model object returned by [run_sdmtmb()] with `dir = NULL`
#' @param diagnostics A list returned by [diagnose()] with `dir = NULL`
#' @param indices A list returned by [calc_index_areas()] with `dir = NULL`
#' @param dir A string specifying a path where results will be saved. The
#'   default is your current working directory. A subdirectory structure will be
#'   created based on the species, survey, and model family. If `NULL`, the fitted
#'   object is returned with nothing saved to disk
#' @param dir_main Deprecated. A string specifying a path where results will be saved. The
#'   default is your current working directory. A subdirectory structure will be
#'   created based on the species, survey, and model family. If `NULL`, the fitted
#'   object is returned with nothing saved to disk
#' @param overwrite Logical. If `TRUE`, existing files will be overwritten. Default is `FALSE`
#'
#' @details
#' This function recreates the file structure that would have been created if you had
#' run [run_sdmtmb()], [diagnose()], and [calc_index_areas()] with directory arguments
#' specified from the start.
#'
#' The directory structure created is:
#' ```
#' dir/
#' └── species_name/
#'     └── survey_name/
#'         └── family_name/
#'             ├── data/
#'             │   ├── data.rdata
#'             │   └── fit.rds
#'             ├── diagnostics/
#'             │   ├── mesh.png
#'             │   ├── sanity_data_frame.csv
#'             │   ├── aic_nll.txt
#'             │   ├── run_diagnostics_and_estimates.rdata
#'             │   ├── qq.png
#'             │   ├── residuals_1_*.png
#'             │   ├── residuals_2_*.png (if delta model)
#'             │   ├── anisotropy.png
#'             │   ├── fixed_effects.png
#'             │   ├── density_*.png
#'             │   ├── data_with_residuals.rdata
#'             │   └── predictions.rdata
#'             └── index/
#'                 ├── est_by_area.csv
#'                 ├── index_coastwide.png (if Coastwide calculated)
#'                 └── index_all_areas.png
#' ```
#'
#' @author Chantel R. Wetzel
#' @export
#' @seealso
#' * [run_sdmtmb()], fit the model
#' * [diagnose()], run diagnostics
#' * [calc_index_areas()], calculate indices
#'
#' @examples
#' \dontrun{
#'
#' # Pull and format data
#' my_data <- pull_and_format_data(
#'    configuration_to_run = configuration[1,]
#' )
#'
#' # Fit model without saving
#' fit <- run_sdmtmb(
#'   data = my_data$data_filtered,
#'   family = my_data$family,
#'   formula = my_data$formula
#' )
#'
#' # Run diagnostics without saving
#' diag <- diagnose(
#'   fit = fit
#' )
#'
#' # Calculate indices without saving
#' index <- calc_index_areas(
#'   data = fit$data,
#'   fit = fit,
#'   boundaries = "Coastwide"
#' )
#'
#' # Now save everything to disk
#' paths <- save_index_outputs(
#'   fit = fit,
#'   diagnostics = diag,
#'   indices = index,
#'   dir = fit$dir
#' )
#' }
#'
#' @importFrom rlang .data
save_index_outputs <- function(
  fit,
  diagnostics,
  indices,
  dir = getwd(),
  dir_main = lifecycle::deprecated(),
  overwrite = FALSE
) {
  nwfscSurvey::check_dir(dir = dir, verbose = TRUE)
  if (lifecycle::is_present(dir_main)) {
    lifecycle::deprecate_warn(
      when = "1.0",
      what = "indexwc::run_sdmtmb(dir_main =)",
      with = "indexwc::run_sdmtmb(dir =)"
    )
    dir <- dir_main
  }
  # Check structure of fit
  if (!inherits(fit, "sdmTMB")) {
    cli::cli_abort(c(
      "x" = "{.arg fit} must be an sdmTMB object",
      "i" = "Did you run {.fn run_sdmtmb}?"
    ))
  }
  # Check structure of diagnostics
  if (!is.list(diagnostics)) {
    cli::cli_abort(c(
      "x" = "{.arg diagnostics} must be a list",
      "i" = "Did you run {.fn diagnose}?"
    ))
  }
  # Check structure of indices
  if (!is.list(indices)) {
    cli::cli_abort(c(
      "x" = "{.arg indices} must be a list",
      "i" = "Did you run {.fn calc_index_areas}?"
    ))
  }
  # Extract metadata from fit object to create directory structure
  data <- fit$data
  family_obj <- fit$family
  dir_save <- fit$dir
  #dir_save <- data |>
  #  dplyr::group_by(.data$survey_name, .data$common_name) |>
  #  dplyr::count() |>
  #  dplyr::mutate(
  #    common_without = format_common_name(.data$common_name),
  #    survey_without = format_common_name(.data$survey_name),
  #    directory = fs::path(
  #      dir,
  #      .data$common_without,
  #      .data$survey_without,
  #      format_family(family_obj)
  #    )
  #  ) |>
  #  dplyr::pull(.data$directory)

  if (length(dir_save) != 1) {
    cli::cli_abort(c(
      "x" = "Multiple species or surveys detected in data",
      "i" = "This function expects a single species/survey combination"
    ))
  }

  # Create directory structure, following indexwc
  dir_data <- fs::path(dir_save, "data")
  dir_diagnostics <- fs::path(dir_save, "diagnostics")
  dir_index <- fs::path(dir_save, "index")

  fs::dir_create(dir_data, recurse = TRUE)
  fs::dir_create(dir_diagnostics, recurse = TRUE)
  fs::dir_create(dir_index, recurse = TRUE)

  # Check for existing files
  if (!overwrite) {
    existing_files <- c(
      fs::path(dir_data, "data.rdata"),
      fs::path(dir_data, "fit.rds")
    )
    existing <- existing_files[fs::file_exists(existing_files)]
    if (length(existing) > 0) {
      cli::cli_abort(c(
        "x" = "Files already exist in {.path {dir}}",
        "i" = "Set {.code overwrite = TRUE} to replace existing files",
        "i" = "Or choose a different {.arg dir}"
      ))
    }
  }

  ##############################################################################
  # Save model fit and data (from run_sdmtmb)
  ##############################################################################
  # Save the original data
  data_to_save <- data
  save(data_to_save, file = fs::path(dir_data, "data.rdata"))

  # Save the fitted model
  saveRDS(fit, file = fs::path(dir_data, "fit.rds"))

  ##############################################################################
  # Check model
  ##############################################################################
  cli::cli_inform(c("*" = "Running sanity check..."))
  # Save sanity checks
  utils::write.table(
    diagnostics$sanity,
    file = fs::path(dir_diagnostics, "sanity_data_frame.csv"),
    append = FALSE,
    sep = ",",
    row.names = FALSE
  )

  # Save AIC and NLL
  write.table(
    rbind(
      c("AIC", diagnostics$aic),
      c("NLL", -1 * diagnostics$loglike)
    ),
    file = fs::path(dir_diagnostics, "aic_nll.txt"),
    row.names = FALSE,
    col.names = FALSE
  )

  ##############################################################################
  # Save indices (from calc_index_areas)
  ##############################################################################
  cli::cli_inform(c("*" = "Saving and plotting indices..."))
  # Save index table
  write.csv(
    indices$indices,
    file = fs::path(dir_index, "est_by_area.csv"),
    row.names = FALSE
  )

  # Save coastwide index plot if it exists
  if (any(grepl("wide", indices$indices[["area"]], ignore.case = TRUE))) {
    gg_index_coastwide <- plot_indices(
      data = dplyr::filter(
        indices$indices,
        grepl("wide", area, ignore.case = TRUE)
      ),
      save_loc = NULL,
      file_name = NULL
    )
    suppressMessages(ggplot2::ggsave(
      filename = fs::path(dir_index, "index_coastwide.png"),
      plot = gg_index_coastwide,
      height = 7,
      width = 7
    ))
  }

  # Save all areas index plot
  if (!is.null(indices$plot_indices)) {
    suppressMessages(ggplot2::ggsave(
      filename = fs::path(dir_index, "index_all_areas.png"),
      plot = indices$plot_indices,
      height = 7,
      width = 10
    ))
  }

  ##############################################################################
  # Save diagnostics (from diagnose)
  ##############################################################################
  cli::cli_inform(c("*" = "Plotting diagnostics..."))

  # Save mesh plot
  if (!is.null(diagnostics$mesh_plot)) {
    suppressMessages(ggplot2::ggsave(
      filename = fs::path(dir_diagnostics, "mesh.png"),
      plot = diagnostics$mesh_plot,
      height = 7,
      width = 7
    ))
  }

  # Save run diagnostics and estimates
  run_diagnostics <- list(
    model = diagnostics$model,
    formula = diagnostics$formula,
    loglike = diagnostics$loglike,
    aic = diagnostics$aic,
    effects = diagnostics$effects
  )
  save(
    run_diagnostics,
    file = fs::path(dir_diagnostics, "run_diagnostics_and_estimates.rdata")
  )

  # Save QQ plot
  if (!is.null(diagnostics$qq_plot)) {
    suppressMessages(ggplot2::ggsave(
      filename = fs::path(dir_diagnostics, "qq.png"),
      plot = diagnostics$qq_plot,
      height = 7,
      width = 7
    ))
  }

  # Save anisotropy plot
  if (
    !is.null(diagnostics$anisotropy_plot) &&
      inherits(diagnostics$anisotropy_plot, "ggplot")
  ) {
    suppressMessages(ggplot2::ggsave(
      filename = fs::path(dir_diagnostics, "anisotropy.png"),
      plot = diagnostics$anisotropy_plot,
      height = 7,
      width = 7
    ))
  }

  # Save fixed effects plot
  if (!is.null(diagnostics$fixed_effects_plot)) {
    suppressMessages(ggplot2::ggsave(
      filename = fs::path(dir_diagnostics, "fixed_effects.png"),
      plot = diagnostics$fixed_effects_plot,
      height = 7,
      width = 7
    ))
  }

  # Save residual maps
  if (!is.null(diagnostics$residual_maps_by_year)) {
    for (i in seq_along(diagnostics$residual_maps_by_year)) {
      residual_plot <- diagnostics$residual_maps_by_year[[i]]
      if (!is.null(residual_plot)) {
        # Get number of pages in the plot
        n_pages <- ggforce::n_pages(residual_plot)
        for (page in 1:n_pages) {
          filename <- fs::path(
            dir_diagnostics,
            sprintf("residuals_%d_page_%02d.png", i, page)
          )
          suppressMessages(ggplot2::ggsave(
            filename = filename,
            plot = residual_plot +
              ggforce::facet_wrap_paginate(
                "year",
                nrow = 1,
                ncol = 2,
                page = page
              ),
            height = 5,
            width = 10
          ))
        }
      }
    }
  }

  if (!is.null(diagnostics$density_plots)) {
    for (i in seq_along(diagnostics$density_plots)) {
      density_plots <- diagnostics$density_plots
      if (!is.null(density_plots)) {
        # Get number of pages in the plot
        n_pages <- ggforce::n_pages(density_plots)
        for (page in 1:n_pages) {
          filename <- fs::path(
            dir_diagnostics,
            sprintf("density_page_%02d.png", i, page)
          )
          suppressMessages(ggplot2::ggsave(
            filename = filename,
            plot = density_plots +
              ggforce::facet_wrap_paginate(
                "year",
                nrow = 1,
                ncol = 2,
                page = page
              ),
            height = 5,
            width = 10
          ))
        }
      }
    }
  }

  # Save data with residuals
  data_with_residuals <- diagnostics$data_with_residuals
  save(
    data_with_residuals,
    file = fs::path(dir_diagnostics, "data_with_residuals.rdata")
  )

  # Save predictions
  predictions <- diagnostics$predictions
  save(predictions, file = fs::path(dir_diagnostics, "predictions.rdata"))
}
