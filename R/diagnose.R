#' Retrieve model estimates and plot diagnostics
#'
#' @details
#' Parameter definitions:
#' * Range: A derived parameter that defines the distance at which two
#' points are effectively independent (actually about 13% correlated).
#' If the share_range argument is changed to FALSE then the spatial
#' and spatiotemporal ranges will be unique, otherwise the default is
#' for both to share the same range.
#' * Phi: Observation error scale parameter (e.g., SD in Gaussian).
#' * sigma_O: Standard deviation of the spatial process ("Omega").
#' * sigma_E: Standard deviation of the spatiotemporal process ("Epsilon").
#'
#' @template dir
#' @param fit List created by the sdmTMB:: fit function.
#' @param prediction_grid The prediction grid for the survey that sdmTMB will
#'   use to make model predictions to.
#'
#' @import sdmTMB
#'
#' @author Chantel R. Wetzel
diagnose <- function(dir,
                     fit,
                     prediction_grid) {
  sdmTMB::sanity(fit)

  run_diagnostics <- list()
  run_diagnostics$model <- fit$family$clean_name
  run_diagnostics$formula <- fit$formula[[1]]
  run_diagnostics$loglike <- logLik(fit)
  run_diagnostics$aic <- AIC(fit)
  write.table(
    rbind(c("AIC", run_diagnostics$aic), c("NLL", run_diagnostics$loglike)),
    file = file.path(dir, "aic_nll.txt"),
    row.names = FALSE, col.names = FALSE
  )

  all_combos <- tidyr::expand_grid(
    x = seq(length(fit$formula)),
    y = c("fixed", "ran_pars")
  )
  run_diagnostics[["effects"]] <- purrr::map2_dfr(
    .x = all_combos[["x"]],
    .y = all_combos[["y"]],
    .f = ~ tidy(
      x = fit,
      model = .x,
      effects = .y,
      conf.int = TRUE,
      silent = TRUE
    ),
    .id = "model"
  ) %>%
    dplyr::mutate(
      model = unlist(all_combos[model, "x"])
    )

  save(
    run_diagnostics,
    file = file.path(dir, "run_diagnostics_and_estimates.rdata")
  )

  # Calculate the residuals
  # TODO
  # * calculate these by model component 1 or 1:2
  # * save by model component
  fit[["data"]][["residuals"]] <- stats::residuals(fit, model = 1)
  if (length(fit[["formula"]]) == 2 &&
      !grepl("mix", fit$family$clean_name)) {
    fit[["data"]][["residuals2"]] <- stats::residuals(fit, model = 2)
  }

  if (!grepl("mix", fit$family$clean_name)) {
    plot_qq(
      data = fit,
      dir = dir
    )

    ignore <- purrr::map(
      seq(length(fit[["formula"]])),
      .f = function(x, y, f_dir) {
        y[["data"]][["residuals"]] <- stats::residuals(y, model = x)
        map_residuals(
          y,
          save_prefix = file.path(f_dir, paste0("residuals_", x, "_"))
        )
      },
      y = fit,
      f_dir = dir
    )
  }

  gg <- sdmTMB::plot_anisotropy(
    object = fit
  ) +
    ggplot2::theme_bw()
  suppressMessages(ggplot2::ggsave(
    filename = fs::path(dir, "anisotropy.png"),
    height = 7,
    width = 7,
    plot = gg
  ))
  ignore <- plot_pars_fixed(
    fit = fit,
    dir = dir
  )

  # Calculate the predictions based on the grid
  predictions <- predict(
    fit,
    newdata = prediction_grid
  )

  # TODO:
  # * plot these by model component
  density_plot <- map_density(
    predictions = predictions,
    save_prefix = file.path(dir, "density")
  )

  # TODO Two remaining diagnostics needs help
  # I think map_effects is broken though
  # map_effects(
  #   predictions = predictions,
  #   dir = dir
  # )
  # map_year_re(
  #   predictions = predictions,
  #   dir = dir
  # )

  data_with_residuals <- fit$data
  save(data_with_residuals, file = file.path(dir, "data_with_residuals.rdata"))
  save(predictions, file = file.path(dir, "predictions.rdata"))
}
