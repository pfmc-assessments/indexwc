#' Plot the annual parameter estimates from the fit model
#'
#' @param fit List created by [sdmTMB::sdmTMB()].
#' @param file_name A string giving the file name. The default is
#'   `"parameters_fixed_effects.png"`. If NULL, the file is not written but the ggplot object is returned
#' @template dir
#'
#' @author Chantel R. Wetzel
#' @export
plot_pars_fixed <- function(fit, dir, file_name = "parameters_fixed_effects.png") {
  plot_fixed_helper <- function(fit, model_number = 1) {
    out <- tidy(fit, model = model_number, silent = TRUE) |>
      dplyr::mutate(
        model = as.factor(model_number),
        term = gsub("fyear", "", .data$term),
        lower_ci = .data$estimate - 2 * .data$std.error,
        upper_ci = .data$estimate + 2 * .data$std.error,
      ) |>
      dplyr::filter(grepl("^[0-9]+$", .data$term)) |>
      dplyr::mutate(
        term = as.numeric(.data$term)
      ) |>
      dplyr::arrange(.data$term)
    return(out)
  }
  n_plot <- ifelse(isTRUE(fit$family$delta), 2, 1)
  gg_out <- ggplot2::ggplot(
    data = purrr::map_df(
      seq(n_plot),
      .f = ~ plot_fixed_helper(fit = fit, model_number = .x)
    ),
    mapping = ggplot2::aes(x = .data$term, y = .data$estimate, color = .data$model)
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(
      mapping = ggplot2::aes(
        ymin = .data$lower_ci,
        ymax = .data$upper_ci
      ),
      width = 0.1
    ) +
    ggplot2::xlab("Year") +
    ggplot2::ylab("Fixed effects") +
    ggplot2::theme_bw()
  if (!is.null(dir) && !is.null(file_name)) {
    suppressMessages(ggplot2::ggsave(
      filename = file.path(dir, file_name),
      plot = gg_out,
      height = 10,
      width = 10,
      units = "in"
    ))
  }
  return(gg_out)
}
