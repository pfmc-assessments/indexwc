#' Plot the annual parameter estimates from the fit model
#'
#' @param fit List created by [sdmTMB::sdmTMB()].
#' @template dir
#'
#' @author Chantel R. Wetzel
#' @export
plot_pars_fixed <- function(fit, dir) {
  plot_fixed_helper <- function(fit, model_number = 1) {
    out <- tidy(fit, model = model_number, silent = TRUE) %>%
      dplyr::mutate(
        model = as.factor(model_number),
        term = gsub("fyear", "", term),
        lower_ci = estimate - 2 * std.error,
        upper_ci = estimate + 2 * std.error,
      ) %>%
      dplyr::filter(grepl("^[0-9]+$", term)) %>%
      dplyr::mutate(
        term = as.numeric(term)
      ) %>%
      dplyr::arrange(term)
    return(out)
  }

  n_plot <- length(grep("b_j", names(sdmTMB::get_pars(fit))))

  gg_out <- ggplot2::ggplot(
    data = purrr::map_df(
      seq(n_plot),
      .f = ~ plot_fixed_helper(fit = fit, model_number = .x)
    ),
    mapping = ggplot2::aes(x = term, y = estimate, color = model)
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(
      mapping = ggplot2::aes(
        ymin = lower_ci,
        ymax = upper_ci
      ),
      width = 0.1
    ) +
    ggplot2::xlab("Year") +
    ggplot2::ylab("Fixed effects") +
    ggplot2::theme_bw()
  suppressMessages(ggplot2::ggsave(
    filename = file.path(dir, "parameters_fixed_effects.png"),
    plot = gg_out,
    height = 10,
    width = 10,
    units = "in"
  ))
  return(gg_out)
}
