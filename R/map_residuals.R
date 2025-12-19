#' Plot the data from a model fit by [sdmTMB::sdmTMB()]
#'
#' @param data A data frame with at least the following three columns:
#'   * `X`,
#'   * `Y`, and
#'   * `data`,
#'   where the data column is the result of [stats::residuals()].
#' @inheritParams map_density
#'
#' @author Chantel R. Wetzel
#' @export
#'
map_residuals <- function(data,
                          n_row = 1,
                          n_col = 2,
                          save_prefix = NULL) {

  gg <- map_base() +
    ggplot2::geom_point(
      data = data,
      ggplot2::aes(X * 1000, Y * 1000, colour = residuals, fill = residuals),
      pch = 15,
      alpha = 0.5
    ) +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::scale_colour_viridis_c() +
    ggplot2::labs(fill = "Residuals") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
    ) +
    ggplot2::guides(colour = "none") +
    ggforce::facet_wrap_paginate("year", nrow = n_row, ncol = n_col)

  # Only save if save_prefix is provided
  if (!is.null(save_prefix)) {
    purrr::map(
      seq(ggforce::n_pages(gg)),
      .f = ggsave_year,
      y = gg,
      prefix = save_prefix,
      n_col = n_col,
      n_row = n_row
    )
  }

  return(gg)
}
