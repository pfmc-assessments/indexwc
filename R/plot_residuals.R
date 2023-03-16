#' Plot the residuals from a model fit by [sdmTMB::sdmTMB()]
#'
#' @param data A list returned from [sdmTMB::sdmTMB()].
#' @template dir
#' @template nrow
#' @template ncol
#'
#' @author Chantel R. Wetzel
#' @export
#'
plot_residuals <- function(data, dir, nrow = 3, ncol = 4) {
  n_groups <- ceiling(length(unique(
    dplyr::pull(data[["data"]], year)
  )) / (ncol * nrow))
  gg <- purrr::map(
    .x = dplyr::group_by(
      data[["data"]],
      bin = ggplot2::cut_number(
        year,
        n = n_groups
      )
    ) %>%
      dplyr::group_split(),
    .f = ~ ggplot2::ggplot(
        data = .x, 
    		ggplot2::aes(longitude, latitude, colour = residuals)
      ) + 
    		ggplot2::geom_point(alpha = 0.5) + 
    		ggplot2::scale_colour_viridis_c() +
    		# ggplot2::scale_colour_gradient2() + 
    		ggplot2::facet_wrap(. ~ year, ncol = ncol, nrow = nrow) +
    		ggplot2::labs(
          x = "Longitude (decimal degrees)",
          y = "Latitude (decimal degrees)",
          colour = "Residuals"
        ) +
        ggplot2::coord_fixed() +
        ggplot2::theme_bw()
  )
  purrr::map2(
    .x = fs::path(dir, sprintf("residuals_page%d.png", seq_along(gg))),
    .y = gg,
    .f = ggsave,
    width = 14,
    height = ifelse(length(gg) == nrow * ncol, 10, 7)
  )
}
