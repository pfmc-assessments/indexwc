#' Plot the data from a model fit by [sdmTMB::sdmTMB()]
#'
#' @param data A data frame with at least the following three columns:
#'   * `x`,
#'   * `y`, and
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
                          save_prefix = file.path(getwd(), "data_")) {

  # data_extent <- raster::extent(data[, c("x", "y")])
  # data_raster <- raster::raster(data_extent,
  #   ncol = floor((slot(data_extent, "xmax") - slot(data_extent, "xmin")) / 2),
  #   nrow = floor((slot(data_extent, "ymax") - slot(data_extent, "ymin")) / 2)
  # )
  # proj4string(data_raster) <- CRS(paste("+proj=utm +zone=10 ellps=WGS84"))
  # data_grouped <- data %>%
  #   dplyr::group_by(year)
  # split_names <- unlist(dplyr::group_keys(data_grouped))
  # x <- purrr::map(
  #   data_grouped %>%
  #     dplyr::group_split(),
  #   .f = ~ raster::rasterize(
  #     x = data.frame(.x[["x"]], .x[["y"]]),
  #     y = data_raster,
  #     field = .x[["residuals"]],
  #     fun = mean
  #   ) %>%
  #     raster::rasterToPoints() %>%
  #     as.data.frame()
  # )
  # names(x) <- split_names

  gg <- map_base() +
    ggplot2::geom_point(
      data = data,
      ggplot2::aes(x * 1000, y * 1000, colour = residuals, fill = residuals),
      pch = 15,
      alpha = 0.5
    ) +
    # ggplot2::geom_tile(
    #   data = purrr::list_rbind(x, names_to = "year"),
    #   mapping = aes(x * 1000, y * 1000, fill = layer),
    #   width = 10000,
    #   height = 10000
    # ) +
    scale_fill_viridis_c() +
    scale_colour_viridis_c() +
    labs(fill = "Residuals") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
    ) +
    ggplot2::guides(colour = "none") +
    ggforce::facet_wrap_paginate("year", nrow = n_row, ncol = n_col)

  purrr::map(
    seq(ggforce::n_pages(gg)),
    .f = ggsave_year,
    y = gg,
    prefix = save_prefix,
    n_col = n_col,
    n_row = n_row
  )
  return(gg)
}
