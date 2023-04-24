#' Plot the predicted biomass density by year.
#'
#'
#' @param predictions Output from [stats::predict()].
#' @param save_prefix A string providing the desired prefix to use when saving
#'   the density plots. The default is to save them in the current working
#'   directory using the name `"density[0-9]{2}.png"`. Obviously, the same
#'   outcome would result from just `"density"` but the use of `file.path()`
#'   in the default is present to provide an example for users who want to
#'   change the path to something other than their current directory.
#' @param n_row,n_col Integers specifying the number of rows and columns for
#'   the faceted figures. The default is one row and two columns.
#' @param column_grep A regular expression string that will be used to search
#'   the column names of `predictions` to find the column name that will be
#'   passed to `ggplot2::geom_tile(ggplot2::aes(fill = ))`. The default results
#'   in either `"est"` or `"est2"` depending on if the model is a delta model
#'   or not.
#' @param tile_size A vector of length two with numeric values specifying the
#'   tile width and height that will be passed to
#'   `ggplot2::geom_tile(width, height)`.`
#'
#' @author Chantel R. Wetzel
#' @export
#' @return
#' A {ggplot2}/{ggforce} object is returned.
#' @seealso
#' * [ggplot2::geom_tile()] for how the raster is created
#' * [ggforce::facet_wrap_paginate()] for how the pages are created
#' * [ggforce::n_pages()] for how many pages the object has
#'
map_density <- function(predictions,
                        save_prefix = file.path(getwd(), "density"),
                        n_row = 1,
                        n_col = 2,
                        column_grep = "^est[2]*$",
                        tile_size = c(2000, 2000)) {

  column <- grep(column_grep, colnames(predictions), value = TRUE)
  predictions <- dplyr::rename(
    .data = predictions,
    plot_me = column
  ) %>%
    suppressWarnings(sdmTMB::add_utm_columns(
      ll_crs = utm_zone_10
    ))

  data_extent <- raster::extent(predictions[, c("x", "y")])
  data_raster <- raster::raster(data_extent,
    ncol = floor((slot(data_extent, "xmax") - slot(data_extent, "xmin")) / 2),
    nrow = floor((slot(data_extent, "ymax") - slot(data_extent, "ymin")) / 2)
  )
  data_grouped <- predictions %>%
    dplyr::group_by(year)
  split_names <- unlist(dplyr::group_keys(data_grouped))
  x <- purrr::map(
    data_grouped %>%
      dplyr::group_split(),
    .f = ~ raster::rasterize(
      x = data.frame(.x[["x"]], .x[["y"]]),
      y = data_raster,
      field = .x[["plot_me"]],
      fun = mean
    ) %>%
      raster::rasterToPoints() %>%
      as.data.frame()
  )
  names(x) <- split_names

 gg <- map_base() +
    ggplot2::geom_tile(
      data = purrr::list_rbind(x, names_to = "year"),
      mapping = aes(x * 1000, y * 1000, fill = layer)
    ) +
    scale_fill_viridis_c() +
    scale_colour_viridis_c() +
    labs(fill = "Predicted\nln(density)") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
    ) +
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

ggsave_year <- function(x, y, prefix = "density_", n_row = 1, n_col = 2) {
  save_me <- y +
    ggforce::facet_wrap_paginate("year", nrow = n_row, ncol = n_col, page = x)
  suppressMessages(ggplot2::ggsave(
    plot = save_me,
    filename = sprintf("%s%02d.png", prefix, x)
  ))
}
