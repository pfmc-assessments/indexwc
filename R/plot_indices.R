#' Plot the indices
#'
#' @details
#' TODO
#' * Think about adding some plotting information back in to have a title
#'   on the figure.
#'
#' @param data A data frame created by [sdmTMB::get_index()] with an additional
#'   column giving the `.id` of each index called `area`. If `area` is not
#'   not present, then all data points will be assumed to be from one default
#'   area. It behooves you to make `area` an ordered factor such that the areas
#'   are plotted in the order that you desire.
#' @param save_loc A string providing the location of the directory you wish to
#'   save the png file to. The default is your current working directory, and if
#'   `NULL` no directory will be created
#' @param file_name A string giving the file name. The default is
#'   `"index.png"`. If NULL, the file is not written but the ggplot object is returned
#' @param legend_loc Location for the legend to be added to the figure where
#'   the options are `"top"`, `"bottom"`, `"left"`, `"right"`, or a vector of
#'   length 2 giving relative coordinate positions. The default is `"right"`.
#'
#' @export
#' @author Chantel R. Wetzel
#' @return
#' Creates and saves a plot of the abundance indices in the directory location
#' specified by the save_loc function input.
#'
#' @importFrom rlang .data
plot_indices <- function(data,
                         save_loc = getwd(),
                         file_name = "index.png",
                         legend_loc = "right") {
  if (!"area" %in% colnames(data)) {
    data[["area"]] <- ""
  }
  gg <- ggplot2::ggplot(
    data = data,
    ggplot2::aes(
      x = .data$year,
      y = .data$est,
      group = .data$area,
      colour = .data$area,
      fill = .data$area
    )
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_line(lty = 2) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$lwr, ymax = .data$upr)
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      # If we want legend on the figure
      #   legend.justification = c(0, 1),
      #   legend.direction = "horizontal"
      legend.position = legend_loc,
    ) +
    ggplot2::scale_colour_viridis_d() +
    ggplot2::xlab("Year") +
    ggplot2::ylab("Index (mt)") +
    ggplot2::expand_limits(y = 0)
  if(!is.null(file_name)) {
    suppressMessages(ggplot2::ggsave(
      plot = gg,
      filename = fs::path(save_loc, file_name),
      width = 10,
      height = 7,
      dpi = 300
    ))
  }
  return(gg)
}
