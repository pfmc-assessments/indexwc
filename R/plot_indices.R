#' 
#'
#' 
#' 
#' 
#' 
#' 
#'
#' @param data Dataframe of the estimated indices of abundance 
#' created by the run_sdmtmb function.
#' @param plot_info Object created by the nwfscSurvey::GetSpp.fn
#' function that contain species name, scientific name, and default
#' areas to calculate the index of abundance.
#' @param save_loc Directory location to save the png file.
#' @param legend_loc Location for the legend to be added to the figure
#' where the options are "topleft", "bottomleft", "left",...
#' @param ymax NULL Maximum value on the y-axis for plotting. If left
#' as null the function will dynamically determine a ymax. 
#'
#' @export
#' @author Chantel Wetzel
#' @return Creates and saves a plot of the abundance indices
#' in the directory location specified by the save_loc function 
#' input.
#'
plot_indices <- function(data, plot_info, save_loc,
  legend_loc = "topright", ymax = NULL) {

  colors <- viridis::viridis(4)
  years <- sort(unique(data$Year))
  hi <- data[data$area == "coastwide", "upr"]
  lo <- data[data$area == "coastwide", "lwr"]
  est <- data[data$area == "coastwide", "est"]

  out_file = file.path(save_loc, paste0(plot_info$common_name, "_", plot_info$survey, "_Index.png"))
  grDevices::png(filename = out_file,
    width = 10, height = 7, units = "in", res = 300, pointsize = 12)

  cex.axis = 1.25
  cex.lab = 1.20
  if (is.null(ymax)) {
    ymax = max(hi) + 0.10 * max(hi)
    if(ymax > 3 * max(est)){
      ymax =  3 * max(est)
    }
  }

  plot(0, type = "n",
      xlim = range(years),
      ylim = c(0, ymax),
      xlab = "", ylab = "", yaxs = "i",
      main = "", cex.axis = cex.axis)

  graphics::mtext(side = 1 , "Year", cex = cex.lab, line = 3)
  graphics::mtext(side = 2, "Index (mt)", cex = cex.lab, line = 2.5)
  graphics::mtext(side = 3, text = stringr::str_to_title(plot_info$common),
    font = 2, cex = cex.lab, line = 0.25)
  graphics::mtext(side = 3, text = "",
    font = 2, cex = cex.lab, line = -1.75)
  graphics::arrows(x0 = years, y0 = lo, x1 = years, y1 = hi, col = colors[1],
    angle = 90, code = 3, length = 0.01)
  graphics::points(years, est, pch = 17, bg = 1, cex = 1.6, col = colors[1])
  graphics::lines(years,  est,cex = 1, lwd = 2, col = colors[1])

  # Area
  legend <- NULL; aa <- 2;  x <- 0.05
  for (area in unique(data[data$area != "coastwide", "area"])){  
    hi  <- data[data$area == area, "upr"]
    lo  <- data[data$area == area, "lwr"]
    est <- data[data$area == area, "est"]
    graphics::arrows(x0 = years + x, y0 = lo, x1 = years + x, y1 = hi, 
      angle = 90, code = 3, length = 0.01, col = colors[aa], 
      lty = 1)
    graphics::points(years + x, est, pch = 16, bg = 1, cex = 1.6, col = colors[aa])
    graphics::lines( years + x,  est, cex = 1, col = colors[aa], lty = 2, lwd = 2)
    legend <- c(legend, toupper(area))
    aa <- aa + 1
    x  <- x + 0.05
  }

  legend(legend_loc, bty = 'n', 
    lty = c(1, rep(2, length(unique(data$area)) - 1)),
    legend = c("Coastwide", legend), 
    col = colors, 
    cex = rep(1.25, length(unique(data$area))),
    pch = c(17, rep(16, length(unique(data$area)) - 1)),
    lwd = rep(2, length(unique(data$area)))
  )
  dev.off()

}