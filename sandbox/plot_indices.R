
plot_indices <- function(data, main_name, save_loc, ymax = NULL) {

  years = sort(unique(data$year))
  vast_est = data[data$index == "VAST","est"]
  sdmtmb_est = data[data$index == "sdmTMB","est"]

  hi_sdmtmb <- data[data$index == "sdmTMB", "upr"]
  lo_sdmtmb <- data[data$index == "sdmTMB", "lwr"]
  hi_vast <- data[data$index == "VAST", "upr"]
  lo_vast <- data[data$index == "VAST", "lwr"]

  out_file = file.path(save_loc, "Index_Comparison.png")
  grDevices::png(filename = out_file,
    width = 10, height = 7, units = "in", res = 300, pointsize = 12)
  #on.exit(grDevices::dev.off(), add = TRUE)

  cex.axis = 1.25
  cex.lab = 1.20
  if (is.null(ymax)) {
    ymax = max(hi_sdmtmb, hi_vast) + 0.10 * max(hi_sdmtmb, hi_vast)
    if(ymax > 3 * max(vast_est, sdmtmb_est)){
      ymax =  3 * max(vast_est, sdmtmb_est)
    }
  }

  x <- 0.04

  plot(0, type = "n",
      xlim = range(years),
      ylim = c(0, ymax),
      xlab = "", ylab = "", yaxs = "i",
      main = "", cex.axis = cex.axis)

  graphics::mtext(side = 1 , "Year", cex = cex.lab, line = 3)
  graphics::mtext(side = 2, "Index (mt)", cex = cex.lab, line = 2.5)
  graphics::mtext(side = 3, text = main_name,
    font = 2, cex = cex.lab, line = 0.25)
  graphics::mtext(side = 3, text = "",
    font = 2, cex = cex.lab, line = -1.75)
  graphics::arrows(x0 = years, y0 = lo_vast, x1 = years, y1 = hi_vast, 
    angle = 90, code = 3, length = 0.01, col = "darkgrey")
  graphics::points(years, vast_est, pch = 17, bg = 1, cex = 1.6)
  graphics::lines(years,  vast_est, col = 1, cex = 1, lwd = 2)

  graphics::arrows(x0 = years + x, y0 = lo_sdmtmb, x1 = years + x, y1 = hi_sdmtmb, 
    angle = 90, code = 3, length = 0.01, col = "red", #"lightskyblue4", 
    lty = 2)
  graphics::points(years + x, sdmtmb_est, pch = 16, bg = 1, cex = 1.6, col = 'red')
  graphics::lines(years + x,  sdmtmb_est, cex = 1, col = 'red', lty = 2, lwd = 2)

  legend("topright", bty = 'n', 
    lty = c(1,2),
    legend = c("VAST", "sdmTMB"), 
    col = c('black', 'red'), 
    lwd = 2)
  dev.off()

  out_file = file.path(save_loc, "Index_Comparison_2_panel.png")
  grDevices::png(filename = out_file,
    width = 10, height = 7, units = "in", res = 300, pointsize = 12)
  par(mfrow = c(2, 1), cex = 0.8, mar = c(1.5, 1, 1, 1), oma = c(2, 3, 1, 1))
  #on.exit(grDevices::dev.off(), add = TRUE)
  ymax = max(hi_vast) + 0.02 * max(hi_vast) 

  plot(0, type = "n",
      xlim = range(years),
      ylim = c(0, ymax),
      xlab = "", ylab = "", yaxs = "i",
      main = "", cex.axis = cex.axis)

  graphics::mtext(side = 1 , "Year", cex = cex.lab, line = 3)
  graphics::mtext(side = 2, "Index", cex = cex.lab, line = 2.5)
  graphics::mtext(side = 3, text = main_name,
    font = 2, cex = cex.lab, line = 0.25)
  graphics::mtext(side = 3, text = "",
    font = 2, cex = cex.lab, line = -1.75)
  graphics::arrows(x0 = years, y0 = lo_vast, x1 = years, y1 = hi_vast, 
    angle = 90, code = 3, length = 0.01, col = "grey")
  graphics::points(years, vast_est, pch = 16, bg = 1, cex = 1.6)
  graphics::lines(years,  vast_est, col = 1, cex = 1)

  ymax = max(hi_sdmtmb) + 0.10 *  max(hi_sdmtmb) 
  plot(0, type = "n",
      xlim = range(years),
      ylim = c(0, ymax),
      xlab = "", ylab = "", yaxs = "i",
      main = "", cex.axis = cex.axis)
  graphics::mtext(side = 2, "Index", cex = cex.lab, line = 2.5)
  graphics::arrows(x0 = years, y0 = lo_sdmtmb, x1 = years, y1 = hi_sdmtmb, 
    angle = 90, code = 3, length = 0.01, col = "lightskyblue4", lty = 2)
  graphics::points(years, sdmtmb_est, pch = 16, bg = 1, cex = 1.6, col = 'blue')
  graphics::lines(years,  sdmtmb_est, cex = 1, col = 'blue')

  legend("topright", bty = 'n', legend = c("sdmTMB", "VAST"), col = c('blue', 'black'), 
    lwd = 2)
  dev.off()


  mean_sdmtmb  = sdmtmb_est / mean(sdmtmb_est)
  mean_vast = vast_est / mean(vast_est)
  max_y = max(mean_sdmtmb, mean_vast) + 0.20
  min_y = min(mean_sdmtmb, mean_vast) - 0.20

  out_file = file.path(save_loc, "Index_Comparison_Mean_Standardized.png")
  grDevices::png(filename = out_file,
    width = 10, height = 7, units = "in", res = 300, pointsize = 12)

  plot(0, type = "n",
      xlim = range(years),
      ylim = c(min_y, max_y),
      xlab = "", ylab = "", yaxs = "i",
      main = "", cex.axis = cex.axis)

  graphics::mtext(side = 1 , "Year", cex = cex.lab, line = 3)
  graphics::mtext(side = 2, "Mean Standardized Index", cex = cex.lab, line = 2.5)
  graphics::mtext(side = 3, text = main_name,
    font = 2, cex = cex.lab, line = 0.25)
  graphics::mtext(side = 3, text = "",
    font = 2, cex = cex.lab, line = -1.75)
  graphics::points(years, mean_vast, pch = 17, bg = 1, cex = 1.6)
  graphics::lines(years,  mean_vast, col = 1, cex = 1)

  graphics::points(years, mean_sdmtmb, pch = 16, bg = 1, cex = 1.6, col = 'blue')
  graphics::lines(years,  mean_sdmtmb, cex = 1, col = 'blue')

  legend("topright", bty = 'n', legend = c("sdmTMB", "VAST"), col = c('blue', 'black'), 
    lwd = 2)
  dev.off()

}