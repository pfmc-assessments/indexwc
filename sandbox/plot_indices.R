
plot_indices <- function(data, main_name, save_loc, ymax = NULL) {

  years = sort(unique(data$year))
  vast_est = data[data$index == "VAST","est"]
  sdmtmb_est = data[data$index == "sdmTMB","est"]

  hi_sdmtmb <- exp(log(sdmtmb_est) + 1.96 * data[data$index == "sdmTMB","se"])
  lo_sdmtmb <- exp(log(sdmtmb_est) - 1.96 * data[data$index == "sdmTMB","se"])
  hi_vast <- stats::qlnorm(0.975, meanlog = log(vast_est), sdlog = data[data$index == "VAST", "se"])
  lo_vast <- stats::qlnorm(0.025, meanlog = log(vast_est), sdlog = data[data$index == "VAST", "se"]) 

  out_file = file.path(save_loc, "Index_Comparison.png")
  grDevices::png(filename = out_file,
    width = 10, height = 7, units = "in", res = 300, pointsize = 12)
  #on.exit(grDevices::dev.off(), add = TRUE)

  cex.axis = 1.25
  cex.lab = 1.20
  if (is.null(ymax)) {
    ymax = max(hi_sdmtmb, hi_vast) + 0.02
    if(ymax > 3*max(vast_est, sdmtmb_est)){
      ymax =  3*max(vast_est, sdmtmb_est)
    }
  }

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

  graphics::arrows(x0 = years, y0 = lo_sdmtmb, x1 = years, y1 = hi_sdmtmb, 
    angle = 90, code = 3, length = 0.01, col = "lightskyblue4", lty = 2)
  graphics::points(years, sdmtmb_est, pch = 16, bg = 1, cex = 1.6, col = 'blue')
  graphics::lines(years,  sdmtmb_est, cex = 1, col = 'blue')

  legend("topright", bty = 'n', legend = c("sdmTMB", "VAST"), col = c('blue', 'black'), 
    lwd = 2)
  dev.off()

  out_file = file.path(save_loc, "Index_Comparison_2_panel.png")
  grDevices::png(filename = out_file,
    width = 10, height = 7, units = "in", res = 300, pointsize = 12)
  par(mfrow = c(2, 1), cex = 0.8, mar = c(1.5, 1, 1, 1), oma = c(2, 3, 1, 1))
  #on.exit(grDevices::dev.off(), add = TRUE)
  ymax = max(hi_vast) + 0.02

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

  ymax = max(hi_sdmtmb) + 0.02
  plot(0, type = "n",
      xlim = range(years),
      ylim = c(0, ymax),
      xlab = "", ylab = "", yaxs = "i",
      main = "", cex.axis = cex.axis)

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
  graphics::points(years, mean_vast, pch = 16, bg = 1, cex = 1.6)
  graphics::lines(years,  mean_vast, col = 1, cex = 1)

  graphics::points(years, mean_sdmtmb, pch = 16, bg = 1, cex = 1.6, col = 'blue')
  graphics::lines(years,  mean_sdmtmb, cex = 1, col = 'blue')

  legend("topright", bty = 'n', legend = c("sdmTMB", "VAST"), col = c('blue', 'black'), 
    lwd = 2)
  dev.off()

}