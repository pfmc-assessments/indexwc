#' Plot the quantile-quantile for `fit` with a 1:1 line
#'
#' @param data A list created by [sdmTMB::sdmTMB()].
#' @template dir
#'
#' @author Chantel R. Wetzel
#' @export
plot_qq <- function(data, dir) {
  residuals <- data[["data"]][["residuals"]]

	grDevices::png(
		filename = file.path(dir, "qq.png"),
    width = 7, 
    height = 7, 
    units = "in", 
    res = 300, 
    pointsize = 12
  )
  on.exit(dev.off)
  stats::qqnorm(residuals) 
  stats::qqline(residuals)

}
