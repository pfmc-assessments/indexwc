#' Plot the QQ and 1:1 line
#'
#' @param data List created by the sdmTMB:: fit function
#' @template dir 
#'
#' 
#'
#' @author Chantel Wetzel
#' @export
#'
#' 
#'	
plot_qq <- function(data, dir){

    resids <- fit$data$residuals

	grDevices::png(
		filename = file.path(dir, "qq.png"),
      	width = 7, 
      	height = 7, 
      	units = "in", 
      	res = 300, 
      	pointsize = 12
    )
    stats::qqnorm(resids) 
    stats::qqline(resids)
    dev.off()

}