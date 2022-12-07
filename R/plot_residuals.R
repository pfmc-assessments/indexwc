#' Plot the estimated model residuals from
#' the model fit parameters.
#'
#' @param data List created by the sdmTMB:: fit function
#' @template dir 
#' @template nrow
#' @template ncol
#'
#' @import ggplot2
#' 
#'
#' @author Chantel Wetzel
#' @export
#' 
#'
#' 
#'
plot_residuals<- function(data, dir, nrow = 3, ncol = 4){
	
	year <- data$time
	df <- data$data
	num_years <- sort(unique(df[, year]))
	g <- split(
		num_years, 
		ceiling(seq_along(num_years) / (ncol * nrow))
	)

	for(page in 1:length(g)) {
    	ggplot2::ggplot(df[df$Year %in% g[[page]], ], 
    		aes(Lon, Lat, colour = residuals)) + 
    		geom_point() + 
    		scale_colour_viridis_c() +
    		facet_wrap(~Year, ncol = ncol, nrow = nrow) +
    		#scale_colour_gradient2() + 
    		labs(x = "Longitude", y = "Latitude", colour = "Residuals") 

    	height <- ifelse(
    		length(g[[page]]) == nrow * ncol, 10, 7)
    	ggsave(
    	  filename = file.path(dir, paste0("residuals_page", page, ".png")), 
    	  width = 14, height = height, units = 'in')
    }
}