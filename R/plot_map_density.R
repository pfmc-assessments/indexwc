#' Plot the predicted biomass density by year.
#'
#'
#' @template predictions
#' @template dir 
#' @template nrow
#' @template ncol
#' @template verbose
#'
#'
#' @author Chantel Wetzel
#' @export
#' 
#'
#'
plot_map_density <- function(predictions, dir, ncol = 4, nrow = 3, verbose = FALSE){

	column <- ifelse(
		"est2" %in% colnames(predictions),
		"est2",
		ifelse("est" %in% colnames(predictions),
		"est", 
		"skip")
	)

	if(column != 'skip'){
		num_years <- sort(unique(predictions$Year))
		g <- split(
			num_years, 
			ceiling(seq_along(num_years) / (ncol * nrow))
		)
		for(page in 1:length(g)) {
			plot_map(predictions[predictions$Year %in% g[[page]], ], 
				exp(predictions[predictions$Year %in% g[[page]], column]) ) +
    			geom_tile() + 
    			labs(x = "Longitude", y = "Latitude") +
  				scale_fill_viridis_c(
  				  name = "exp(pred)",
  				  trans = "sqrt",
  				  # trim extreme high values to make spatial variation more visible
  				  na.value = "yellow", 
  				  limits = c(0, quantile(exp(predictions[, column]), 0.995))
  				) +
  				facet_wrap(~Year, ncol = ncol, nrow = nrow) +
  				ggtitle("Density Predictions: Fixed Effects + Random Effects")
  			
  			height <- ifelse(
    				length(g[[page]]) == nrow * ncol, 10, 7)
	
  			ggsave(
    			filename = file.path(dir, paste0("prediction_density_", page, ".png")), 
    			width = 10, 
    			height = height, 
    			units = 'in'
    		)
  		}
	} else {
		if(verbose){
			message('The est column not found in the predictions. Prediction density map not created.')
		}
	}

	
}

