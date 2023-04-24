#' Plot the annual random effects predictions by year 
#' for the fit model
#'
#'
#' @template predictions
#' @template dir 
#' @template nrow
#' @template ncol
#' @template verbose
#'
#'
#' @author Chantel R. Wetzel
#' @export
#'
#' @examples
#' \dontrun{
#'		predictions <- predict(
#'		    fit , 
#'		    newdata = prediction_grid
#'		) 
#'		predictions[, c("lon", "lat")] <- round(predictions[, c("Lon", "Lat")], 1)
#'		map_year_re(
#'			predictions = predictions, 
#'			dir = file.path(sppdir, survey, "index", obs)
#'		)
#'  }
#'
#'
map_year_re <- function(predictions, dir, nrow = 3, ncol = 4, verbose = FALSE){

	column <- ifelse(
		"epsilon_st2" %in% colnames(predictions),
		"epsilon_st2",
		ifelse("epsilon_st" %in% colnames(predictions),
		"epsilon_st", 
		"skip")
	)

	if(column != 'skip'){
		num_years <- sort(unique(predictions$Year))
		g <- split(
			num_years, 
			ceiling(seq_along(num_years) / (ncol * nrow))
		)
	
		for(page in 1:length(g)) {
  			map_nwfsc(predictions[predictions$Year %in% g[[page]], ], 
  				predictions[predictions$Year %in% g[[page]], column]) +
  				#scale_fill_gradient2() +
  				scale_fill_viridis_c(
  					  name = "Spatial \neffects",
  					  # trim extreme high values to make spatial variation more visible
  					  na.value = "yellow", 
  					  limits = c(quantile(predictions[, column], 0.005), 
  					  	quantile(predictions[, column], 0.995))
  					) +
  				facet_wrap(~Year, ncol = ncol, nrow = nrow) +
  				labs(x = "Longitude", y = "Latitude")   + 
  				ggtitle("Spatio-temporal random effects of the catch rate model")
  			
  			height <- ifelse(
    			length(g[[page]]) == nrow * ncol, 10, 7)

  			suppressMessages(ggsave(
    			filename = file.path(dir, paste0("year_random_effects_", page, ".png")), 
    			width = 10, 
    			height = height, 
    			units = 'in'
    		))
  		}
  	} else {
  		if(verbose){
			message('The espsilon_st column not found in the predictions. Random effects map not created.')
		}
  	}

}
