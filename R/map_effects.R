#' Plot the fixed and spatial effects from the fit predictions
#'
#'
#' @template predictions
#' @template dir
#' @template verbose
#'
#' @import ggplot2
#' @import gridExtra
#'
#' @author Chantel R. Wetzel
#' @export
map_effects <- function(predictions, dir, verbose = FALSE){
	column <- ifelse(
		"est_non_rf2" %in% colnames(predictions),
		"est_non_rf2",
		ifelse("est_non_rf" %in% colnames(predictions),
		"est_non_rf", 
		"skip")
	)

	if(column != 'skip'){
	p1 <- map_nwfsc(predictions, exp(predictions[,column])) +
  		scale_fill_viridis_c(trans = "sqrt", name = "Fixed \nEffects") +
  		labs(x = "Longitude", y = "Latitude") + 
  		ggtitle("Fixed pass effects")		
	} else {
		if(verbose){
			message('The est_non_rf column not found in the predictions. Fixed effects map not created.')
		}
	}


  	column <- ifelse(
		"omega_s2" %in% colnames(predictions),
		"omega_s2",
		ifelse("omega_s" %in% colnames(predictions),
		"omega_s", 
		"skip")
	)

  	if(column != 'skip'){
  	p2 <- map_nwfsc(predictions, predictions[,column]) +
  		scale_fill_viridis_c(name = "Random \nEffects") +
  		labs(x = "Longitude", y = "Latitude") +
  		ggtitle("Spatial random effects")  		
  	} else {
  		if(verbose){
			message('The omega_s column not found in the predictions. Spatial random effects map not created.')
		}
  	}

  	g <- gridExtra::grid.arrange(p1, p2, nrow = 1)

  	suppressMessages(ggsave(
  		plot = g,
    	filename = file.path(dir, paste0("fixed_and_spatial_effects.png")), 
    	width = 14, height = 10, units = 'in'
    ))
}