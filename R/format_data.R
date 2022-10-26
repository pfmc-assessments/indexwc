#' Format data pull from the data warehouse
#'
#' 
#' 
#' 
#' 
#' @param data Catch data pulled from the data warehouse
#' 
#' @return A dataframe of formatted catch data.
#' 
#' 
#' @author Chantel Wetzel
#' @importFrom VASTWestCoast 
#' @export
#' 
#' @examples
#' formatted_data <- format_data(data = catch_data)
#' 
format_data <- function(data){
	# clean and add needed columns to the data
	data <- VASTWestCoast::clean_data(data = data)
	# add catch and cpue in terms of metric tons
	data[,"Catch_mt"] <- data[,"Total_catch_wt_kg"] / 1000
	data[, "Catch_KG"] <- data[,"Total_catch_wt_kg"]
	data[,"Area_swept_km2"] <- data[, "Area_swept_ha_der"] / 100
	data[,"cpue_mt_km2"] <- data[,"Catch_mt"] / data[,"Area_swept_km2"]
	# add effort = 1 column for specific VAST / sdmTMB comparisons
	data$effort <- 1
	# add factors and random effects terms
	data[,"pass_scaled"] <- data[,"Pass", drop = FALSE] - mean(range(data[,"Pass"]))
	data[,"vessel_scaled"] <- as.numeric(data[, "Vessel"], as.is = FALSE) - 1	
	return(data)
}