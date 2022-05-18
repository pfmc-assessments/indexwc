format_data <- function(Out){
	# clean and add needed columns to the data
	data <- VASTWestCoast::clean_data(data = Out)
	# add catch and cpue in terms of metric tons
	data[,"Catch_mt"] <- data[,"Catch_KG"] / 1000
	data[,"cpue_mt_km2"] <- data[,"Catch_mt"] / data[,"AreaSwept_km2"]
	units(data[, "Catch_mt"]) <- 't'
	# add effort = 1 column for specific VAST / sdmTMB comparisons
	data$effort <- 1
	# add factors and random effects terms
	data[,"pass_scaled"] <- data[,"Pass", drop = FALSE] - mean(range(data[,"Pass"]))
	data[,"vessel_scaled"] <- as.numeric(data[, "Vessel"], as.is = FALSE) - 1	
	return(data)
}