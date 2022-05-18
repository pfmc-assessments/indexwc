create_pred_grid <- function(subdata, load = FALSE){

  if (load == FALSE){
    survey <- VASTWestCoast::convert_survey4vast(survey = "WCGBTS")
    grid <- VASTWestCoast::get_inputgrid(survey = survey)
    vast_grid <- grid
    vast_grid <- vast_grid[vast_grid$Area_km2 > 0, ]
    coordinates(vast_grid) <- c("Lon", "Lat")
    proj4string(vast_grid) <- CRS("+proj=longlat +datum=WGS84")
    newproj <- paste("+proj=utm +zone=10 ellps=WGS84 +datum=WGS84")
    grid_trans <- spTransform(vast_grid, CRS(newproj))
    grid_trans <- as.data.frame(grid_trans)
    vast_grid$X <- grid_trans[,"Lon"] / 1000 # convert to km
    vast_grid$Y <- grid_trans[,"Lat"] / 1000 # convert to km
    
    # Random setting to test - but definitely not correct
    vast_grid$vessel_scaled <- factor(median(as.numeric(subdata$vessel_scaled)))
    vast_grid$pass_scaled <- factor(median(as.numeric(subdata$pass_scaled)))
    
    # Create a grid for each year with all factors included
    year_grid <- NULL
    for(y in c(2003:2019, 2021)){
      vast_grid$Year <- y
      year_grid <- rbind(year_grid, 
        data.frame(vast_grid[, c("X", "Y", "Year", "Area_km2", "vessel_scaled", "pass_scaled")]))
    }
  } else {
    print('not done yet')
  }

}