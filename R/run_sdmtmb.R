#' 
#'
#' @param dir Directory location
#' @param species 
#' @param dist 
#'
#' @export
#' @author Chantel Wetzel
#' @return 
#' @example
#' 
#' devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/nwfscSurvey")
#' run_sdmtmb( dir = "C:/Assessments/2022/sdmTMB/create_run_code",
#' 	species = "petrale sole",
#' 	dist = c("lognormal", "gamma")
#' 
#' 
#'  options to build in
#'  1) select to run over specific surveys
#'  2) add input to turn on/off vessel effects
#'  3) if vessel effects included determine if sd > 0 (model convergence)
#'  4) option to turn off area estimates and plotting

run_sdmtmb <- function(dir, species, dist = c("lognormal", "gamma")){

knots <- 250
#### Get species, survey, and strata info
info <- nwfscSurvey::GetSpp.fn(species)
survey_list <- rev(grep("Tri|Combo|Slope",
  nwfscSurvey::createMatrix()[, 1], value = TRUE))[2]

strata_limits <- VASTWestCoast::convert_strata4vast(
	overridedepth = TRUE,
  strata = nwfscSurvey::GetStrata.fn(info[, "strata"])
)

sppdir <- file.path(normalizePath(dir, mustWork = FALSE),
  info[, "common_name"])
dir.create(sppdir,recursive = TRUE, showWarnings = FALSE)

for (survey in survey_list){

	dir.create(file.path(sppdir, survey), recursive = TRUE, showWarnings = FALSE)
	dir.create(file.path(sppdir, survey, "data"), recursive = TRUE, showWarnings = FALSE)
	dir.create(file.path(sppdir, survey, "index"), recursive = TRUE, showWarnings = FALSE)

	# modify for other surveys & whether to include vessel effects
	if (survey == "NWFSC.Combo"){
		# with random vessel effects
		formula = list(
			Catch_mt ~ 0 + as.factor(Year) + pass_scaled + (1 | vessel_scaled),
		  Catch_mt ~ 0 + as.factor(Year) + pass_scaled + (1 | vessel_scaled))
		# without RE
		formula = list(
			Catch_mt ~ 0 + as.factor(Year) + pass_scaled,
		  Catch_mt ~ 0 + as.factor(Year) + pass_scaled)
	} else {
		formula = list(
			Catch_mt ~ 0 + as.factor(Year) + pass_scaled,
		  Catch_mt ~ 0 + as.factor(Year) + pass_scaled)
	}

	spp <- paste(survey, info[, "scientific_name"], sep = "_")

	# Pull survey data
	data <- nwfscSurvey::pull_catch(
		common_name = info$common,
		survey = survey,
		dir = file.path(sppdir, survey, "data"))

	catch_data <- format_data(data = data)
	surveyspp <- VASTWestCoast::get_spp(input = spp)
	data_years <- sort(unique(data$Year))

	for (obs in dist) {

		# create subdirectory folder
		dir.create(file.path(sppdir, survey, "index", obs), recursive = TRUE, showWarnings = FALSE)

		if (obs == "gamma"){
			family = sdmTMB::delta_gamma()
		} 
		if (obs == "lognormal"){
			family = sdmTMB::delta_lognormal()
		} 
		if (!obs %in% c("gamma", "lognormal")){
			# Add error statement and stop run
		}
		
		# Create mesh
		mesh_survey <- VASTWestCoast::convert_survey4vast(
			survey = surveyspp["survey"])

		mesh <- VASTWestCoast::VAST_mesh(
			data = catch_data,
		  survey = mesh_survey,
		  numknots = knots,
		  savedir = file.path(sppdir, survey, "index", obs))
		data_mesh <- mesh$mesh$data.inner
				
		# Create prediction grid
		grid <- VASTWestCoast::get_inputgrid(survey = mesh_survey)
		grid <- grid[grid$Area_km2 > 0, ]
		grid <- sdmTMB::add_utm_columns(grid, c("Lon", "Lat"), utm_crs = 32610) # UTM 10
		grid$pass_scaled <- 0
		grid$vessel_scaled <- data_mesh$vessel_scaled[1]
		sub_grid <- dplyr::select(grid, X, Y, Area_km2, pass_scaled, vessel_scaled, Lat, Lon)
		
		year_grid <- purrr::map_dfr(data_years, function(yr) {
		  sub_grid$Year <- yr
		  sub_grid
		})

		mesh <- sdmTMB::make_mesh(
    	data = data_mesh,
    	xy_cols = c("X", "Y"),
    	n_knots = knots
	  )

		# plot and save the mesh
		grDevices::png(filename = file.path(sppdir, survey, "index", obs, "mesh.png"),
      width = 7, height = 7, units = "in", res = 300, pointsize = 12)
    plot(mesh)
    dev.off()

		data_mesh$vessel_scaled <- as.factor(data_mesh$vessel_scaled)
		
		# Run model
		fit <- sdmTMB::sdmTMB(
  		formula = formula,
  		time = "Year",
  		offset = log(data_mesh$Area_swept_km2),
  		data = data_mesh,
  		mesh = mesh,
  		family = family,
  		spatial = "on",
  		spatiotemporal = list("iid", "iid"),
  		anisotropy = TRUE,
  		silent = TRUE,
  		control = sdmTMB::sdmTMBcontrol(
  		  newton_loops = 1L,
  		  map = list(ln_H_input = factor(c(1, 2, 1, 2))) # <- force sdmTMB to share anisotropy parameters across the two delta models
  		),
  		do_index = TRUE,
  		predict_args = list(newdata = year_grid, re_form_iid = NA),
  		index_args = list(area = year_grid$Area_km2)
		)

		index <- sdmTMB::get_index(
				  fit, 
				  bias_correct = FALSE # TRUE
		)
		index$area <- "coastwide" 

		loglike <- logLik(fit)
		aic <- AIC(fit)

		# Create indices by area
		area_indices <- NULL 
		for (strata in strata.limits[-1,"STRATA"]){

			sub_year_grid <- year_grid %>% 
				dplyr::filter(Lat < strata.limits[strata.limits$STRAT == strata, "north_border"]) %>%
				dplyr::filter(Lat >= strata.limits[strata.limits$STRAT == strata, "south_border"])
			
			pred_area <- predict( #does not work with sdmTMB::predict
				fit, 
				newdata = sub_year_grid,
			  return_tmb_object = TRUE
			)

			sub_index <- sdmTMB::get_index(
		  	pred_area, #fit, # skipping prediction step
		  	area = sub_year_grid$Area_km2,
		  	bias_correct = FALSE # TRUE
			)

			sub_index$area <- strata
			area_indices <- rbind(area_indices, sub_index)
		}

		# Need to fix this for plotting
		all_indices <- rbind(index, area_indices)
		# Plot the index
		plot_indices(data = all_indices, 
			plot_info = info, 
			save_loc = file.path(sppdir, survey, "index", obs), 
			ymax = NULL)

		# Add diagnostics
		# 1) QQ plot
		# 2) Residuals by year
		resids <- residuals(fit)
    grDevices::png(filename = file.path(sppdir, survey, "index", obs, "qq.png"),
      width = 7, height = 7, units = "in", res = 300, pointsize = 12)
    qqnorm(resids); qqline(resids)
    dev.off()

    catch_data$resids <- resids
    grDevices::png(filename = file.path(sppdir, survey, "index", obs, "residuals.png"),
      width = 14, height = 7, units = "in", res = 300, pointsize = 12)
    ggplot(catch_data, aes(Lon, Lat, colour = resids)) + 
    	geom_point() + 
    	facet_wrap(~Year) +
    	scale_colour_gradient2() + 
    	coord_fixed()
    dev.off()

    save(data_mesh, mesh, year_grid, index, fit, area_indices, all_indices,
			loglike, aic, catch_data, plot_info,
  		file = file.path(sppdir, survey, "index", obs, "sdmTMB_save.RData")
		)

	} # end obs loop over distributions
} # end survey loop


}