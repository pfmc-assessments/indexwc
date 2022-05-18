###########################################################################
# All example set-ups below use the following settings with the observation
# model set to gamma
###########################################################################
	FieldConfig <- matrix(c("IID", "IID", "IID", "IID", "IID", "IID"),
	  ncol = 2, nrow = 3,
	  dimnames = list(
	    c("Omega", "Epsilon", "Beta"),
	    c("Component_1", "Component_2")
	  )
	)
	RhoConfig <- c("Beta1" = 0, "Beta2" = 0, "Epsilon1" = 0, "Epsilon2" = 0)
	
	settings <- FishStatsUtils::make_settings(
	  n_x = 300, # number of vertices in the SPDE mesh
	  Region = "User",
	  purpose = "index2", # use recommended defaults for an index of abundance
	  fine_scale = TRUE, # use bilinear interpolation from the INLA 'A' matrix
	  FieldConfig = FieldConfig,
	  RhoConfig = RhoConfig,
	  ObsModel = c(2, 0), # conventional logit-linked delta-Gamma; c(10, 2) for Tweedie
	  bias.correct = FALSE,
	  use_anisotropy = FALSE,
	  max_cells = Inf, # use all grid cells from the extrapolation grid
	  knot_method = "samples" # "samples" or "grid"
	)

###########################################################################
# Simple model with effort = 1 (can produce matching estimates)
###########################################################################
	fit <- fit_model(
	  settings = settings,
	  Lat_i = subdata[, "Lat"],
	  Lon_i = subdata[, "Lon"],
	  t_i = subdata[, "Year"],
	  b_i = subdata[,'Catch_mt'], 
	  a_i = subdata[,"effort"]	  
	  working_dir = dir,
	  input_grid = vast_mesh[['inputgrid']]
	)

	fit_sdmTMB <- sdmTMB::sdmTMB(
  		catch_mt ~ 0 + as.factor(Year),
  		time = "Year",
  		offset = log(subdata$effort),
  		data = subdata,
  		mesh = mesh_sdmTMB,
  		family = delta_gamma(),
  		spatial = "on",
  		spatiotemporal = "iid",
  		control = sdmTMBcontrol(newton_loops = 1), # match VAST default settings
  		silent = TRUE
	)

	# Prediction and get_index calls do not change
	pred <- predict(
	  fit_sdmTMB,
	  newdata = year_grid, 
	  return_tmb_object = TRUE 
	)
	
	# Create index with bias correction
	index_sdmTMB <- sdmTMB::get_index(pred, 
	  bias_correct = FALSE,
  	  area = year_grid$Area_km2)

###################################################################
# catch_mt ~ 0 + Year using area swept 
	fit <- fit_model(
	  settings = settings,
	  Lat_i = subdata[, "Lat"],
	  Lon_i = subdata[, "Lon"],
	  t_i = subdata[, "Year"],
	  b_i = subdata[,'Catch_mt'],  
	  a_i = subdata[, "AreaSwept_km2"], 
	  working_dir = dir,
	  input_grid = vast_mesh[['inputgrid']]
	)

	fit_sdmTMB <- sdmTMB::sdmTMB(
  		catch_mt ~ 0 + as.factor(Year),
  		time = "Year",
  		offset = log(subdata$AreaSwept_km2),
  		data = subdata,
  		mesh = mesh_sdmTMB,
  		family = delta_gamma(),
  		spatial = "on",
  		spatiotemporal = "iid",
  		control = sdmTMBcontrol(newton_loops = 1), # match VAST default settings
  		silent = TRUE
	)

#############################################################################
# catch_mt ~ 0 + year + 1|vessel_scaled (vessel is a random effect)
#############################################################################
	fit <- fit_model(
	  settings = settings,
	  Lat_i = subdata[, "Lat"],
	  Lon_i = subdata[, "Lon"],
	  t_i = subdata[, "Year"],
	  b_i = subdata[,'Catch_mt'], 
	  a_i = subdata[, "AreaSwept_km2"],
	  v_i = subdata[, "vessel_scaled"],
	  working_dir = dir,
	  input_grid = vast_mesh[['inputgrid']]
	)

	fit_sdmTMB <- sdmTMB::sdmTMB(
  		catch_mt ~ 0 + as.factor(Year) + as.factor(1|vessel_scaled),
  		time = "Year",
  		offset = log(subdata$AreaSwept_km2),
  		data = subdata,
  		mesh = mesh_sdmTMB,
  		family = delta_gamma(),
  		spatial = "on",
  		spatiotemporal = "iid",
  		control = sdmTMBcontrol(newton_loops = 1), # match VAST default settings
  		silent = TRUE
	)

#############################################################################
# catch_mt ~ 0 + year + 1|vessel_scaled + 1|pass 
# vessel is a random effect - not sure what pass would be categorized as here
# Q1_formula description from VAST:
# \code{Q1_formula} affects the 1st linear predictor for samples.
# However, the effect of \code{Q1_formula} is not used when predicting values at extrapolation-grid locations.
# Therefore, the \code{Q1_formula} is interpreted as affecting "catchability" (a.k.a. "detectabiility"), and it represents
# processes that affect the outcome of sampling but not the "true" underlying value of a variable being sampled.
# For example, a factor representing gear-type might be included to estimate the relative performacne of each gear type
# (relative to the base level of that factor).
# Note that \code{Q1_formula} defines a relationship that is applied to all samples (regardless of category \code{c_i}),
# whereas \code{X1_formula} defines a relationship that is estimated independently for each category.
# For a catchability covariate that varies by category, please include the category as factor in \code{catchability_data}
# and then include an interaction with category in \code{Q1_formula} for any variable which has an effect that varies among categories.
#############################################################################

	fit <- fit_model(
	  settings = settings,
	  Lat_i = subdata[, "Lat"],
	  Lon_i = subdata[, "Lon"],
	  t_i = subdata[, "Year"],
	  b_i = subdata[,'Catch_mt'], 
	  a_i = subdata[, "AreaSwept_km2"], 
	  v_i = subdata[, "vessel_scaled"],
	  Q1_formula = ~ Pass,
	  Q2_formula = ~ Pass,
	  catchability_data = subdata$pass_scaled,
	  working_dir = dir,
	  input_grid = vast_mesh[['inputgrid']]
	)

	# I am sure this is not the correct setting for the pass
	fit_sdmTMB <- sdmTMB::sdmTMB(
  		catch_mt ~ 0 + as.factor(Year) + as.factor(1|vessel_scaled) + as.factor(1|pass_scaled),
  		time = "Year",
  		offset = log(subdata$AreaSwept_km2),
  		data = subdata,
  		mesh = mesh_sdmTMB,
  		family = delta_gamma(),
  		spatial = "on",
  		spatiotemporal = "iid",
  		control = sdmTMBcontrol(newton_loops = 1), # match VAST default settings
  		silent = TRUE
	)

###########################################################################
# Anistropy - All above examples set this to FALSE, but woul ultimately like to 
# run with this turned on (TRUE) 
# I think the following set-up would be as follows
#
###########################################################################
	FieldConfig <- matrix(c("IID", "IID", "IID", "IID", "IID", "IID"),
	  ncol = 2, nrow = 3,
	  dimnames = list(
	    c("Omega", "Epsilon", "Beta"),
	    c("Component_1", "Component_2")
	  )
	)
	RhoConfig <- c("Beta1" = 0, "Beta2" = 0, "Epsilon1" = 0, "Epsilon2" = 0)
	
	settings <- FishStatsUtils::make_settings(
	  n_x = 300, # number of vertices in the SPDE mesh
	  Region = "User",
	  purpose = "index2", # use recommended defaults for an index of abundance
	  fine_scale = TRUE, # use bilinear interpolation from the INLA 'A' matrix
	  FieldConfig = FieldConfig,
	  RhoConfig = RhoConfig,
	  ObsModel = c(2, 0), # conventional logit-linked delta-Gamma; c(10, 2) for Tweedie
	  bias.correct = FALSE,
	  use_anisotropy = TRUE,
	  max_cells = Inf, # use all grid cells from the extrapolation grid
	  knot_method = "samples" # "samples" or "grid"
	)

	fit <- VAST::fit_model(
	  settings = settings,
	  Lat_i = subdata[, "Lat"],
	  Lon_i = subdata[, "Lon"],
	  t_i = subdata[, "Year"],
	  b_i = subdata[,'Catch_mt'], 
	  a_i = subdata[, "AreaSwept_km2"], 
	  v_i = subdata[, "vessel_scaled"],
	  Q1_formula = ~ Pass,
	  Q2_formula = ~ Pass,
	  catchability_data = subdata$pass_scaled,
	  working_dir = dir,
	  input_grid = vast_mesh[['inputgrid']]
	)

	# The mesh passed here will now grab the anisotropic mesh created within vast
	# rather than the isotropic mesh
	mesh <- sdmTMB::make_mesh(
		data = subdata, 
		xy_cols = c("X","Y"), 
		mesh = fit$spatial_list$MeshList$anisotropic_mesh)

	# I am sure this is not the correct setting for the pass
	fit_sdmTMB <- sdmTMB::sdmTMB(
  		catch_mt ~ 0 + as.factor(Year) + as.factor(1|vessel_scaled) + as.factor(1|pass_scaled),
  		time = "Year",
  		offset = log(subdata$AreaSwept_km2),
  		data = subdata,
  		mesh = mesh_sdmTMB,
  		family = delta_gamma(),
  		spatial = "on",
  		spatiotemporal = "iid",
  		anisotropy = TRUE,
  		control = sdmTMBcontrol(newton_loops = 1), # match VAST default settings
  		silent = TRUE
	)
