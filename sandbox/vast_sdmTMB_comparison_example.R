library(VAST)
library(VASTWestCoast)
library(sdmTMB)
library(here)

here::i_am("vast_sdmTMB_comparison_example.R")
species = "dover_example"
dir.create(here(species))
setwd(here(species))

# Load catch data file for dover sole in the "dover_example" folder
load("Catch__NWFSC.Combo_2022-04-09.rdata")
data <- VASTWestCoast::clean_data(data = Out)
catchability_data <- data[,"Pass", drop = FALSE] - mean(range(data[,"Pass"]))

# Set up the Field and Rho Configuration for VAST
FieldConfig <- matrix(c("0", "0", "IID", "Identity", "IID", "IID", "IID", "Identity"),
	ncol = 2, nrow = 4,
	dimnames = list(
	c("Omega", "Epsilon", "Beta", "Epsilon_year"),
	c("Component_1", "Component_2")
	)
	)

RhoConfig <- c("Beta1" = 3, "Beta2" = 0, "Epsilon1" = 0, "Epsilon2" = 0)

settings <- FishStatsUtils::make_settings(
	n_x = 500, # number of vertices in the SPDE mesh
	Region = "User",
	purpose = "index2", # use recommended defaults for an index of abundance
	fine_scale = TRUE, # use bilinear interpolation from the INLA 'A' matrix
	FieldConfig = FieldConfig,
	RhoConfig = RhoConfig,
	ObsModel = c(10, 2), # use the Tweedie distribution as the observation model
	bias.correct = FALSE,
	use_anisotropy = FALSE,
	max_cells = Inf, # use all grid cells from the extrapolation grid
	knot_method = "samples" # "samples" or "grid"
	)

# Create the mesh for VAST
survey <- VASTWestCoast::convert_survey4vast(survey = "WCGBTS")
mesh <- VASTWestCoast::VAST_mesh(data = data,
		survey = survey,
		numknots = as.numeric(settings['n_x']))

# Set the data to the updated data frame create within the VAST_mesh fxn
subdata <- mesh$mesh$data.inner

# Run VAST with simpliefied model structure
fit <- fit_model(
	settings = settings,
	Lat_i = subdata[, "Lat"],
	Lon_i = subdata[, "Lon"],
	t_i = subdata[, "Year"],
	b_i = subdata[, "Catch_KG"],
	a_i = subdata[, "AreaSwept_km2"],
	#v_i = as.numeric(subdata[, "Vessel"], as.is = FALSE) - 1,
	working_dir = getwd(),
	#Q1_formula = ~ Pass,
	#Q2_formulat = ~ Pass,
	catchability_data = catchability_data,
	extrapolation_args = c(
		settings['zone'],
		settings['Region'],
		settings['strata.limits'],
		suveyname = survey,
		input_grid = list(mesh[['inputgrid']]))
	)

# Calculate the index of abundance from VAST
index_vast <- suppressWarnings(FishStatsUtils::plot_biomass_index(
  fit = fit,
  DirName = file.path(getwd(), .Platform$file.sep),
  TmbData = fit$data_list,
  Sdreport = fit$parameter_estimates$SD,
  use_biascorr = TRUE,
  Year_Set = fit$year_labels,
  Years2Include = fit$years_to_plot,
  strata_names = fit$settings$strata.limits$STRATA
))

# Create the Table_for_SS3 csv from the Index.csv with needed corrections
# for our indices
fileindex <- file.path(getwd(), "Table_for_SS3.csv")
indexdata <- data.frame(
  Year = fit$year_labels[index_vast[["Table"]][, "Time"]],
  Unit = "mt", #index_vast[["Table"]][, "Units"],
  Fleet = index_vast[["Table"]][, "Stratum"],
  # Go from kg to mt to keep backwards compatibility with VASTWestCoast
  Estimate_metric_tons = index_vast[["Table"]][, "Estimate"] / 1000,
  SD_log = index_vast[["Table"]][, "Std. Error for ln(Estimate)"],
  SD_mt = index_vast[["Table"]][, "Std. Error for Estimate"] / 1000
)
utils::write.csv(x = indexdata, file = fileindex, row.names = FALSE)

save(fit, index_vast, mesh, settings,
	file = file.path(getwd(), "vast_save.RData"))

########################################################################
# sdmTMB
########################################################################

# create mesh for sdmTMB from mesh used by VAST
mesh <- sdmTMB::make_mesh(
	data = subdata,
	xy_cols = c("X","Y"),
	mesh = fit$spatial_list$MeshList$isotropic_mesh)
plot(mesh)

# Run simplified model structure for sdmTMB
fit_sdmTMB <- sdmTMB::sdmTMB(
  Catch_KG ~ 0 + as.factor(Year), # + as.factor(Pass) + (1 | Vessel),
  time = "Year",
  offset = log(subdata$AreaSwept_km2),
  data = subdata,
  mesh = mesh,
  family = tweedie(link = "log"),
  spatial = "on",
  spatiotemporal = 'iid', # SA: was misspelled here; latest sdmTMB no longer has `...` in sdmTMB()
  control = sdmTMBcontrol(newton_loops = 1), # match VAST
  silent = FALSE
)

# Create predictions
pred <- predict(
		fit_sdmTMB,
		newdata = subdata,
		return_tmb_object = TRUE)

# Create index without bias correction
index_sdmTMB <- sdmTMB::get_index(pred, bias_correct = FALSE)

save(mesh, fit_sdmTMB, pred, index_sdmTMB,
	file = file.path(getwd(), "sdmTMB_save.RData"))

###########################################################################
# Compare parameter estimates between VAST and sdmTMB
###########################################################################

fit$parameter_estimates$SD
s1 <- fit$ParHat$beta1_ft[fit$ParHat$beta2_ft != 0]
s2 <- fit$ParHat$beta2_ft[fit$ParHat$beta2_ft != 0]
b_VAST <- as.numeric(s1 + s2)

b_sdmTMB <- tidy(fit_sdmTMB)
b_only_sdmTMB <- b_sdmTMB$estimate[b_sdmTMB$term != "as.factor(Pass)2"]

plot(b_only_sdmTMB, b_VAST, ylim = c(-2,0), xlim = c(-2,0)); abline(0, 1)

cor(b_sdmTMB$estimate, b_VAST)
# 0.9957409

s <- fit$parameter_estimates$SD
vast_est1 <- as.list(s, "Estimate", report = FALSE)
vast_est2 <- as.list(s, "Estimate", report = TRUE)
vast_sd1 <- as.list(s, "Std. Error", report = FALSE)
vast_sd2 <- as.list(s, "Std. Error", report = TRUE)
sdmtmb_est <- tidy(fit_sdmTMB, "ran_pars")

# range
sdmtmb_est$estimate[sdmtmb_est$term == "range"]
vast_est2$Range_raw2

# sigma_O
sdmtmb_est$estimate[sdmtmb_est$term == "sigma_O"]
vast_est1$L_omega2_z

# sigma_E
sdmtmb_est$estimate[sdmtmb_est$term == "sigma_E"]
vast_est1$L_epsilon2_z

# Tweedie p
sdmtmb_est$estimate[sdmtmb_est$term == "tweedie_p"]
# not in sdmtmb_est
1 + plogis(fit$ParHat$logSigmaM[1,1] )
# [1] 1.625061

# Tweedie dispersion
as.numeric(exp(fit$ParHat$beta1_ft))[1]
# [1] 192.2561
sdmtmb_est$estimate[sdmtmb_est$term == "phi"]
# [1] 15.1877

