library(VAST)
library(VASTWestCoast)
library(sdmTMB)
library(sp)
library(here)
library(dplyr)
library(ggplot2)

setwd("C:/Users/Chantel.Wetzel/Documents/GitHub/indexwc/sandbox")
here::i_am("vast_sdmTMB_comparison_example.R")
data_dir = here("Catch__NWFSC.Combo_2022-04-09.rdata")
dir.create(here(paste0("gamma_example/input_grid")))
dir = here(paste0("gamma_example/input_grid"))

# Load catch data file for dover sole in the "dover_example" folder
load("Catch__NWFSC.Combo_2022-04-09.rdata")
data <- VASTWestCoast::clean_data(data = Out)
catchability_data <- data[,"Pass", drop = FALSE] - mean(range(data[,"Pass"]))

# Set up the Field and Rho Configuration for VAST
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

# Create the mesh for VAST
survey <- VASTWestCoast::convert_survey4vast(survey = "WCGBTS")
vast_mesh <- VASTWestCoast::VAST_mesh(data = data,
  survey = survey,
  numknots = as.numeric(settings['n_x']),
  savedir = dir)

# Set the data to the updated data frame create within the VAST_mesh fxn
subdata <- mesh$mesh$data.inner
subdata[,"Catch_mt"] <- subdata[,"Catch_KG"] / 1000
subdata[,"cpue_mt_km2"] <- subdata[,"Catch_mt"] / subdata[,"AreaSwept_km2"]
units(subdata[, "Catch_mt"]) <- 't'
subdata$effort <- 1

# Run VAST with simplified model structure
fit <- fit_model(
  settings = settings,
  Lat_i = subdata[, "Lat"],
  Lon_i = subdata[, "Lon"],
  t_i = subdata[, "Year"],
  b_i = subdata[, "Catch_mt"],
  a_i = subdata[,"effort"], #subdata[, "AreaSwept_km2"],
  working_dir = dir,
  input_grid = vast_mesh[['inputgrid']]
)


# Calculate the index of abundance from VAST
index_vast <- suppressWarnings(FishStatsUtils::plot_biomass_index(
  fit = fit,
  DirName = file.path(dir, .Platform$file.sep),
  TmbData = fit$data_list,
  Sdreport = fit$parameter_estimates$SD,
  use_biascorr = TRUE,
  Year_Set = fit$year_labels,
  Years2Include = fit$years_to_plot,
  strata_names = fit$settings$strata.limits$STRATA
))

save(fit, index_vast, vast_mesh, settings,
  file = file.path(dir, "vast_save.RData"))

save(data, subdata,
  file = file.path(dir, "data.RData"))

########################################################################
# sdmTMB
########################################################################

# create mesh for sdmTMB from mesh used by VAST
mesh_sdmTMB <- sdmTMB::make_mesh(
  data = subdata,
  xy_cols = c("X", "Y"),
  mesh = fit$spatial_list$MeshList$isotropic_mesh
)
plot(mesh_sdmTMB)

# Run simplified model structure for sdmTMB
subdata$catch_mt <- drop_units(subdata$Catch_mt)
tictoc::tic()
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
tictoc::toc()

print(fit_sdmTMB)

# sanity(fit_sdmTMB) # experimental... function name may change

# Create the same grid used by vast for sdmTMB
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

year_grid <- NULL
for(y in c(2003:2019, 2021)){
  vast_grid$Year <- y
  year_grid <- rbind(year_grid, data.frame(vast_grid[, c("X", "Y", "Year", "Area_km2")]))
}

# Create predictions
pred <- predict(
  fit_sdmTMB,
  newdata = year_grid, # the grid in question
  return_tmb_object = TRUE 
)

# Create index with bias correction
tictoc::tic()
index_sdmTMB <- sdmTMB::get_index(pred, 
  bias_correct = FALSE,
  area = year_grid$Area_km2)
tictoc::toc()

save(mesh_sdmTMB, fit_sdmTMB, year_grid, pred, index_sdmTMB,
  file = file.path(dir, "sdmTMB_save.RData")
)

###########################################################################
# Bind and plot indices
###########################################################################

source(here("bind_indices.R"))
source(here("plot_indices.R"))

both_i <- bind_indices(dir = dir, sdmtmb_index = index_sdmTMB)
save(both_i, file = file.path(dir, "both_indices.Rdata"))
plot_indices(data = both_i, 
  main_name = "Dover sole - Year Model", 
  save_loc = dir, 
  ymax = NULL) 

###########################################################################
# Compare parameter estimates between VAST and sdmTMB
###########################################################################
source(here("plot_betas.R"))

out_file = file.path(dir, "parameter_comparison.png")
grDevices::png(filename = out_file,
  width = 10, height = 7, units = "in", res = 300, pointsize = 12)
par(mfrow = c(2, 1), cex = 0.8, mar = c(1.5, 1, 1, 1), oma = c(2, 3, 1, 1))
years <- sort(unique(subdata$Year))
plot_betas(fit, fit_sdmTMB, "beta1_ft", sdmTMB_pars = 1, years = years)
plot_betas(fit, fit_sdmTMB, "beta2_ft", sdmTMB_pars = 2, years = years)
dev.off()

# fit$parameter_estimates$SD
s1 <- fit$ParHat$beta1_ft[fit$ParHat$beta1_ft != 0]
s2 <- fit$ParHat$beta2_ft[fit$ParHat$beta2_ft != 0]

b1 <- tidy(fit_sdmTMB, model = 1)
b2 <- tidy(fit_sdmTMB, model = 2)
# b_only_sdmTMB <- b_sdmTMB$estimate[b_sdmTMB$term != "as.factor(Pass)2"]

out_file = file.path(dir, "s1_b1.png")
grDevices::png(filename = out_file,
  width = 10, height = 7, units = "in", res = 300, pointsize = 12)
par(mfrow = c(2, 1), cex = 0.8, mar = c(1.5, 1, 1, 1), oma = c(2, 3, 1, 1))
plot(s1, b1$estimate);abline(0, 1)
plot(s2, b2$estimate);abline(0, 1)
dev.off()

s1 - b1$estimate
s2 - b2$estimate

s <- fit$parameter_estimates$SD
vast_est1 <- as.list(s, "Estimate", report = FALSE)
vast_est2 <- as.list(s, "Estimate", report = TRUE)
vast_sd1 <- as.list(s, "Std. Error", report = FALSE)
vast_sd2 <- as.list(s, "Std. Error", report = TRUE)
sdmtmb_est1 <- tidy(fit_sdmTMB, "ran_pars", model = 1)
sdmtmb_est2 <- tidy(fit_sdmTMB, "ran_pars", model = 2)

# range
sdmtmb_est1$estimate[sdmtmb_est1$term == "range"]
vast_est2$Range_raw1

sdmtmb_est2$estimate[sdmtmb_est2$term == "range"]
vast_est2$Range_raw2

# sigma_O
sdmtmb_est1$estimate[sdmtmb_est1$term == "sigma_O"]
vast_est1$L_omega1_z

sdmtmb_est2$estimate[sdmtmb_est2$term == "sigma_O"]
vast_est1$L_omega2_z

# sigma_E
sdmtmb_est1$estimate[sdmtmb_est1$term == "sigma_E"]
vast_est1$L_epsilon1_z

sdmtmb_est2$estimate[sdmtmb_est2$term == "sigma_E"]
vast_est1$L_epsilon2_z

# Gamma shape parameter:
sdmtmb_est2$estimate[sdmtmb_est2$term == "phi"]

# VAST: shape = 1/CV^2
1/exp(vast_est1$logSigmaM[1,1])^2

