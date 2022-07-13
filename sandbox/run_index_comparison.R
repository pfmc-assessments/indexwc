# Load libraries
library(VAST)
library(VASTWestCoast)
library(sdmTMB)
library(here)
library(dplyr)
library(ggplot2)
TMB::openmp(n = 1L) # because VAST may be picking this up now with TMBad?
options(sdmTMB.cores = 1L)
# make sure VAST compiled models are trashed if previously compiled without
# this option:
# See `.Library` VAST/executables
options(tmb.ad.framework = "TMBad")

setwd("C:/Users/Chantel.Wetzel/Documents/GitHub/indexwc/sandbox")
source("do_vast_settings.R")
source("format_data.R")
source("utils.R")
source("plot_indices.R")
source("my_apply_epsilon.R")

sp = c("arrowtooth_flounder",
       "aurora_rockfish",
       "bocaccio",
       "big_skate",
       "dover_sole",
       "sablefish",
       "shortspine_thornyhead",
       "widow_rockfish")

anis = TRUE
bias_correct = TRUE
re = FALSE
dist = c("lognormal","gamma","tweedie")[2]

for (sp in species) {

rm(index_vast, both, index_sdmTMB)

formula = list(
    catch_mt ~ 0 + as.factor(Year) + pass_scaled + (1 | vessel_scaled),
    catch_mt ~ 0 + as.factor(Year) + pass_scaled + (1 | vessel_scaled))
eta1 = 1; eta2 = 1

if (re == FALSE) {
  formula = list(
    catch_mt ~ 0 + as.factor(Year) + pass_scaled,
    catch_mt ~ 0 + as.factor(Year) + pass_scaled)
  eta1 = 0; eta2 = 0
}

obs_model = c(2,0)
family = delta_gamma()
FieldConfig <- matrix(c("IID", "IID", "IID", "IID", "IID", "IID"),
  ncol = 2, nrow = 3,
  dimnames = list(
    c("Omega", "Epsilon", "Beta"),
    c("Component_1", "Component_2")
  )
)
RhoConfig <- c("Beta1" = 0, "Beta2" = 0, "Epsilon1" = 0, "Epsilon2" = 0)

if (dist == "lognormal"){
  obs_model =  c(1,0)
  family = delta_lognormal() }
if (dist == "tweedie"){
  obs_model = c(10, 2)
  family = tweedie()
  FieldConfig <- matrix(c("0", "0", "IID", "Identity", "IID", "IID", "IID", "Identity"),
    ncol = 2, nrow = 4,
    dimnames = list(
    c("Omega", "Epsilon", "Beta", "Epsilon_year"),
    c("Component_1", "Component_2")
    )
    )
  RhoConfig <- c("Beta1" = 3, "Beta2" = 0, "Epsilon1" = 0, "Epsilon2" = 0)
}

dir <- file.path(getwd(), "comparisons", sp)
if (anis){
	dir <- file.path(getwd(), paste0("anisotropy_", dist), sp) }
if (bias_correct){
	dir <- file.path(getwd(), paste0("bias_correct_", dist), sp)  }
if (bias_correct & anis){
	dir <- file.path(getwd(), paste0("anisotropy_bias_correct_", dist), sp) }
if (re){
  dir = paste0(dir, "_re=TRUE")
} else {
  dir = paste0(dir, "_re=FALSE")
}

dir.create(dir, showWarnings = FALSE)

# Load catch data file
load(file.path(getwd(), "data", paste0(sp, "_catch_data.rdata")))
data <- format_data(Out = Out)

# Set up the Field and Rho Configuration for VAST
settings <- do_vast_settings(
  knots = 250,
  obs_model = obs_model,
  eta1 = eta1,
  eta2 = eta2,
	anis = anis,
  RhoConfig = RhoConfig,
  FieldConfig = FieldConfig,
  bias = FALSE, # will be done with apply_epsilon() below
  Version = "VAST_v14_0_1"
)

# Create the mesh for VAST
survey <- VASTWestCoast::convert_survey4vast(survey = "WCGBTS")
vast_mesh <- VASTWestCoast::VAST_mesh(data = data,
  survey = survey,
  numknots = as.numeric(settings['n_x']),
  savedir = paste0(dir, "/"))

# Set the data to the updated data frame create within the VAST_mesh fxn
# this adds the X and Y coordinates
subdata <- vast_mesh$mesh$data.inner

tictoc::tic()
fit <- fit_model(
  settings = settings,
  Lat_i = subdata[, "Lat"],
  Lon_i = subdata[, "Lon"],
  t_i = subdata[, "Year"],
  b_i = subdata[,'Catch_mt'],
  a_i = subdata[, "AreaSwept_km2"],
  v_i = subdata[, "vessel_scaled"],
  Q1_formula = ~ pass_scaled,
  Q2_formula = ~ pass_scaled,
  working_dir = paste0(dir, "/"),
  catchability_data = subdata,
  input_grid = vast_mesh[['inputgrid']]
)
vast_time <- tictoc::toc()

tictoc::tic()
if (bias_correct) {
  #sdv <- VAST::apply_epsilon(fit, data_function = strip_units)
  sdv <- my_apply_epsilon(fit, data_function = strip_units)
  index_vast <- extract_vast_index(x = fit)
  unbiased_vast <- sdv$unbiased$value[names(sdv$unbiased$value) == "Index_ctl"]
  suppressWarnings({ # just to figure out which > 0:
    vi <- FishStatsUtils::plot_biomass_index(fit,
      DirName = tempdir())
  })
  est <- vi$Table$Estimate
  lwr <- exp(log(unbiased_vast) + qnorm(0.025) * vi$Table$`Std. Error for ln(Estimate)`)
  upr <- exp(log(unbiased_vast) + qnorm(0.975) * vi$Table$`Std. Error for ln(Estimate)`)
  index_vast$est <- unbiased_vast[est > 0]
  index_vast$lwr <- lwr[est > 0]
  index_vast$upr <- upr[est > 0]
} else {
  index_vast <- extract_vast_index(x = fit)
}
vast_index_time <- tictoc::toc()

########################################################################
# Save VAST and data objects
########################################################################

save(fit, index_vast, vast_mesh, settings,
  file = file.path(dir, "vast_save.RData"))

save(data, subdata,
  file = file.path(dir, "data.RData"))

###########################################################################
# Create the same grid used by vast for sdmTMB
###########################################################################

survey <- VASTWestCoast::convert_survey4vast(survey = "WCGBTS")
grid <- VASTWestCoast::get_inputgrid(survey = survey)
vast_grid <- grid
vast_grid <- vast_grid[vast_grid$Area_km2 > 0, ]
vast_grid <- sdmTMB::add_utm_columns(vast_grid, c("Lon", "Lat"), utm_crs = 32610) # UTM 10
vast_grid$pass_scaled <- 0
vast_grid$vessel_scaled <- subdata$vessel_scaled[1]
vast_grid <- dplyr::select(vast_grid, X, Y, Area_km2, pass_scaled, vessel_scaled)

year_grid <- purrr::map_dfr(c(2003:2019, 2021), function(yr) {
  vast_grid$Year <- yr
  vast_grid
})

########################################################################
# Run sdmTMB
########################################################################

if (anis){
	mesh <- fit$spatial_list$MeshList$anisotropic_mesh
} else {
	mesh <- fit$spatial_list$MeshList$isotropic_mesh
}

# create mesh for sdmTMB from mesh used by VAST
mesh_sdmTMB <- sdmTMB::make_mesh(
  data = subdata,
  xy_cols = c("X", "Y"),
  mesh = mesh
)

# Modify the suddata data frame to run with sdmTMB
subdata$catch_mt <- drop_units(subdata$Catch_mt)
subdata$vessel_scaled <- as.factor(subdata$vessel_scaled)

tictoc::tic()
fit_sdmTMB <- sdmTMB(
  formula = formula,
  time = "Year",
  offset = log(subdata$AreaSwept_km2),
  data = subdata,
  mesh = mesh_sdmTMB,
  family = family,
  spatial = "on",
  spatiotemporal = list("iid", "iid"),
  anisotropy = anis,
  silent = TRUE,
  control = sdmTMBcontrol(
    newton_loops = 1L,
    map = list(ln_H_input = factor(c(1, 2, 1, 2))) # <- force sdmTMB to share anisotropy parameters across the two delta models
  ),
  do_index = TRUE,
  predict_args = list(newdata = year_grid, re_form_iid = NA),
  index_args = list(area = year_grid$Area_km2)
)
sdmTMB_time = tictoc::toc()

tictoc::tic()
index_sdmTMB <- sdmTMB::get_index(
  fit_sdmTMB, # skipping prediction step
  bias_correct = bias_correct
)
sdmTMB_index_time = tictoc::toc()

# print(fit_sdmTMB)
# tidy(fit_sdmTMB, model = 1)
# tidy(fit_sdmTMB, model = 2)
# sanity(fit_sdmTMB) 

###########################################################################
# Compare parameter estimates between VAST and sdmTMB
###########################################################################

out_file = file.path(dir, "parameter_comparison.png")
grDevices::png(filename = out_file,
  width = 10, height = 7, units = "in", res = 300, pointsize = 12)
par(mfrow = c(2, 1), cex = 0.8, mar = c(4, 4, 2, 2), oma = c(2, 3, 1, 1))
plot_betas_delta(vast_model = fit, sdmTMB_model = fit_sdmTMB, "beta1_ft", sdmTMB_pars = 1)
plot_betas_delta(vast_model = fit, sdmTMB_model = fit_sdmTMB, "beta2_ft", sdmTMB_pars = 2)
dev.off()

s1 <- fit$ParHat$beta1_ft[fit$ParHat$beta1_ft != 0]
s2 <- fit$ParHat$beta2_ft[fit$ParHat$beta2_ft != 0]

b1 <- tidy(fit_sdmTMB, model = 1)
b2 <- tidy(fit_sdmTMB, model = 2)

yr_i <- grep("year", b1$term, ignore.case = TRUE) # find year coefs

out_file = file.path(dir, "s1_b1.png")
grDevices::png(filename = out_file,
  width = 10, height = 7, units = "in", res = 300, pointsize = 12)
par(mfrow = c(2, 1), cex = 0.8, mar = c(1.5, 1, 1, 1), oma = c(2, 3, 1, 1))
plot(s1, b1$estimate[yr_i], xlab = "VAST s1", ylab = "sdmTMB b1");abline(0, 1)
plot(s2, b2$estimate[yr_i], xlab = "VAST s2", ylab = "sdmTMB b2");abline(0, 1)
dev.off()

# Tweedie Comparison
#s1 <- fit$ParHat$beta1_ft[fit$ParHat$beta2_ft != 0]
#s2 <- fit$ParHat$beta2_ft[fit$ParHat$beta2_ft != 0]
#b_VAST <- as.numeric(s1 + s2)
#b_sdmTMB <- tidy(fit_sdmTMB)
#
#out_file = file.path(dir, "s1_b1.png")
#grDevices::png(filename = out_file,
#  width = 10, height = 7, units = "in", res = 300, pointsize = 12)
#plot(b_VAST, b_sdmTMB[1:18,"estimate"], xlab = "VAST", ylab = "sdmTMB")
#abline(0, 1)
#dev.off()

###########################################################################
# Bind and plot indices
###########################################################################

index_sdmTMB$index <- "sdmTMB"
index_sdmTMB <- rename(index_sdmTMB, year = Year)
both <- bind_rows(index_sdmTMB, index_vast)
save(both, file = file.path(dir, "both_indices.Rdata"))

out_file = file.path(dir, "indices_ggplot.png")
grDevices::png(filename = out_file,
  width = 10, height = 7, units = "in", res = 300, pointsize = 12)
g <- ggplot(both, aes(x = year, y = est, ymin = lwr, ymax = upr, colour = index, fill = index)) +
  geom_ribbon(alpha = 0.1, position = position_dodge(width = 0.00), lty = 3) +
  geom_line(alpha = 0.8, position = position_dodge(width = 0.00), lwd = 1) +
  coord_cartesian(expand = FALSE) +
  theme_bw() +
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  ylab("Relative biomass estimate") + xlab("Year") + labs(title = sp) +
  labs(colour = "Package", fill = "Package")
print(g)
dev.off()

plot_indices(data = both,
  main_name = sp,
  save_loc = dir,
  ymax = NULL)

save(mesh_sdmTMB, fit_sdmTMB, year_grid, index_sdmTMB,
  file = file.path(dir, "sdmTMB_save.RData")
)

save(vast_time, vast_index_time, sdmTMB_time, sdmTMB_index_time,
  file = file.path(dir, "run_time.Rdata"))

} # end species loop
