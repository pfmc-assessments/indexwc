
library(VAST)
library(VASTWestCoast)
#devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/VASTWestCoast")
library(sdmTMB)
#devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/sdmTMB")
library(here)
library(dplyr)
library(ggplot2)
TMB::openmp(n = 4L) # because VAST may be picking this up now with TMBad?
options(sdmTMB.cores = 4L)

run <- "catch_mt~year_with_areaswept_in_offset"
run <- "catch_mt~year_with_no_areaswept_in_offset"
run <- "catch_mt~year_with_areaswept(vast)_offset_effort(sdmtmb)"

setwd("C:/Users/Chantel.Wetzel/Documents/GitHub/indexwc/sandbox")
here::i_am("vast_sdmTMB_comparison_example.R")
source("utils.R")
data_dir = here("Catch__NWFSC.Combo_2022-04-09.rdata")
dir.create(here(paste0("gamma_example/", run)), showWarnings = FALSE)
dir.create(here(paste0("gamma_example/", run, "/dover")), showWarnings = FALSE)
dir = here(paste0("gamma_example/", run, "/dover"))
file.copy(from = here(paste0("gamma_example/doverKmeans-knots-300.RData")),
  to = here(paste0("gamma_example/", run, "/doverKmeans-knots-300.RData")))

source(here("do_vast_settings.R"))
source(here("format_data.R"))

# Load catch data file for dover sole in the "dover_example" folder
load("Catch__NWFSC.Combo_2022-04-09.rdata")
data <- format_data(Out = Out)

# Set up the Field and Rho Configuration for VAST
settings <- do_vast_settings(knots = 150, obs_model = c(2, 0))

# Create the mesh for VAST
survey <- VASTWestCoast::convert_survey4vast(survey = "WCGBTS")
vast_mesh <- VASTWestCoast::VAST_mesh(data = data,
  survey = survey,
  numknots = as.numeric(settings['n_x']),
  savedir = dir)

# Set the data to the updated data frame create within the VAST_mesh fxn
# this adds the X and Y coordinates
subdata <- vast_mesh$mesh$data.inner

# Run VAST with simplified model structure
fit <- fit_model(
  settings = settings,
  Lat_i = subdata[, "Lat"],
  Lon_i = subdata[, "Lon"],
  t_i = subdata[, "Year"],
  b_i = subdata[,'Catch_mt'], #subdata[,'cpue_kg_km2'],
  a_i = subdata[, "AreaSwept_km2"], #subdata[,"effort"]
  v_i = subdata[, "vessel_scaled"],
  Q1_formula = ~ pass_scaled,
  Q2_formula = ~ pass_scaled,
  working_dir = dir,
  catchability_data = subdata,
  #extrapolation_args = c(
  #  settings['zone'],
  #  settings['Region'],
  #  settings['strata.limits'],
  #  suveyname = survey,
  #input_grid = list(mesh[['inputgrid']]))
  input_grid = vast_mesh[['inputgrid']]
)

# Calculate the index of abundance from VAST
# index_vast <- suppressWarnings(FishStatsUtils::plot_biomass_index(
#   fit = fit,
#   DirName = file.path(dir, .Platform$file.sep),
#   TmbData = fit$data_list,
#   Sdreport = fit$parameter_estimates$SD,
#   use_biascorr = FALSE,
#   year_labels = as.numeric(fit$year_labels),
#   years_to_plot = fit$years_to_plot,
#   strata_names = fit$settings$strata.limits$STRATA
# ))
index_vast <- extract_vast_index(fit)

########################################################################
# Save VAST and data objects
########################################################################

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
#plot(mesh_sdmTMB)

# Run simplified model structure for sdmTMB
subdata$catch_mt <- drop_units(subdata$Catch_mt)
subdata$vessel_scaled <- as.factor(subdata$vessel_scaled)
# subdata$pass_scaled <- as.factor(subdata$pass_scaled)

fit_sdmTMB <- sdmTMB(
  catch_mt ~ 0 + as.factor(Year) + pass_scaled + (1 | vessel_scaled),
  time = "Year",
  offset = log(subdata$AreaSwept_km2),
  data = subdata,
  mesh = mesh_sdmTMB,
  family = delta_gamma(),
  spatial = "on",
  spatiotemporal = "iid",
  silent = FALSE,
  control = sdmTMBcontrol(newton_loops = 1L)
)

print(fit_sdmTMB)
# tidy(fit_sdmTMB, model = 1)
# tidy(fit_sdmTMB, model = 2)
sanity(fit_sdmTMB) # experimental... function name may change

###########################################################################
# Compare parameter estimates between VAST and sdmTMB
###########################################################################
# source(here("plot_betas.R"))

out_file = file.path(dir, "parameter_comparison.png")
grDevices::png(filename = out_file,
  width = 10, height = 7, units = "in", res = 300, pointsize = 12)
par(mfrow = c(2, 1), cex = 0.8, mar = c(1.5, 1, 1, 1), oma = c(2, 3, 1, 1))
plot_betas_delta(vast_model = fit, sdmTMB_model = fit_sdmTMB, "beta1_ft", sdmTMB_pars = 1)
plot_betas_delta(vast_model = fit, sdmTMB_model = fit_sdmTMB, "beta2_ft", sdmTMB_pars = 2)
dev.off()

# fit$parameter_estimates$SD
s1 <- fit$ParHat$beta1_ft[fit$ParHat$beta1_ft != 0]
s2 <- fit$ParHat$beta2_ft[fit$ParHat$beta2_ft != 0]

b1 <- tidy(fit_sdmTMB, model = 1)
b2 <- tidy(fit_sdmTMB, model = 2)
#b1 <- b1[b1$term != "as.factor(Pass)2", ]
#b2 <- b2[b2$term != "as.factor(Pass)2", ]
# b_only_sdmTMB <- b_sdmTMB$estimate[b_sdmTMB$term != "as.factor(Pass)2"]

yr_i <- grep("year", b1$term, ignore.case = TRUE) # find year coefs

out_file = file.path(dir, "s1_b1.png")
grDevices::png(filename = out_file,
  width = 10, height = 7, units = "in", res = 300, pointsize = 12)
par(mfrow = c(2, 1), cex = 0.8, mar = c(1.5, 1, 1, 1), oma = c(2, 3, 1, 1))
plot(s1, b1$estimate[yr_i]);abline(0, 1)
plot(s2, b2$estimate[yr_i]);abline(0, 1)
dev.off()

s1 - b1$estimate[yr_i]
s2 - b2$estimate[yr_i]

###########################################################################
# Create the same grid used by vast for sdmTMB
###########################################################################

survey <- VASTWestCoast::convert_survey4vast(survey = "WCGBTS")
grid <- VASTWestCoast::get_inputgrid(survey = survey)
vast_grid <- grid
vast_grid <- vast_grid[vast_grid$Area_km2 > 0, ]
# get_crs(vast_grid, c("Lon", "Lat"))
vast_grid <- sdmTMB::add_utm_columns(vast_grid, c("Lon", "Lat"), utm_crs = 32610) # UTM 10

vast_grid$pass_scaled <- 0
vast_grid <- dplyr::select(vast_grid, X, Y, Area_km2, pass_scaled)

# Create a grid for each year with all factors included
# year_grid <- NULL
# for(y in c(2003:2019, 2021)){
#   vast_grid$Year <- y
#   year_grid <- rbind(year_grid,
#     data.frame(vast_grid[, c("X", "Y", "Year", "Area_km2", "pass_scaled")]))
# }

# Much faster:
year_grid <- purrr::map_dfr(c(2003:2019, 2021), function(yr) {
  vast_grid$Year <- yr
  vast_grid
})

###########################################################################
# Create predictions and index using grid
###########################################################################

# fake... I guess the formula parsing currently requires all predictors, but not used
year_grid$vessel_scaled <- subdata$vessel_scaled[1]

fit_sdmTMB$tmb_data$simulate_t <- rep(0L, length(unique(subdata$Year)))
pred <- predict(
  fit_sdmTMB,
  re_form_iid = NA,
  newdata = year_grid,
  return_tmb_object = TRUE
)

filter(pred$data, Year == 2021) %>%
  ggplot(aes(X, Y, colour = plogis(est1))) + geom_point() +
  scale_colour_viridis_c()

filter(pred$data, Year == 2021) %>%
  ggplot(aes(X, Y, colour = exp(est2))) + geom_point() +
  scale_colour_viridis_c(trans = "log10")

p <- pred$data
ind <- group_by(p, Year) %>%
  summarise(total = sum(plogis(est1) * exp(est2) * Area_km2))

# r <- pred$obj$report(pred$obj$env$last.par.best)
# pe <- r$proj_eta
# # head(plogis(pe[,1]) * exp(pe[,2]))
# plot(ind$total, index_vast$est);abline(0, 1)

# Create index:
tictoc::tic()
index_sdmTMB <- sdmTMB::get_index(
  pred,
  bias_correct = FALSE,
  area = year_grid$Area_km2
)
tictoc::toc()

save(mesh_sdmTMB, fit_sdmTMB, year_grid, pred, index_sdmTMB,
  file = file.path(dir, "sdmTMB_save.RData")
)

###########################################################################
# Bind and plot indices
###########################################################################

# source(here("bind_indices.R"))
# source(here("plot_indices.R"))
#
# both_i <- bind_indices(dir = dir, sdmtmb_index = index_sdmTMB)
#
# save(both_i, file = file.path(dir, "both_indices.Rdata"))
# plot_indices(data = both_i,
#   main_name = "Dover sole - Year (metric tons)",
#   save_loc = dir,
#   ymax = NULL)

index_sdmTMB$index <- "sdmTMB"
index_sdmTMB <- rename(index_sdmTMB, year = Year)
both <- bind_rows(index_sdmTMB, index_vast)

g <- ggplot(both, aes(x = year, y = est, ymin = lwr, ymax = upr, colour = index, fill = index)) +
  geom_ribbon(alpha = 0.1, position = position_dodge(width = 0.00), lty = 3) +
  geom_line(alpha = 0.8, position = position_dodge(width = 0.00), lwd = 1) +
  # ylim(0, max(both$upr)) +
  coord_cartesian(expand = FALSE) +
  theme_bw() +
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  ylab("Relative biomass estimate") + xlab("Year") +
  labs(colour = "Package", fill = "Package")
print(g)


###########################################################################
# More Parameter Comparisons
###########################################################################

# s <- fit$parameter_estimates$SD
# vast_est1 <- as.list(s, "Estimate", report = FALSE)
# vast_est2 <- as.list(s, "Estimate", report = TRUE)
# vast_sd1 <- as.list(s, "Std. Error", report = FALSE)
# vast_sd2 <- as.list(s, "Std. Error", report = TRUE)
# sdmtmb_est1 <- tidy(fit_sdmTMB, "ran_pars", model = 1)
# sdmtmb_est2 <- tidy(fit_sdmTMB, "ran_pars", model = 2)
#
## range
# sdmtmb_est1$estimate[sdmtmb_est1$term == "range"]
# vast_est2$Range_raw1
#
# sdmtmb_est2$estimate[sdmtmb_est2$term == "range"]
# vast_est2$Range_raw2

## sigma_O
# sdmtmb_est1$estimate[sdmtmb_est1$term == "sigma_O"]
# vast_est1$L_omega1_z
#
# sdmtmb_est2$estimate[sdmtmb_est2$term == "sigma_O"]
# vast_est1$L_omega2_z

## sigma_E
#sdmtmb_est1$estimate[sdmtmb_est1$term == "sigma_E"]
#vast_est1$L_epsilon1_z
#
#sdmtmb_est2$estimate[sdmtmb_est2$term == "sigma_E"]
#vast_est1$L_epsilon2_z
#
## Gamma shape parameter:
#sdmtmb_est2$estimate[sdmtmb_est2$term == "phi"]
#
## VAST: shape = 1/CV^2
#1/exp(vast_est1$logSigmaM[1,1])^2

