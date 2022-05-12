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
mesh <- VASTWestCoast::VAST_mesh(data = data,
  survey = survey,
  numknots = as.numeric(settings['n_x']))

# Set the data to the updated data frame create within the VAST_mesh fxn
subdata <- mesh$mesh$data.inner

# Run VAST with simplified model structure

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
  xy_cols = c("X", "Y"),
  mesh = fit$spatial_list$MeshList$isotropic_mesh
)
plot(mesh)

# Run simplified model structure for sdmTMB
fit_sdmTMB <- sdmTMB::sdmTMB(
  Catch_KG ~ 0 + as.factor(Year), # + as.factor(Pass) + (1 | Vessel),
  time = "Year",
  offset = log(subdata$AreaSwept_km2),
  data = subdata,
  mesh = mesh,
  family = delta_gamma(),
  spatial = "on",
  spatiotemporal = "iid",
  control = sdmTMBcontrol(newton_loops = 1), # match VAST default settings
  silent = FALSE
)

print(fit_sdmTMB)

sanity(fit_sdmTMB) # experimental... function name may change

# Create predictions
pred <- predict(
  fit_sdmTMB,
  newdata = subdata, # SA: fix this!
  return_tmb_object = TRUE
)

# Create index with bias correction
index_sdmTMB <- sdmTMB::get_index(pred, bias_correct = FALSE)

save(mesh, fit_sdmTMB, pred, index_sdmTMB,
  file = file.path(getwd(), "sdmTMB_save.RData")
)

###########################################################################
# Compare parameter estimates between VAST and sdmTMB
###########################################################################

plot_betas <- function(vast_model, sdmTMB_model, years, vast_par = "beta1_ft", sdmTMB_pars = 1) {
  s <- vast_model$parameter_estimates$SD
  vast_est1 <- as.list(s, "Estimate", report = FALSE)
  vast_est2 <- as.list(s, "Estimate", report = TRUE)
  vast_sd1 <- as.list(s, "Std. Error", report = FALSE)
  vast_sd2 <- as.list(s, "Std. Error", report = TRUE)
  sdmTMB_est <- as.list(sdmTMB_model$sd_report, "Estimate", report = FALSE)
  sdmTMB_sd <- as.list(sdmTMB_model$sd_report, "Std. Error", report = FALSE)
  b_year_vast <- vast_est1[[vast_par]][!is.na(vast_sd1[[vast_par]])]
  b_year_vast_se <- vast_sd1[[vast_par]][!is.na(vast_sd1[[vast_par]])]
  lwr_vast <- b_year_vast - 2 * b_year_vast_se
  upr_vast <- b_year_vast + 2 * b_year_vast_se
  plot(years, b_year_vast, ylim = range(c(lwr_vast, upr_vast)))
  segments(years, lwr_vast, years, upr_vast)
  years <- years + 0.05
  if (sdmTMB_pars == 1) {
    points(years, sdmTMB_est$b_j)
    segments(years, sdmTMB_est$b_j - 2 * sdmTMB_sd$b_j,
      years, sdmTMB_est$b_j + 2 * sdmTMB_sd$b_j,
      col = "red")
  } else {
    points(years, sdmTMB_est$b_j2)
    segments(years, sdmTMB_est$b_j2 - 2 * sdmTMB_sd$b_j2,
      years, sdmTMB_est$b_j2 + 2 * sdmTMB_sd$b_j2,
      col = "red")
  }
  legend("topright", legend = c("VAST", "sdmTMB"),
    col = c("black", "red"), bty = "n", lty = c(1, 1))
}

par(mfrow = c(2, 1), cex = 0.8, mar = c(1.5, 1, 1, 1), oma = c(2, 3, 1, 1))
years <- sort(unique(subdata$Year))
plot_betas(fit, fit_sdmTMB, "beta1_ft", sdmTMB_pars = 1, years = years)
plot_betas(fit, fit_sdmTMB, "beta2_ft", sdmTMB_pars = 2, years = years)

# fit$parameter_estimates$SD
s1 <- fit$ParHat$beta1_ft[fit$ParHat$beta1_ft != 0]
s2 <- fit$ParHat$beta2_ft[fit$ParHat$beta2_ft != 0]

b1 <- tidy(fit_sdmTMB, model = 1)
b2 <- tidy(fit_sdmTMB, model = 2)
# b_only_sdmTMB <- b_sdmTMB$estimate[b_sdmTMB$term != "as.factor(Pass)2"]

plot(s1, b1$estimate);abline(0, 1)
plot(s2, b2$estimate);abline(0, 1)
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
