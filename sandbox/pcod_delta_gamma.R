library(TMB)
library(VAST)
library(sdmTMB)
library(here)
library(dplyr)
library(ggplot2)

# parallel TMB:
TMB::openmp(n = 4L) # because VAST may be picking this up now with TMBad?
options(sdmTMB.cores = 4L) # only set up to work on Unix machines for now...

# setwd("C:/Users/Chantel.Wetzel/Documents/GitHub/indexwc")
here::i_am("sandbox/pcod_delta_gamma.R")
dir.create(here("doc", "appendix-VAST"), recursive = TRUE, showWarnings = FALSE)
source("sandbox/utils.R")

FieldConfig <- matrix(c("IID", "IID", "IID", "IID", "IID", "IID"),
  ncol = 2, nrow = 3,
  dimnames = list(
    c("Omega", "Epsilon", "Beta"),
    c("Component_1", "Component_2")
  )
)
RhoConfig <- c("Beta1" = 0, "Beta2" = 0, "Epsilon1" = 0, "Epsilon2" = 0)

# make lat-long version of grid for VAST:
qcs_grid_ll <- qcs_grid
qcs_grid_ll$Y <- qcs_grid_ll$Y * 1000
qcs_grid_ll$X <- qcs_grid_ll$X * 1000
sp::coordinates(qcs_grid_ll) <- ~ X + Y
sp::proj4string(qcs_grid_ll) <- CRS("+proj=utm +zone=9")
qcs_grid_ll <- as.data.frame(spTransform(qcs_grid_ll, CRS("+proj=longlat +datum=WGS84")))

# remove replicate locations for each year and format for VAST:
qcs_grid_ll <- subset(qcs_grid_ll, year == min(qcs_grid_ll$year))
input_grid <- cbind(Lat = qcs_grid_ll$Y, Lon = qcs_grid_ll$X, Area_km2 = 4)

settings <- make_settings(
  n_x = 185, # number of vertices in the SPDE mesh
  Region = "User",
  purpose = "index2", # index of abundance with Gamma for positive catches
  fine_scale = TRUE, # use bilinear interpolation from the INLA 'A' matrix
  zone = 9,
  FieldConfig = FieldConfig,
  RhoConfig = RhoConfig,
  ObsModel = c(2, 0), # conventional logit-linked delta-Gamma; c(10, 2) for Tweedie
  bias.correct = FALSE,
  use_anisotropy = FALSE,
  max_cells = Inf, # use all grid cells from the extrapolation grid
  knot_method = "grid" # or "samples"
)

# effort is 1 when using CPUE instead of observed weight as the response:
set.seed(1) # fake effort here:
pcod$effort <- exp(rnorm(nrow(pcod), mean = 0, sd = 0.2))
# pcod$effort <- 1
pcod <- as.data.frame(pcod) # ensure not a tibble

# f <- here("doc", "appendix-VAST", "vast-cache.rds")
# if (!file.exists(f)) {
fit_vast <- fit_model(
  settings = settings,
  Lat_i = pcod[, "lat"],
  Lon_i = pcod[, "lon"],
  t_i = pcod[, "year"],
  b_i = pcod[, "density"],
  a_i = pcod[, "effort"],
  input_grid = input_grid,
  working_dir = paste0(here("doc", "appendix-VAST"), "/")
)
#   saveRDS(fit_vast, file = f)
# } else {
#   fit_vast <- readRDS(f)
# }

# save(fit_vast, file = here("doc/appendix-VAST/vast_out.Rdata"))

fit_vast$parameter_estimates$SD

mesh_sdmTMB <- make_mesh(pcod,
  xy_cols = c("X", "Y"),
  mesh = fit_vast$spatial_list$MeshList$isotropic_mesh
)

fit_sdmTMB <- sdmTMB(
  density ~ 0 + as.factor(year),
  data = pcod,
  mesh = mesh_sdmTMB,
  offset = log(pcod$effort),
  family = delta_gamma(),
  time = "year",
  silent = FALSE,
  control = sdmTMBcontrol(newton_loops = 1L)
)
fit_sdmTMB
sanity(fit_sdmTMB)

par(mfrow = c(2, 1), cex = 0.8, mar = c(1.5, 1, 1, 1), oma = c(2, 3, 1, 1))
plot_betas_delta(fit_vast, fit_sdmTMB, "beta1_ft", sdmTMB_pars = 1)
plot_betas_delta(fit_vast, fit_sdmTMB, "beta2_ft", sdmTMB_pars = 2)

vast_i <- extract_vast_index(fit_vast)

p <- predict(fit_sdmTMB, newdata = qcs_grid, return_tmb_object = TRUE)
ind <- get_index(p, bias_correct = FALSE, area = 4)

# save(fit_sdmTMB, ind, file = here("doc/appendix-VAST/sdmTMB_out.Rdata"))

sdm_i$index <- "sdmTMB"
both_i <- bind_rows(sdm_i, vast_i)

g <- ggplot(both_i, aes(x = year, y = est, ymin = lwr, ymax = upr, colour = index, fill = index)) +
  geom_ribbon(alpha = 0.1, position = position_dodge(width = 0.04), lty = 3) +
  geom_line(alpha = 0.8, position = position_dodge(width = 0.04), lwd = 1) +
  ylim(0, max(both_i$upr)) +
  coord_cartesian(expand = FALSE) +
  theme_bw() +
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  ylab("Relative biomass estimate") + xlab("Year") +
  labs(colour = "Package", fill = "Package")
print(g)

# relative error:
(ind$est - vast_i$est) / vast_i$est
(ind$upr - vast_i$upr) / vast_i$upr
(ind$lwr - vast_i$lwr) / vast_i$lwr

# save(both_i, file = here("doc/appendix-VAST/both_indices.Rdata"))
