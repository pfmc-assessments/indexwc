library(dplyr)
library(ggplot2)
library(sdmTMB)
head(pcod)

mesh <- make_mesh(pcod, xy_cols = c("X", "Y"), cutoff = 10)
fit <- sdmTMB(
  density ~ s(depth, k = 5),
  data = pcod,
  mesh = mesh,
  family = tweedie(link = "log"),
  spatial = "on"
)
# presence-absence model
fit <- sdmTMB(
  present ~ s(depth, k = 5),
  data = pcod, 
  mesh = mesh,
  family = binomial(link = "logit")
)
# spatiotemporal model
fit_spatiotemporal <- sdmTMB(
  density ~ s(depth, k = 5), 
  data = pcod, 
  mesh = mesh,
  time = "year",
  family = tweedie(link = "log"), 
  spatial = "off", 
  spatiotemporal = "ar1"
)
# time-varying intercept
fit <- sdmTMB(
  density ~ 0 + s(depth, k = 5), 
  time_varying = ~ 1, 
  data = pcod, mesh = mesh,
  time = "year",  
  family = tweedie(link = "log"),
  silent = FALSE # see progress
)
# time-varying random-walk of depth
fit <- sdmTMB(
  density ~ 1, 
  time_varying = ~ 0 + depth_scaled + depth_scaled2,
  data = pcod, mesh = mesh,
  time = "year",
  family = tweedie(link = "log"),
  spatial = "off",
  spatiotemporal = "ar1",
  silent = FALSE
)
# spatially varying coefficients
pcod$year_scaled <- as.numeric(scale(pcod$year))
fit <- sdmTMB(
  density ~ s(depth, k = 5) + year_scaled,
  spatial_varying = ~ 0 + year_scaled, 
  data = pcod, mesh = mesh, 
  time = "year",
  family = tweedie(link = "log"),
  spatiotemporal = "off"
)
# random integers
pcod$year_factor <- as.factor(pcod$year)
fit <- sdmTMB(
  density ~ s(depth, k = 5) + (1 | year_factor),
  data = pcod, mesh = mesh,
  time = "year",
  family = tweedie(link = "log")
)
# breakpoint and threshold effects
fit <- sdmTMB(
  present ~ 1 + breakpt(depth_scaled), 
  data = pcod, mesh = mesh,
  family = binomial(link = "logit")
)
fit <- sdmTMB(
  present ~ 1 + logistic(depth_scaled), 
  data = pcod, mesh = mesh,
  family = binomial(link = "logit")
)


fit
tidy(fit, conf.int = TRUE)
tidy(fit, effects = "ran_pars", conf.int = TRUE)
plot_smooth(fit, ggplot = TRUE)
p <- predict(fit, newdata = qcs_grid)
head(p)
ggplot(p, aes(X, Y, fill = exp(est))) + geom_raster() +
  scale_fill_viridis_c(trans = "sqrt")

p_st <- predict(fit_spatiotemporal, newdata = qcs_grid, 
  return_tmb_object = TRUE, area = 4)
index <- get_index(p_st)
ggplot(index, aes(year, est)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "grey90") +
  geom_line(lwd = 1, colour = "grey30") +
  labs(x = "Year", y = "Biomass (kg)")
cog <- get_cog(p_st, format = "wide")
ggplot(cog, aes(est_x, est_y, colour = year)) +
  geom_pointrange(aes(xmin = lwr_x, xmax = upr_x)) +
  geom_pointrange(aes(ymin = lwr_y, ymax = upr_y)) +
  scale_colour_viridis_c()

