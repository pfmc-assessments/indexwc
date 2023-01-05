run_sdmtmb2 <- function(dir = getwd(),
                       data,
                       family = sdmTMB::delta_gamma(),
                       formula,
                       n_knots = 500,
                       automated = TRUE) {
  # TODO:
  # * check if random effects in formula and if converged, if yes and no, then
  #   re run the model after changing the formula to not have random effects
  # * document california_current_grid
  # * check licence file to see if I am even able to steal it
  # * think about setting a distance cutoff for the triangulation network
  #   rather than pre-specifying the number of knots
  dir_data <- fs::path(dir, "data")
  dir_index <- fs::path(dir, "index")
  fs::dir_create(c(dir_data, dir_index))

  # Create prediction grid
  grid <- lookup_grid(
    x = data[["survey_name"]][1],
    years = sort(unique(data$year))
  )
  # TODO: think about vessel_year, might want a different level scaling things
  #       might not have to give this to grid, might be able to use as.factor()
  #       in the formula
  data$vessel_year <- as.factor(data$vessel_year)

  # plot and save the mesh
  mesh <- sdmTMB::make_mesh(
    data = data,
    xy_cols = c("x", "y"),
    n_knots = n_knots
  )
  grDevices::png(
    filename = fs::path(dir_data, "mesh.png"),
    width = 7,
    height = 7,
    units = "in",
    res = 300,
    pointsize = 12
  )
  plot(mesh)
  dev.off()

  # Run model
  fit <- sdmTMB::sdmTMB(
    # TODO: fix the formula so the column list works
    # formula = data[["formula"]][1],
    formula = catch_weight ~ 0 + as.factor(year) + pass_scaled + (1 | vessel_year),
    time = "year",
    offset = log(data$effort),
    data = data,
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
    predict_args = list(newdata = grid, re_form_iid = NA),
    index_args = list(area = grid$area_km2)
  )

  # There is no way to estimate the index with bias correction in sdmTMB::sdmTMB
  # which is why we have to call [sdmTMB::get_index()] even though we already
  # specified the predictions in [sdmTMB::sdmTMB()].
  index <- sdmTMB::get_index(
    fit, 
    bias_correct = TRUE
  )
  index$area <- "coastwide" 

  loglike <- logLik(fit)
  aic <- AIC(fit)

    # # Create indices by area
    # area_indices <- NULL 
    # for (strata in strata.limits[-1,"STRATA"]){

    #   sub_year_grid <- year_grid %>% 
    #     dplyr::filter(Lat < strata.limits[strata.limits$STRAT == strata, "north_border"]) %>%
    #     dplyr::filter(Lat >= strata.limits[strata.limits$STRAT == strata, "south_border"])
      
    #   pred_area <- predict( #does not work with sdmTMB::predict
    #     fit, 
    #     newdata = sub_year_grid,
    #     return_tmb_object = TRUE
    #   )

    #   sub_index <- sdmTMB::get_index(
    #     pred_area, #fit, # skipping prediction step
    #     area = sub_year_grid$Area_km2,
    #     bias_correct = FALSE # TRUE
    #   )

    #   sub_index$area <- strata
    #   area_indices <- rbind(area_indices, sub_index)
    # }

    # # Need to fix this for plotting
    # all_indices <- rbind(index, area_indices)
    # # Plot the index
    # plot_indices(
    #   data = all_indices, 
    #   plot_info = info, 
    #   save_loc = dir_data, 
    #   ymax = NULL
    # )

    # # Add diagnostics
    # # 1) QQ plot
    # # 2) Residuals by year
    # resids <- residuals(fit)
    # grDevices::png(filename = fs::path(dir_index, "qq.png"),
    #   width = 7, height = 7, units = "in", res = 300, pointsize = 12)
    # qqnorm(resids)
    # qqline(resids)
    # dev.off()

    # catch_data$resids <- resids
    # grDevices::png(filename = fs::path(dir_index, "residuals.png"),
    #   width = 14, height = 7, units = "in", res = 300, pointsize = 12)
    # ggplot2::ggplot(catch_data, ggplot2::aes(Lon, Lat, colour = resids)) +
    #   ggplot2::geom_point() +
    #   ggplot2::facet_wrap(~Year) +
    #   ggplot2::scale_colour_gradient2() +
    #   ggplot2::coord_fixed()
    # dev.off()

    # save(
    #   data, mesh, year_grid, index, fit, area_indices, all_indices,
    #   loglike, aic, catch_data, plot_info,
    #   file = fs::path(dir_index, "sdmTMB_save.RData")
    # )
}
