plot_betas_delta <- function(vast_model, sdmTMB_model, vast_par = "beta1_ft", sdmTMB_pars = 1,
  years = sort(unique(fit_sdmTMB$data[[fit_sdmTMB$time]]))) {
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
    td <- tidy(fit_sdmTMB)
    yr_i <- grep("year", td$term, ignore.case = TRUE)
    points(years, sdmTMB_est$b_j[yr_i])
    segments(years, sdmTMB_est$b_j[yr_i] - 2 * sdmTMB_sd$b_j[yr_i],
      years, sdmTMB_est$b_j[yr_i] + 2 * sdmTMB_sd$b_j[yr_i],
      col = "red"
    )
  } else {
    td <- tidy(fit_sdmTMB, model = 2)
    yr_i <- grep("year", td$term, ignore.case = TRUE)
    points(years, sdmTMB_est$b_j2[yr_i])
    segments(years, sdmTMB_est$b_j2[yr_i] - 2 * sdmTMB_sd$b_j2[yr_i],
      years, sdmTMB_est$b_j2[yr_i] + 2 * sdmTMB_sd$b_j2[yr_i],
      col = "red"
    )
  }
  legend("topright",
    legend = c("VAST", "sdmTMB"),
    col = c("black", "red"), bty = "n", lty = c(1, 1)
  )
}

extract_vast_index <- function(x, dir_name = paste0(tempdir(), "/")) {
  suppressWarnings({
    vi <- FishStatsUtils::plot_biomass_index(x,
      DirName = dir_name)
  })
  est <- vi$Table$Estimate
  lwr <- exp(log(est) + qnorm(0.025) * vi$Table$`Std. Error for ln(Estimate)`)
  upr <- exp(log(est) + qnorm(0.975) * vi$Table$`Std. Error for ln(Estimate)`)
  vast_i <- data.frame(est, lwr, upr)
  vast_i$year <- as.numeric(fit$year_labels)
  vast_i <- dplyr::filter(vast_i, est > 0)
  vast_i$index <- "VAST"
  vast_i
}
