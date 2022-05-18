plot_betas <- function(vast_model, sdmTMB_model, vast_par = "beta1_ft", 
  sdmTMB_pars = 1, years, version = 'main') {
  s <- vast_model$parameter_estimates$SD
  vast_est1 <- as.list(s, "Estimate", report = FALSE)
  vast_est2 <- as.list(s, "Estimate", report = TRUE)
  vast_sd1 <- as.list(s, "Std. Error", report = FALSE)
  vast_sd2 <- as.list(s, "Std. Error", report = TRUE)
  sdmTMB_est <- as.list(sdmTMB_model$sd_report, "Estimate", report = FALSE)
  sdmTMB_sd <- as.list(sdmTMB_model$sd_report, "Std. Error", report = FALSE)
  b_year_vast <- vast_est1[[vast_par]][!is.na(vast_sd1[[vast_par]])]
  b_year_vast_se <- vast_sd1[[vast_par]][!is.na(vast_sd1[[vast_par]])]
  #years <- sort(unique(pcod$year))
  lwr_vast <- b_year_vast - 2 * b_year_vast_se
  upr_vast <- b_year_vast + 2 * b_year_vast_se
  plot(years, b_year_vast, ylim = range(c(lwr_vast, upr_vast)))
  segments(years, lwr_vast, years, upr_vast)
  years <- years + 0.05
  if (version == "main"){
     points(years, sdmTMB_est$b_j[,sdmTMB_pars])
     segments(years, sdmTMB_est$b_j[,sdmTMB_pars] - 2 * sdmTMB_sd$b_j[,sdmTMB_pars], 
     years, sdmTMB_est$b_j[,sdmTMB_pars] + 2 * sdmTMB_sd$b_j[,sdmTMB_pars],
     col = "red")
  } else {
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
  }

  legend("topright", legend = c("VAST", "sdmTMB"), 
    col = c("black", "red"), bty = "n", lty = c(1, 1))
}