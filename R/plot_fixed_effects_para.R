#' Plot the annual parameter estimates from the fit model
#'
#'
#' @param data List created by the sdmTMB:: fit function
#' @template dir 
#'
#' @import sdmTMB
#'
#' @author Chantel Wetzel
#' @export
#'
#' @examples
#' \dontrun{
#'  plot_fixed_effects_para(
#'        data = fit, 
#'        dir = file.path(sppdir, survey, "index", obs))
#' }
#'
#'
plot_fixed_effects_para <- function(data, dir) {

  est <- as.list(data$sd_report, "Estimate", report = FALSE)
  sd  <- as.list(data$sd_report, "Std. Error", report = FALSE)
  years <- sort(unique(data$data[, data$time]))

  n_plot <- ifelse(
    "b_j2" %in% names(est),
    2, 1
  )

  png(
    filename = file.path(dir, "fixed_effects_parameters.png"),
    height = 10,
    width = 10,
    units = "in",
    res = 300
  )
  on.exit(dev.off(), add = TRUE)

  par(mfrow = c(n_plot, 1))

  td <- tidy(data, model = 1, silent = TRUE)
  yr_i <- grep("year", td$term, ignore.case = TRUE)
   upr <- est$b_j[yr_i] + 2 * sd$b_j[yr_i]
  lwr <- est$b_j[yr_i] - 2 * sd$b_j[yr_i]
  main_text <- ifelse(
    n_plot == 2, 
    "Fixed Effects: Presence Model",
    "Fixed Effects"
  )

  plot(years, est$b_j[yr_i], ylim = range(c(lwr, upr)),
    ylab = "Parameter Estimates", xlab = "Year", 
    main = main_text)
  segments(years, lwr, 
     years, upr,
     col = "black")

  if (n_plot > 1) {
    td <- tidy(data, model = 2, silent = TRUE)
    upr <- est$b_j2[yr_i] + 2 * sd$b_j2[yr_i]
    lwr <- est$b_j2[yr_i] - 2 * sd$b_j2[yr_i]
    yr_i <- grep("year", td$term, ignore.case = TRUE)
    plot(years, est$b_j2[yr_i], ylim = range(c(lwr, upr)),
      ylab = "Parmater Estimates", xlab = "Year", 
      main = "Fixed Effects: Catch Rate Model")
    segments(
      years, lwr,
      years, upr,
      col = 'black'
    )    
  }


}