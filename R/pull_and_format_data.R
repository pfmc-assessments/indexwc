#' Pull and format `data` for use in downstream functions.
#'
#' A list is returned with specifications to run a sdmTMB model, formatted and
#' filtered data, and a prediction grid used to estimate indices of abundance
#' in [calc_index_areas()] and [diagnose()].
#'
#' @param configuration_to_run A data frame based on the configuration data object
#'   that is used to pull and format data list used in downstream functions (e.g.,
#'   [run_sdmtmb()]). If a configuration_to_run is specified, the function will
#'   pull data from [nwfscSurvey::pull_catch()] for each line and will create a
#'   data list that is used by [run_sdmtmb()].
#' @param common_name A character string used to pull data from [nwfscSurvey::pull_catch()].
#'   This is only used if `configuration_to_run` is not specified.
#' @param survey A character string used to pull data from [nwfscSurvey::pull_catch()].
#'   Commonly used values are `NWFSC.Combo`, `Triennial`, `AFSC.Slope`, and `NWFSC.Slope`.
#'   This is only used if `configuration_to_run` is not specified.
#' @param years A numeric vector used to pull data from [nwfscSurvey::pull_catch()].
#'   This is only used if `configuration_to_run` is not specified.
#' @param verbose A logical that specifies if you want to print messages and
#'   warnings to the console. The default is `FALSE`.
#'
#' @return A list of configurations and data
#' * `species`
#' * `fxn`
#' * `source`
#' * `family`
#' * `formula`
#' * `min_depth`
#' * `max_depth`
#' * `min_latitude`
#' * `min_year`
#' * `max_year`
#' * `anisotropy`
#' * `knots`
#' * `spatiotemporal1`
#' * `spatiotemporal2`
#' * `share_range`
#' * `used`
#' * `data_raw`
#' * `data_filtred`
#'
#' @author Chantel R. Wetzel and Kelli F. Johnson
#' @export
#'
#'
pull_and_format_data <- function(
  configuration_to_run,
  common_name = NULL,
  survey = "NWFSC.Combo",
  years = 2003:2050,
  verbose = TRUE
) {
  if (missing(configuration_to_run)) {
    if (is.null(common_name)) {
      cli::cli_abort(
        "Either a common_name or configuration_to_run must be specified"
      )
    }
  }
  if (missing(configuration_to_run)) {
    pulled_data <- nwfscSurvey::pull_catch(
      common_name = common_name,
      survey = survey,
      years = years,
      verbose = verbose
    )

    format_data <- format_data(data = pulled_data)
    data <- list()
    data$species <- unique(format_data$common_name)
    data$fxn <- NULL
    data$source <- survey
    data$family <- "sdmTMB::delta_gamma()"
    data$formula <- ifelse(
      test = survey == "NWFSC.Combo",
      yes = "catch_weight ~ 0 + fyear + pass_scaled",
      no = "catch_weight ~ 0 + fyear"
    )
    data$min_depth <- min(format_data$depth)
    data$max_depth <- max(format_data$depth)
    data$min_latitude <- min(format_data$latitude)
    data$max_latitude <- max(format_data$latitude)
    data$min_year <- min(format_data$year)
    data$max_year <- max(format_data$year)
    data$anisotropy <- TRUE
    data$knots <- 250
    data$spatiotemporal1 <- "iid"
    data$spatiotemporal2 <- "iid"
    data$share_range <- FALSE
    data$data_raw <- format_data
    data$data_filtered <- format_data
    if (verbose) {
      cli::cli_alert_info(
        "The returned data object has default settings, users should evaluate
                          if model settings should be revised."
      )
    }
  } else {
    data <- configuration_to_run |>
      dplyr::rowwise() |>
      # Pull the data based on the function found in fxn column
      dplyr::mutate(
        data_raw = list(format_data(eval(parse(text = fxn)))),
        data_filtered = list(
          data_raw |>
            dplyr::filter(
              depth <= min_depth,
              depth >= max_depth,
              latitude >= min_latitude,
              latitude <= max_latitude,
              year >= min_year,
              year <= max_year
            )
        )
      ) |>
      dplyr::ungroup()
  }

  return(data)
}
