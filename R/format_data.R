#' Format `data` by standardizing column names and units
#'
#'
#' @template data
#'
#' @return A data frame with all lower case column names.
#' * `year`
#' * `survey_name`
#' * `common_name`
#' * `catch_numbers`
#' * `catch_wt` (mt)
#' * `effort` ($km^2$)
#' * `pass_scaled` after subtracting the mean
#' * `vessel_year` pasted together, made numeric, and subtract 1
#' * `longitude` (decimal degrees)
#' * `latitude` (decimal degrees)
#' * `x` and `y` are in Universal Transverse Mercator (UTM)
#' * `depth` (m) with positive entries representing measurements above sea
#'   level and negative entries representing measurements below sea level
#' * `depth_scaled` depth (m) scaled using [base::scale()]; so, subtracting the
#'    mean and dividing by the standard deviation, these values are saved in
#'    attributes for this column so that you can reference them later
#' * `depth_scaled_squared` is `depth_scaled^2`
#'
#' @author Chantel R. Wetzel and Kelli F. Johnson
#' @export
#'
#' @examples
#' catch_wcgbts_canary <- nwfscSurvey::pull_catch(
#'   common_name = "canary rockfish",
#'   years = 2021:2022,
#'   survey = "NWFSC.Combo"
#' )
#' # Must assign the data a class so {indexwc} knows how to format it
#' class(catch_wcgbts_canary) <- c("nwfscSurvey", class(catch_wcgbts_canary))
#' formatted_data <- format_data(data = catch_wcgbts_canary)
format_data <- function(data, ...) {
  UseMethod("format_data", data)
}

#' @export
format_data.default <- function(data, ...) {
  # TODO: Further develop the default code to ensure that all
  #       necessary columns are present
  cols <- colnames(data)
  data <- data %>%
    dplyr::rename_with(.fn = tolower)

  return(data)
}

#' @export
format_data.data.frame <- function(data, ...) {
  # Nothing is needed for formatting when data has the data.frame
  # class, just move on to the next class present.
  NextMethod()
}

#' @export
format_data.odfw <- function(data, ...) {
  # 1. Ensure numerics are actually numerics
  # 1. GF_OpenDepth should be a factor
  # 1. Create column of presence/absence

  stop("Methods to format ODFW data do not exist yet")
}

#' @export
format_data.nwfscSurvey <- function(data, ...) {
  if (!"Datetime_utc_iso" %in% colnames(data)) {
    stop(
      "Please use nwfscSurvey::pull_catch() not nwfscSurvey::PullCatch.fn() ",
      "to pull your data."
    )
  }
  data <- data %>%
    dplyr::rename_with(tolower) %>%
    dplyr::rename_with(gsub, pattern = "_dd", replace = "") %>%
    dplyr::rename(
      catch_numbers = total_catch_numbers,
      survey_name = project,
    ) %>%
    dplyr::mutate(
      # TODO: complete the case_when statement for all known survey names
      #       make them match the sa4ss glossary
      survey_name = dplyr::case_when(
        survey_name == "Groundfish Slope and Shelf Combination Survey" ~ "WCGBTS",
        survey_name == "Groundfish Triennial Shelf Survey" ~ "Triennial",
        survey_name == "AFSC/RACE Slope Survey" ~ "AFSC_Slope",
        survey_name == "Groundfish Slope Survey" ~ "NWFSC_Slope",
        TRUE ~ "unknown survey"
      ),
      # Catches are needed in mt for stock synthesis
      catch_weight = total_catch_wt_kg * 0.001,
      effort = area_swept_ha_der * 0.01,
      depth = depth_m * -1,
      vessel_year = as.numeric(
        as.factor(paste(vessel, year, sep = "_")),
        as.is = FALSE
      ) - 1,
      depth_scaled = scale(depth),
      depth_scaled_squared = depth_scaled^2,
      pass_scaled = pass - mean(range(pass))
    ) %>%
    dplyr::select(
      year,
      survey_name,
      common_name,
      catch_numbers,
      catch_weight,
      effort,
      pass_scaled,
      vessel_year,
      longitude,
      latitude,
      depth,
      depth_scaled,
      depth_scaled_squared
    ) %>%
    dplyr::filter(
      # TODO: fix the survey  names
      # 1997, 1999, 2000, and 2001 are the only years that fully sampled the coast
      !(survey_name == "AFSC.Slope" & year <= 1996),
      # 1997 was a smaller survey
      !(grepl("Triennial", survey_name) & year == 1977),
      # Truncate Triennial by depth and latitude to spatial extent present in
      # every year to allow for a continuous time series
      !(common_name %in% c("petrale sole", "canary rockfish") &
        depth > 366 & grepl("Triennial", survey_name)
      ),
      !(common_name %in% c("petrale sole", "canary rockfish") &
        latitude < 36.8 & grepl("Triennial", survey_name)
      ),
    )
  data_utm <- sdmTMB::add_utm_columns(
    data,
    utm_crs = 32610,
    utm_names = c("x", "y")
  ) %>%
    dplyr::rename_with(tolower) %>%
    tibble::as_tibble()

  return(data_utm)
}
