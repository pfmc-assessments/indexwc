#' Format `data` by standardizing column names and units
#'
#'
#' @param data A data frame containing tow-level information on catch weight
#'   and effort. Additional columns can be present such as depth.
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
#'
#' @author Chantel R. Wetzel and Kelli F. Johnson
#' @export
#'
#' @examples formatted_data <- format_data(data = catch_data)
#'
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
  data <- data %>%
    dplyr::rename_with(tolower) %>%
    dplyr::rename_with(gsub, pattern = "_dd", replace = "") %>%
    dplyr::rename(
      catch_numbers = total_catch_numbers,
      survey_name = project,
    ) %>%
    dplyr::mutate(
      # Catches are needed in mt for stock synthesis
      catch_weight = total_catch_wt_kg * 0.001,
      effort = area_swept_ha_der * 0.01,
      depth = depth_m * -1,
      vessel_year = as.numeric(
        as.factor(paste(vessel, year, sep = "_")),
        as.is = FALSE
      ) - 1,
      formula = lookup_formula(survey_name),
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
      depth
    ) %>%
    dplyr::filter(
      # TODO: fix the survey  names
      # 1997, 1999, 2000, and 2001 are the only years that fully sampled the coast
      !(survey_name == "AFSC.Slope" & year <= 1996),
      !(grepl("Triennial", survey_name) & year == 1977)
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
