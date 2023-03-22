#' Estimate indices of abundance for combinations of surveys and populations
#'
#' Run {sdmTMB} to estimate indices of abundance for combinations of surveys
#' and populations present in `data`.
#'
#' @template data
#' @inheritParams sdmTMB::sdmTMB
#' @param dir_main A string specifying the file path to the working directory
#'   that will be populated with additional directories where the results will
#'   be saved.
#' @family run
run <- function(data = nwfscSurvey::pull_catch(survey = "NWFSC.Combo"),
                family = gaussian(link = "identity"),
                formula = lookup_formula("WCGBTS"),
                dir_main = getwd()) {
  # Checks
  # This code is NOT vectorized for family. So, ensure that only a single
  # family is passed in the argument.
  stopifnot(inherits(family, "family") | "clean_name" %in% names(family))

  # Format
  # format the data after setting class to ensure proper column names exist
  # adds formula column based on survey
  # TODO:
  # * Add checks for class of other data sets, e.g., ODFW
  if (all(c("cpue_kg_km2", "Pass") %in% colnames(data))) {
    class(data) <- c("nwfscSurvey", class(data))
  }
  data_formatted <- format_data(data)

  # Objects
  combinations <- data_formatted %>%
    dplyr::group_by(survey_name, common_name) %>%
    dplyr::count() %>%
    dplyr::mutate(
      common_without = format_common_name(common_name),
      survey_without = format_common_name(survey_name)
    )
  directories <- fs::path(
    dir_main,
    combinations[["common_without"]],
    combinations[["survey_without"]],
    format_family(family)
  )
  data_split <- data_formatted %>%
    dplyr::group_by(survey_name, common_name) %>%
    dplyr::group_split()

  # Run
  # Use {purrr} to loop over all surveys and populations after splitting the
  # data by survey and population, which is based on a common name because
  # scientific names can change over time or be grouped in various ways.
  results <- purrr::map2(
    .x = directories,
    .y = data_split,
    .f = run_sdmtmb,
    family = family,
    formula = formula
  )
}
