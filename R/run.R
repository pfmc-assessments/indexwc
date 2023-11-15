#' Estimate indices of abundance for combinations of surveys and populations
#'
#' Run {sdmTMB} to estimate indices of abundance for combinations of surveys
#' and populations present in `data`. The same `formula` and `family` will be
#' used for all data sets present in data so you may wish to run this function
#' iteratively across different families or using {purrr}.
#'

#' @return
#' A `list` of {sdmTMB} `list`s, where each element in the list is the returned
#' object from [sdmTMB::sdmTMB()] when fitting data to a model and of the class
#' `sdmTMB`. The list of lists is because [purrr::map2()] is used to split the
#' input data by species and survey/source in case your data contain
#' combinations of these two categories in a long data frame.
#' @family run
#' @export
#' @examples
#' \dontrun{
#' # Get some data
#' data <- nwfscSurvey::pull_catch(
#'   common_name = "sablefish",
#'   survey = "NWFSC.Combo"
#' )
#' # Run a single species
#' results <- run(
#'   data = data,
#'   family = sdmTMB::tweedie(),
#'   formula = lookup_formula("WCGBTS")
#' )
#' # Run without the spatiotemporal component by passing arguments using ...
#' results <- run(
#'   data = data,
#'   family = sdmTMB::tweedie(),
#'   formula = lookup_formula("WCGBTS"),
#'   spatiotemporal = "off"
#' )
#' }
run <- function(data,
                family,
                formula,
                dir_main = getwd(),
                n_knots = 500,
                ...) {
  # Checks
  stopifnot(inherits(family, "family"))
  stopifnot(all(
    c(
      "year", "fyear", "survey_name", "common_name",
      "catch_weight", "effort", "x", "y"
    ) %in%
    colnames(data)
  ))

  # Objects
  data_grouped <- data %>%
    dplyr::group_by(survey_name, common_name)
  combinations <- data_grouped %>%
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
  data_split <- data_grouped %>%
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
    formula = formula,
    n_knots = n_knots,
    ...
  )

  return(results)
}
