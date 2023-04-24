#' Estimate indices of abundance for combinations of surveys and populations
#'
#' Run {sdmTMB} to estimate indices of abundance for combinations of surveys
#' and populations present in `data`. The same `formula` and `family` will be
#' used for all data sets present in data so you may wish to run this function
#' iteratively across different families or using {purrr}.
#'
#' @inheritParams format_data
#' @inheritParams sdmTMB::sdmTMB
#' @param dir_main A string specifying a path where results will be saved. The
#'   default is your current working directory.
#' @param n_knots An integer specifying the number of knots you want in your
#'   mesh that is created by {INLA}. More knots is not always better. The
#'   default is to use 500 knots. Future work will look at specifying a
#'   threshold distance between points rather than number of knots.
#' @param ... Optional arguments passed to [sdmTMB::sdmTMB()] can be passed
#'   using viable arguments of [sdmTMB::sdmTMB()]. Note that users
#'   cannot pass `anisotropy` or `sdmTMBcontrol` because both of these are set
#'   in [run_sdmtmb()], where `anisotropy = TRUE` because the coastline
#'   of the western portion of the U.S.A. is not perpendicular to the U.S.A.
#'   and three newton loops are specified in the control parameters.
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

  # Format
  # format the data to ensure proper column names exist
  # TODO:
  # * Require that input data have the appropriate class set prior
  #   to running this function or use format_data() to set the class
  if (
    all(c("cpue_kg_km2", "Pass") %in% colnames(data)) &&
    !inherits(data, "nwfscSurvey")
  ) {
    class(data) <- c("nwfscSurvey", class(data))
  }
  data_formatted <- format_data(data)

  # Objects
  data_formatted_grouped <- data_formatted %>%
    dplyr::group_by(survey_name, common_name)
  combinations <- data_formatted_grouped %>%
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
  data_split <- data_formatted_grouped %>%
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
