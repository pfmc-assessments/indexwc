#' A deprecated function, use [run_sdmtmb()]
#'
#' Split your data into a list of data frames, e.g.,
#' `dplyr::split(data, common_name)` and then use [purrr::map()] on the
#' resulting list with `.f = run_sdmtmb`.
#' @param data The data object being passed in
#' @param family The name of the family used for the response variable
#' @param formula  The formula used in the model
#' @param dir_main The name of the main directory
#' @param n_knots The number of knots to specify for the SPDE mesh
#' @return
#' A `list` of {sdmTMB} `list`s, where each element in the list is the returned
#' object from [sdmTMB::sdmTMB()] when fitting data to a model and of the class
#' `sdmTMB`. The list of lists is because [purrr::map2()] is used to split the
#' input data by species and survey/source in case your data contain
#' combinations of these two categories in a long data frame.
#'
#' @family run
#' @export
run <- function(data,
                family,
                formula,
                dir_main = getwd(),
                n_knots = 500,
                ...) {
  lifecycle::deprecate_warn(
    when = "0.7",
    what = "run()",
    with = "run_sdmtmb()"
  )
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
