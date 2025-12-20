#' Format the name of a distribution
#'
#' Format `clean_name` which is an item in the list returned from a call to a
#' distribution, e.g., `sdmTMB::delta_gamma()[["clean_name"]]`.
#' @param family A distribution from \pkg{sdmTMB} such as [sdmTMB::delta_gamma()].
#' @param pretty A logical specifying if you want the returned value to be
#'   formatted for pretty printing, which is helpful when the text is going to
#'   be placed in a report on a figure.
#' @return A string.
#' @author Kelli F. Johnson
#' @export
#' @examples
#' format_family(sdmTMB::delta_gamma())
#' format_family(sdmTMB::delta_gamma(), pretty = TRUE)
format_family <- function(family, pretty = FALSE) {
  get_name_from <- ifelse(
    "clean_name" %in% names(family),
    "clean_name",
    "family"
  )
  function_name <- gsub(
    x = family[[get_name_from]],
    pattern = "\\(.+$",
    replacement = "",
    perl = TRUE
  )
  if (pretty) {
    out <- gsub("_", " ", function_name)
  } else {
    out <- function_name
  }
  return(out)
}
