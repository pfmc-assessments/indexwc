#' Creates a data frame of information from [sdmTMB::sanity()]
#'
#' The returned data frame is useful for model checking after running several
#' models and not having access to the R console while running the models.
#' @param fit A returned object from [sdmTMB::sdmTMB()].
#' @author Kelli F. Johnson
#' @export
#' @seealso
#' * [sdmTMB::sanity()]
#' * [diagnose()]
#' @return
#' A csv with three columns, `name`, `logical`, and `text`, containing the
#' name of the test, a logical for the results of the test, and a text
#' explanation of what the test really means.
sanity_data <- function(fit) {
  # For some reason I had to force the list returned from sanity() with
  # purrr to a vector applying c and forcing the return to be logical
  sanity_TF <- purrr::map_lgl(sdmTMB::sanity(fit), c)
  sanity_text <- cli::cli_fmt(sdmTMB::sanity(fit)) |>
    gsub(pattern = "^.+ ([A-Z][a-z])", replacement = "\\1") |>
    as.vector()

  out <- data.frame(
    "names" = names(sanity_TF),
    "logical" = matrix(sanity_TF),
    text = sanity_text
  )
  return(out)
}
