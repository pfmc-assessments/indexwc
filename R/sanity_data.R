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
  sanity_out <- sdmTMB::sanity(fit, silent = TRUE)

  # Extract names
  sanity_names <- names(sanity_out)

  # Extract logical values
  sanity_TF <- sapply(sanity_out, function(x) {
    if (length(x) == 1) {
      return(as.logical(x))
    } else {
      return(all(as.logical(x))) # Or use any()
    }
  })

  # Extract text descriptions from attributes
  sanity_text <- sapply(sanity_out, function(x) {
    msg <- attr(x, "message")
    if (!is.null(msg)) {
      return(msg)
    } else {
      return(as.character(x))
    }
  })

  # Create data frame
  out <- data.frame(
    names = sanity_names,
    logical = sanity_TF,
    text = sanity_text,
    stringsAsFactors = FALSE
  )

  return(out)
}
