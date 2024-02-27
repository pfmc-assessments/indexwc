#' Write a file with a single logical regarding the hessian status
#'
#' @param fit A model object returned from [sdmTMB::sdmTMB()].
#' @param file A path to the file you want to write. The default path is
#'   `hess_logical.txt`, which will be saved in your current working directory.
#' @return
#' A logical is invisibly returned providing the status of the hessian.
#' @author
#' Kelli F. Johnson
write_hessian <- function(fit, file = "hess_logical.txt") {
  writeLines(
    text = as.character(fit[["pos_def_hessian"]]),
    con = file
  )
  return(invisible(fit[["pos_def_hessian"]]))
}
