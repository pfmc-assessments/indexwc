lookup_is_mixture <- function(x) {
  any(grepl("mix", x[["family"]], ignore.case = TRUE))
}

lookup_is_delta <- function(x) {
  isTRUE(x[["family"]][["delta"]])
}
