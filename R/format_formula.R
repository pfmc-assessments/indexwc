format_formula <- function(x) {
  # For delta models, users can input a list.
  # So, call this function on each list member.
  if (inherits(x, "list") && length(x) == 2) {
    x <- purrr::map(x, format_formula)
  }
  # This is the typical behavior that should be used and will be called
  # above when this function calls itself to go from character to formula.
  if (inherits(x, "character")) {
    stopifnot(length(x) == 1)
    if (length(x) == 3 && x[1] == "~") {
      x <- paste(x[2], x[1], x[3])
    }
    x <- as.formula(paste(x, collapse = ""))
  }
  return(x)
}

is_depth_in_formula <- function(x, delta_lgl) {
  purrr::map_lgl(
    .x = if (length(x) != 2 && delta_lgl) {
      list(x, x)
    } else {
      x
    },
    .f = ~ any(grepl(pattern = "depth_scaled", .x))
  )
}
