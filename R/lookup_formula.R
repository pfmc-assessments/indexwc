lookup_formula <- function(x) {
  out <- purrr::map_chr(
    .x = x,
    # + (1 | vessel_year) are increasingly not converging so they are being
    # left out by default
    .f = ~ dplyr::case_when(
      grepl("WCGBTS", .x) ~ "catch_weight ~ 0 + as.factor(year) + pass_scaled",
      .default = "catch_weight ~ 0 + as.factor(year)",
    )
  )
  out <- lapply(out, as.formula)
  # TODO: Think about the poor practice of returning different object types
  #       depending on input
  if (length(out) == 1) return(out[[1]])
  return(out)
}

    # TODO: fix the formula so the column list works
    # formula = catch_weight ~ 0 + as.factor(year) + pass_scaled + (1 | vessel_year),
    # formula = catch_weight ~ 0 + as.factor(year) + pass_scaled,
		# formula = catch_weight ~ 0 + as.factor(year) + depth_scaled, # Triennial
		# formula = catch_weight ~ 0 + as.factor(year), # Slope