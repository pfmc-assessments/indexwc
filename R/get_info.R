get_info <- function(species = "yellowtail rockfish", source = "NWFSC.Combo") {
  info <- configuration |>
    dplyr::filter(species == species, source == source)

  if (nrow(info) > 1) {
    warning(
      "multiple rows in configuration for this species/survey combination"
    )
    info <- info[1, ]
  }

  if (info$min_depth != -55) {
    warning("add text about min depth")
  }
  if (info$max_depth > -Inf) {
    info$truncation_text <- glue::glue("The data were truncated to depths less than {abs(info$max_depth)} m prior to modelling given that there were zero positive encounters in depths deeper than {abs(info$max_depth)} m.")
    info$grid_text <- glue::glue("The prediction grid was also truncated to only include available survey locations in depths between {abs(info$min_depth)}--{abs(info$max_depth)} m to limit extrapolating beyond the data and edge effects.")
  }
  if (info$min_depth == -55 & info$max_depth == -Inf) {
    info$truncation_text <- glue::glue("The data and prediction grid were not truncated by depth, so the estimates cover the full depth range of the survey.")
  }

  
  info$dist <- dplyr::case_when(
    info$family == "sdmTMB::delta_gamma()" ~ "gamma",
    info$family == "sdmTMB::delta_lognormal()" ~ "lognormal",
    info$family == "sdmTMB::delta_tweedie()" ~ "tweedie"
  )

  covariates <- stringr::str_split(
    string = info$formula,
    pattern = " \\+ "
  )[[1]][-1]
  covariates <- covariates |>
    dplyr::recode(
      "fyear" = NULL,
      "pass_scaled" = "pass",
      "depth_scaled" = "depth",
      "depth_scaled_squared" = "depth-squared",
      "fyear*split_mendocino" = "annual proportion north and south of Cape Mendocino"
    )

  if (length(covariates) > 0) {
    info$covariates_text <- paste0(
      "The following additional covariates were included: ",
      paste(covariates, collapse = ", "),
      "."
    )
  } else {
    info$covariates_text <- "No other covariates were modeled."
  }

  info$spatiotemporal_text <- dplyr::case_when(
    info$spatiotemporal1 == "iid" & info$spatiotemporal2 == "iid" ~
      "Spatial and spatiotemporal variation was included in the encounter probability and the positive catch rate model.",
    info$spatiotemporal1 == "off" & info$spatiotemporal2 == "iid" ~
      "Spatial and spatiotemporal variation was included in the positive catch rate model but not the encounter probability.",
    info$spatiotemporal1 == "iid" & info$spatiotemporal2 == "off" ~
      "Spatial and spatiotemporal variation was included in the encounter probability but not the positive catch rate model.",
    info$spatiotemporal1 == "off" & info$spatiotemporal2 == "off" ~
      "Spatial and spatiotemporal variation was not included in either the encounter probability nor the positive catch rate model."
  )
  return(info)
}
