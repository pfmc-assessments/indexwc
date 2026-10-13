#' Write default text describing the index standardizations run from the \pkg{indexwc} package
#'
#' Writes both a markdown (.md) file which can be copied into an Quarto assessment report as
#' well as the PDF showing the formatted text. Relies on `inst/default_text.Rmd` and
#' `inst/child.Rmd` files where the child file is populated with information for each survey
#' associated with the species.
#'
#' @param species_list Vector of species names in the configuration data. If NULL, species with at least one used configuration are selected when `used_only` is TRUE; otherwise all configured species are selected.
#' @param dir Directory where the generated Markdown and PDF files are saved.
#' @param used_only If TRUE, include only configurations marked as used. Set to FALSE to generate text for all configured model runs.
#' @export
#' @import cli
#' @importFrom rmarkdown render
#' @author Ian G. Taylor
#' @examples
#' \dontrun{
#' write_default_text()
#' }
write_default_text <- function(
  species_list = NULL,
  dir = "inst",
  used_only = TRUE
) {
  # Survey data are fetched while rendering, so check connectivity before starting.
  if (!curl::has_internet()) {
    cli::cli_abort(
      "No internet connection detected. The function may not work."
    )
  }
  # Use packaged configuration so this works outside a source checkout.
  utils::data("configuration", package = "indexwc", envir = environment())

  # Select configured species by default, or validate names supplied by the caller.
  if (is.null(species_list)) {
    species_list <- configuration |>
      dplyr::filter(!used_only | used) |>
      dplyr::pull(species) |>
      unique() |>
      sort()
  } else {
    all_species <- configuration |>
      dplyr::pull(species) |>
      unique()
    if (!all(species_list %in% all_species)) {
      cli::cli_abort(
        "Some species in species_list are not found in the configuration file: {setdiff(species_list, all_species)}"
      )
    }
  }
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  # Stop before rendering if the requested output location is unavailable.
  if (!dir.exists(dir)) {
    cli::cli_abort("Could not create output directory {dir}.")
  }
  cli::cli_alert_info("species_list: {paste(species_list, sep = ', ')}")

  # Loop through each species in the species_list
  for (species in species_list) {
    cli::cli_alert_info("writing file for {species}")
    output_file <- paste0(gsub(" ", "_", species), "_default_text.pdf")
    # Pass parameters to the template instead of modifying its source file.
    rmarkdown::render(
      "inst/default_text.Rmd",
      output_file = output_file,
      output_dir = dir,
      params = list(common = species, used_only = used_only)
    )
  }
}
