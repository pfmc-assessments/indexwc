#' Write default text describing the index standardizations run from the \pkg{indexwc} package
#'
#' Writes both a markdown (.md) file which can be copied into an Quarto assessment report as
#' well as the PDF showing the formatted text. Relies on `inst/default_text.Rmd` and
#' `inst/child.Rmd` files where the child file is populated with information for each survey
#' associated with the species.
#'
#' @param species_list vector with strings matching a subset of those in the "species" column of the configuration file. If NULL, this will be the set of species that have at least one TRUE value in the "used" column.
#' @export
#' @import cli
#' @import here
#' @importFrom rmarkdown render
#' @author Ian G. Taylor
#' @examples
#' \dontrun{
#' write_default_text()
#' }
write_default_text <- function(species_list = NULL) {
  # check for internet connection
  if (!curl::has_internet()) {
    cli::cli_abort("No internet connection detected. The function may not work.")
  }
  # If species_list is NULL, read the configuration file and extract the list of species
  if (is.null(species_list)) {
    species_list <- read.csv(
      # file = "https://raw.githubusercontent.com/pfmc-assessments/indexwc/main/data-raw/configuration.csv"
      file = here::here("data-raw/configuration.csv") # Using local file because branch is not yet merged
    ) |>
      dplyr::filter(used) |>
      dplyr::pull(species) |>
      unique() |>
      sort() # Get unique and sorted list of species marked as "used"
  } else {
    # confirm that user input species_list is contained within the species in the configuration file
    all_species <- read.csv(
      # file = "https://raw.githubusercontent.com/pfmc-assessments/indexwc/main/data-raw/configuration.csv"
      file = here::here("data-raw/configuration.csv") # Using local file because branch is not yet merged
    ) |>
      dplyr::pull(species)
    if (!all(species_list %in% all_species)) {
      cli::cli_abort("Some species in species_list are not found in the configuration file: {setdiff(species_list, all_species)}")
    }
  }
  cli::cli_alert_info("species_list: {paste(species_list, sep = ', ')}")

  # Loop through each species in the species_list
  for (species in species_list) {
    cli::cli_alert_info("writing file for {species}")
    # Read the default text template file
    default_text_lines <- readLines("inst/default_text.Rmd")

    # Find the line containing the "common" field
    line_index <- grep("  common: ", default_text_lines)
    if (length(line_index) == 0) {
      # Abort if no line containing "common" is found
      cli::cli_abort("No line containing '  common: ' in inst/default_text.Rmd")
    }

    # Replace the "common" field with the current species name
    default_text_lines[line_index] <- paste0("  common: ", species)

    # Write the updated lines back to the template file
    writeLines(default_text_lines, "inst/default_text.Rmd")

    # Render the R Markdown file to generate outputs
    rmarkdown::render("inst/default_text.Rmd")

    # Copy the generated Markdown file to a species-specific file
    file.rename("inst/default_text.md", paste0("inst/", gsub(" ", "_", species), "_default_text.md"))

    # Copy the generated PDF file to a species-specific file
    file.rename("inst/default_text.pdf", paste0("inst/", gsub(" ", "_", species), "_default_text.pdf"))
  }
}
