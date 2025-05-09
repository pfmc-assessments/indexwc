#' Create a default text describing the index standardizations run from the {indexwc} package
#'
#' @param species_list vector with strings matching a subset of those in the "species" column of the configuration file. If NULL, this will be the set of species that have at least one TRUE value in the "used" column.

write_default_text <- function(species_list = NULL) {
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
