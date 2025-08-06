# check that configuration.rda matches configuration.csv
test_that("configuration.rda matches configuration.csv", {
  # Note: configuration.rda is automatically loaded when package is installed
  load(file.path("data", "configuration.rda"))

  # Read the CSV version of configuration
  # (reading from github because data-raw folder doesn't seem to be visible
  # to the installed package)
  configuration_csv <- tibble::as_tibble(read.csv(
    # file.path("data-raw", "configuration.csv")
    "https://raw.githubusercontent.com/pfmc-assessments/indexwc/refs/heads/main/data-raw/configuration.csv"
  ))

  # Check if the configuration and configuration_csv tibbles are identical
  expect_equal(
    configuration,
    configuration_csv
  )
})
