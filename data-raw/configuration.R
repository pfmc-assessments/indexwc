# If adding/modifying configurations the following steps should be done:
# 1. Open and edit the configuration.csv file in the data-raw folder
# 2. Save and close the modified csv file
# 3. Run the below script to update the configuration.rda file that is loaded with the package

configuration <- read.csv(
  file.path("data-raw", "configuration.csv")
)

usethis::use_data(
  configuration,
  overwrite = TRUE
)
