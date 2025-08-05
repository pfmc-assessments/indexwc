load(file.path("data", "configuration.rda"))

usethis::use_data(
  configuration,
  overwrite = TRUE
)

utils::write.csv(
  configuration,
  file = here::here("data-raw", "configuration.csv"),
  row.names = FALSE
)

