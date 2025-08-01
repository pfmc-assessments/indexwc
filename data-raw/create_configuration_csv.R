load(file.path("data", "configuration.rda"))

utils::write.csv(
  configuration,
  file = here::here("data-raw", "configuration.csv"),
  row.names = FALSE
)
