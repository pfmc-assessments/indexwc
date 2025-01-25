configuration <- tibble::as_tibble(read.csv(
  file.path("data-raw", "configuration.csv")
))

utils::write.csv(
  configuration,
  file = here::here("data-raw", "configuration.csv"),
  row.names = FALSE
)

usethis::use_data(
  configuration,
  overwrite = TRUE
)

