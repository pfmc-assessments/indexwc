southern_BC <- 49.0
southern_WA <- 46.0
southern_OR <- 42.0
southern_CA <- 32.0


utm_zone_09 <- 3156
utm_zone_10 <- 32610

usethis::use_data(
  southern_BC, southern_WA, southern_OR, southern_CA,
  utm_zone_09, utm_zone_10,
  internal = TRUE,
  overwrite = TRUE
)
