boundaries_data <- list(
  Coastwide = c(southern_BC, southern_CA),
  WA = c(southern_BC, southern_WA),
  OR = c(southern_WA, southern_OR),
  CA = c(southern_OR, southern_CA),
  # For shortspine thornyhead
  "North of Point Conception" = c(southern_BC, 34.45),
  "South of Point Conception" = c(34.45, southern_CA),
  # Cape Mendocino (Northern California)
  "North of Cape Mendocino" = c(southern_BC, 40.166667),
  "South of Cape Mendocino" = c(40.166667, southern_CA),
  # For sablefish
  "North of Monterey Bay" = c(southern_BC, 36.00),
  "South of Monterey Bay" = c(36.00, southern_CA)
)

usethis::use_data(
  boundaries_data,
  overwrite = TRUE
)
