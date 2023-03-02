lookup_formula <- function(x) {
  purrr::map(
    .x = x,
    .f = ~ dplyr::case_when(
      grepl("WCGBTS", .x) ~ list(
        catch_weight ~ 0 + as.factor(year) + pass_scaled + (1 | vessel_year),
        catch_weight ~ 0 + as.factor(year) + pass_scaled + (1 | vessel_year)
      ),
      TRUE ~ list(
        catch_weight ~ 0 + as.factor(year) + (1 | vessel_year),
        catch_weight ~ 0 + as.factor(year) + (1 | vessel_year)
      )
    )
  )
}
