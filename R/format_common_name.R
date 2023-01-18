format_common_name <- function(x) {
  tolower(
    gsub(
      pattern = "\\s",
      replacement = "_",
      x = gsub(
        pattern = "[[]:punctuation:]]|\\.",
        replacement = "",
        x = x
      )
    )
  )
}
