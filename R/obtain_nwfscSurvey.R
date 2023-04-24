obtain_nwfscSurvey <- function(common_name, survey) {
  data <- nwfscSurvey::pull_catch(common_name = common_name, survey = survey)
  # TODO
  # * think about making this a tibble before adding nwfscSurvey class
  class(data) <- c("nwfscSurvey", class(data))
  return(data)
}
