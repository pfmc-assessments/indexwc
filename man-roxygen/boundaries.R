#' @param boundaries A named list of northern and southern boundaries for a
#'   set of areas. The list can contain as many areas as you would like but
#'   it must contain at least one area and each area must be a vector of two
#'   real numbers specified in decimal degrees. The order of the areas only
#'   matters if you care what order they are plotted because the names will
#'   be turned into factors. The default value uses a data object called
#'   `boundaries_data`, which is a list of several areas along the U.S. West
#'   Coast, including a coastwide area going from the northern Washington
#'   border to the southern California border.
