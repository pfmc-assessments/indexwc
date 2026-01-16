#' A list of northern and southern borders for U.S. West Coast areas
#'
#' Northern and southern borders for areas of interest along the U.S. West
#' Coast. Where, some areas are specific to management regions of the Pacific
#' Fisheries Management Council and some are entirely related to ecological
#' boundaries from currents, etc. that limit movement of populations.
#'
#' @format ## `boundaries_data`
#' A list with 10 entries, where each entry is a numeric vector of length two,
#' with the northern and southern boundaries for that area:
#' \describe{
#'   \item{coastwide}{U.S. West Coast from Southern British Columbia to Northern
#'    Mexico}
#'   \item{WA}{Washington state}
#'   \item{OR}{Oregon state}
#'   \item{CA}{California state}
#'   \item{North of Point Conception}{North of Point Conception}
#'   \item{South of Point Conception}{South of Point Conception}
#'   \item{North of Cape Mendocino}{North of Cape Mendocino}
#'   \item{South of Cape Mendocino}{South of Cape Mendocino}
#'   \item{North of Monterey Bay}{North of Monterey Bay}
#'   \item{South of Monterey Bay}{South of Monterey Bay}
#' }
#' A list of named areas with a numeric vector of northern and southern
#' boundaries for each area. Current areas include
#' * coastwide for the U.S. West Coast
#' * Washington (WA)
#' * Oregon (OR)
#' * California (CA)
#' * North of Point Conception
#' * South of Point Conception
#' * North of Cape Mendocino
#' * South of Cape Mendocino
#' * North of Monterey Bay
#' * South of Monterey Bay
"boundaries_data"

#' California Current prediction grid
#'
#' A 4km prediction grid for the West Coast Groundfish Bottom Trawl Survey,
#' used to generate biomass indices from predictions.
#'
#' @format A data frame
#' @source Keller et al. 2017
"california_current_grid"

#' Configuration data for west coast index standardization models
#'
#' Configuration settings for different species and model specifications.
#'
#' @format A data frame
"configuration"

#' Yellowtail rockfish survey data
#'
#' West Coast Groundfish Bottom Trawl Survey data for yellowtail rockfish, 2003 - 2023. Data
#' represents a subset of all hauls in each year, within the 55m - 425m depth range. The
#' latitudinal extent is 33.5 - 48.5 degrees N.
#'
#' @format A data frame
"yellowtail"

#' Mapping data from rnaturalearth
#'
#' This data object represents the output of a call to rnaturalearth using
#' mapdata <- rnaturalearth::ne_countries(scale = "medium",
#' returnclass = "sf", country = "united states of america") and is included
#' here to avoid dependencies on rnaturalearth
#'
#' @format Class is sf and data.frame
"mapdata"
