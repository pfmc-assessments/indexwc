#' Determine which boundaries in a group should be eliminated
#'
#' @details Sometimes boundaries can be exactly equal and therefore do not need
#' to be included in a set. Other times a boundary was initially valid but
#' because there were no observations in the southern or northern limits the
#' truncated boundary makes the area become exactly equal to another boundary.
#' This can often be the case when there are no southern observations and a
#' boundary becomes equal to the coastwide boundary. Sometimes a northern
#' boundary is further south than the southern boundary, which essentially is a
#' negative area. The opposite can also happen. All of the above constitute bad
#' boundaries.
#'
#' Users often input perfectly fine boundaries but it is the truncation to only
#' positive tows, which is in effort to not predict to areas without
#' observations that leads to the boundary being bad.
#'
#' @param y A vector of y-axis coordinates, i.e., latitude. Coordinates should
#'   be in real values whether that is decimal degrees or m it does not matter
#'   but they cannot be character values.
#' @param boundaries TODO document
#' @return
#' A data frame with two columns (i.e., `upper` and `lower`) is returned,
#' where the bad areas are removed. If the input vectors are named, then the
#' resulting rows of the returned data frame will also be named.
#' @examples
#' filter_boundaries(y = c(34, 48), boundaries = boundaries_data)
filter_boundaries <- function(y, boundaries) {
  stopifnot(is.vector(y))

  # Pull out boundaries for each state if there are no data for that state
  if (length(y[y > southern_WA]) == 0) {
    boundaries <- boundaries[
      -which(names(boundaries) %in% c("WA", "wa", "washington", "Washington"))
    ]
  }
  if (length(y[y < southern_WA]) == 0 & length(y[y > southern_OR]) == 0) {
    boundaries <- boundaries[
      -which(names(boundaries) %in% c("OR", "or", "oregon", "Oregon"))
    ]
  }
  if (length(y[y < southern_OR]) == 0) {
    boundaries <- boundaries[
      -which(names(boundaries) %in% c("CA", "ca", "california", "California"))
    ]
  }

  boundaries_tops <- shrink_boundary(purrr::map(boundaries, 1), max(y), ">")
  boundaries_bottoms <- shrink_boundary(purrr::map(boundaries, 2), min(y), "<")

  # Join the vectors of boundaries into a matrix
  wide_df <- dplyr::bind_rows(boundaries_tops, boundaries_bottoms)

  # Find the areas where truncated for only positive tows led to a boundary
  # that is above or below the adjoining boundary, which makes the area have
  # a negative amount of space.
  southern_is_north_of_northern <- which(wide_df[2, ] > wide_df[1, ])
  northern_is_south_of_southern <- which(wide_df[1, ] < wide_df[2, ])

  # Find the areas that are duplicated because truncating to the limits of
  # positive tows can make the areas of some groups the exact same.
  duplicated_groups <- as.data.frame(t(wide_df)) |>
    dplyr::mutate(
      pasted = paste(V1, V2),
      duplicates = duplicated(pasted)
    )

  # Find the unique locations in the input vectors that are bad and return
  # these values.
  bad_groups <- sort(unique(c(
    southern_is_north_of_northern,
    northern_is_south_of_southern,
    which(duplicated_groups[["duplicates"]])
  )))
  out <- t(dplyr::bind_rows(
    boundaries_tops[!seq_along(boundaries_tops) %in% bad_groups],
    boundaries_bottoms[!seq_along(boundaries_bottoms) %in% bad_groups]
  ))
  colnames(out) <- c("upper", "lower")
  return(out)
}

# Internal function to shrink border boundaries if no positive tows above or
# below the boundary
shrink_boundary <- function(x, y, .f) {
  ifelse(
    test = eval(parse(text = paste("x", .f, "y"))),
    yes = y,
    no = x
  )
}
