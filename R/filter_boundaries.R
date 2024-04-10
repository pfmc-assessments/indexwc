#' Eliminate boundaries that do not have any data in them or are duplicates
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
#' # Removes OR because there are only observations in WA and CA
#' filter_boundaries(y = c(34, 48), boundaries = boundaries_data)
#' # Shrinks northern borders and removes WA because no observations in WA
#' filter_boundaries(y = c(45.5, 34), boundaries = boundaries_data)
filter_boundaries <- function(y, boundaries) {
  stopifnot(is.vector(y))

  # Remove boundaries without any data inside of them
  bool_inside <- purrr::map_vec(
    boundaries_data,
    \(z) all_x_inside(y, z[2], z[1])
  )
  boundaries_with_data <- boundaries_data[bool_inside]
  rm(bool_inside)

  # Shrink the boundaries so we are not predicting to somewhere the species
  # has never been seen
  boundaries_tops <- shrink_boundary(
    purrr::map(boundaries_with_data, 1),
    max(y),
    ">"
  )
  boundaries_bottoms <- shrink_boundary(
    purrr::map(boundaries_with_data, 2),
    min(y),
    "<"
  )
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
  # Remove boundaries where upper equals lower b/c a point is right on the
  # boundary
  out <- out[out[, "upper"] != out[, "lower"], ]
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

# #' Ensure that the range has at least one x value in it
# #' @param x A vector of doubles.
# #' @param lower A single double value specifying the lower limit.
# #' @param upper A single double value specifying the upper limit.
# #' @examples
# #' # TRUE
# #' are_all_x_inside(1:5, 3, 5)
# #' # FALSE
# #' are_all_x_inside(1:2, 3, 5)
# #' # Error "lower < upper is not TRUE"
# #' \dontrun{are_all_x_inside(1:5, 6, 5)}
# #' @return
# #' A logical values indicating if all values in `x` are within or equal to
# #' the bounds specified by `lower`--`upper`.
all_x_inside <- function(x, lower, upper) {
  stopifnot(lower < upper)
  any(x >= lower & x <= upper)
}
