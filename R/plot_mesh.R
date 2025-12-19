#' Plot a mesh object
#'
#' @param mesh A mesh object from [sdmTMB::make_mesh()]
#' @param file_name Optional file path to save the plot. If `NULL`, plot is
#'   returned but not saved.
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' mesh <- sdmTMB::make_mesh(data, xy_cols = c("x", "y"), n_knots = 500)
#'
#' # Return plot without saving
#' p <- plot_mesh(mesh, file_name = NULL)
#' print(p)
#' }
plot_mesh <- function(mesh, file_name = NULL) {
  mesh_plot <- ggplot2::ggplot() +
    inlabru::gg(mesh$mesh) +
    ggplot2::coord_fixed()

  # Save if file_name provided
  if(!is.null(file_name)) {
    ggplot2::ggsave(
      plot = mesh_plot,
      filename = file_name,
      width = 7,
      height = 7,
      units = "in",
      dpi = 300
    )
  }

  # Always return the ggplot object
  return(mesh_plot)
}
