plot_mesh <- function(mesh, file_name) {
  grDevices::png(
    filename = file_name,
    width = 7,
    height = 7,
    units = "in",
    res = 300,
    pointsize = 12
  )
  on.exit(dev.off(), add = TRUE, after = FALSE)
  plot(mesh)
  return(invisible(NULL))
}
