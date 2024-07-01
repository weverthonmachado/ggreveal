#' Saves incremental plots
#'
#'
#' @param plot_list A list of plots created by [reveal_by_group()], [reveal_by_layer()] or [reveal_by_panel()]
#' @param basename The base file name that will be used for saving
#' @param ... Additional arguments (e.g. width, height) to be passed to [ggplot2::ggsave()]
#' @export
#' @inherit reveal_by_group examples
reveal_save <- function(plot_list, basename = "plot", ...) {

  cli::cli_h2("Saving incremental plots")

  paths <- c()
  for (i in 1:length(plot_list)) {
    suffix <- ifelse(i == length(plot_list),
                     paste0("_", i, "_full"),
                     paste0("_", i))
    filename <- paste0(basename, suffix, ".png")
    
    filename <- ggplot2::ggsave(filename, plot_list[[i]], ...)
    paths <- c(paths, filename)
    cli::cli_alert_success("{.file {filename}}")
  }

  invisible(paths)

}
