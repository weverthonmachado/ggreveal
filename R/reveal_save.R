reveal_save <- function(plot_list, basename = "plot", ...) {

  cli::cli_h2("Saving incremental plots")

  for (i in 1:length(plot_list)) {
    suffix <- ifelse(i == length(plot_list),
                     paste0("_", i, "_full"),
                     paste0("_", i))
    filename <- paste0(basename, suffix, ".png")

    ggplot2::ggsave(filename, plot_list[[i]], ...)
    cli::cli_alert_success("{filename}")
  }

}
