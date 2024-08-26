#' Saves incremental plots
#'
#'
#' @param plot_list A list of plots created by one of the `reveal_*` functions (e.g. [reveal_groups()], [reveal_layers()], [reveal_aes()]]
#' @param basename The base file name that will be used for saving. 
#' @param ... Additional arguments (e.g. width, height) to be passed to [ggplot2::ggsave()]
#' @return The paths of the saved plots, invisibly
#' @export
#' @inherit reveal_groups examples
reveal_save <- function(plot_list,  basename, ...) {

  cli::cli_h2("Saving incremental plots")

  paths <- c()
  
  omit_blank <- ifelse(is.null(attr(plot_list,"omit_blank")), 
                       FALSE,
                       attr(plot_list,"omit_blank"))
  offset <- ifelse(omit_blank, 0, 1)
  
  for (i in 1:length(plot_list)) {
    number <- ifelse(i == length(plot_list),
                          paste0("_", i-offset, "_last"),
                          paste0("_", i-offset))
    
    ext <- tools::file_ext(basename)

    if (ext != ""){
      suffix <- paste0(number, ".", ext)
    } else {
      suffix <- number
    }

    filename <- paste0(tools::file_path_sans_ext(basename), suffix)
    
    filename <- ggplot2::ggsave(filename, plot_list[[i]], ...)
    paths <- c(paths, filename)
    cli::cli_alert_success("{.file {filename}}")
  }

  invisible(paths)

}
