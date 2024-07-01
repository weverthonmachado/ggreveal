#' Reveal plot by facet
#'
#' Creates a list of plots, showing data incrementally by layers.
#'
#' @param p A ggplot2 object, in which a `group` aesthetic is used.
#' @param order (optional) A numeric vector specifying in which order to reveal
#' the panels. 
#' @param what (optional) one of `"data"` or `"everything"`.' With `"data"`, the
#' basic graph layout (including axes and facet labels) is shown from the start,
#' and only the data points are shown incrementally. With `"everything"`, the
#' entire panels are shown incrementally.
#' @return A list of ggplot2 objects, which can be passed to [reveal_save()]
#' @export
#' @examples
#' # Create full plot
#' library(ggplot2)
#' data("mtcars")
#'
#' p <- mtcars |>
#'   ggplot(aes(mpg, wt,
#'              color = factor(vs),
#'              group = factor(vs))) +
#'   geom_point() +
#'   geom_smooth(method="lm",
#'               formula = 'y ~ x',
#'               linewidth=1) +
#'   facet_wrap(~am)
#' p
#'
#' # Only data
#' plot_list <- reveal_panels(p, what = "data")
#' plot_list[[1]]
#' plot_list[[2]]
#' plot_list[[3]]
#'
#' # Everything
#' plot_list <- reveal_panels(p, what = "everything")
#' plot_list[[1]]
#' plot_list[[2]]
#' plot_list[[3]]
#'
#'\dontrun{
#' # Save plots
#' reveal_save(plot_list, "myplot", width = 8, height = 4)
#' }
reveal_panels <- function(p, order = NULL, what = c("data", "everything")){

  # Check arguments
  "ggplot" %in% class(p) || rlang::abort(paste(deparse(substitute(p)),
                                        "is not a ggplot object"))

  !is.na(stringr::str_extract(
            tolower(class(p$facet)[1]),
            "grid|wrap")) ||  rlang::abort(paste("Plot does not use facet_wrap or",
                                                  "facet_grid. Maybe use",
                                                  "reveal_groups or reveal_layers?")
                                                                )
  
  omit_blank <- FALSE
  n_panels <- length(unique(ggplot2::ggplot_build(p)$layout$layout$PANEL))
  if (!is.null(order)) {
    if (is.numeric(order)){
      order <- unique(order)
      omit_blank <- -1 %in% order
      order <- order[order != -1]
      order <- order[order <= n_panels] # ignore numbers beyond total of panels
      if (length(order)==0) {
        order <- 1:n_panels
      }
    } else {
      rlang::warn("Argument 'order' is not a numeric vector and will be ignored.")
    }  
  } else {
    order <- 1:n_panels
  }


  what = what <- rlang::arg_match(what)

  if (what=="data") {
    plot_list <- reveal_panels_onlydata(p, order, omit_blank)
  } else {
    plot_list <- reveal_panels_everything(p, order, omit_blank, axis = T, label = T)
  }

  return(plot_list)
}
