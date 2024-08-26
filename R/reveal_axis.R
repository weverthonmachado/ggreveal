#' Reveal plot by axis
#'
#' Turns a ggplot into a list of plots, showing data incrementally by the
#' categories in the x or y axis. 
#'
#' @param p A ggplot2 object
#' @param order (optional) A numeric vector specifying in which order to reveal the categories
#'   
#'   For example, if there are three categories in the axis, `order = c(3, 2,
#'   1)` will invert the order in which they are revealed. 
#' 
#'   Any category not included in the vector will be omitted from the incremental
#'   plots. E.g.: with `order = c(3, 1)`, the second category is not shown.
#' 
#'   By default, the first plot is blank, showing layout elements (title,
#'   legends, axes, etc) but no data. To omit the blank plot, include `-1`: e.g. 
#'   `order = c(-1, 3, 1)`, or `order = -1`.
#' 
#' @return A list of ggplot2 objects, which can be passed to [reveal_save()]
#' @export
#' @rdname reveal_axis
#' @examples
#' # Create full plot
#' library(ggplot2)
#' data("mtcars")
#'
#' p <- mtcars |>
#'   ggplot(aes(factor(vs), 
#'              color = gear,
#'              fill= gear, 
#'              group = gear)) +
#'   geom_bar() +
#'   facet_wrap(~am)
#' p
#'
#' plot_list <- reveal_x(p)
#' plot_list[[1]]
#' plot_list[[2]]
#' plot_list[[3]]
#'
#' # Save plots
#' reveal_save(plot_list, "myplot.png", width = 8, height = 4, path = tempdir())
#' 
#' # Clean temp files
#' file.remove(list.files(path = tempdir(), pattern = "myplot", full.names = TRUE)) 
reveal_x <- function(p, order = NULL){

  reveal_aes(p, "x", order)

}

#' @export
#' @rdname reveal_axis
reveal_y <- function(p, order = NULL){

  reveal_aes(p, "y", order)

}

#' @noRd
reveal_axis <- function(p, order = NULL, axis = c("x", "y")){

  axis <- rlang::arg_match(axis)
  reveal_aes(p, axis, order)

}


