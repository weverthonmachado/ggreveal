#' Reveal plot by axis
#'
#' Creates a list of plots, showing data incrementally by categories in the x or y axis. The specified axis must 
#' must be mapped to a discrete variable. `reveal_x()` and `reveal_y()` are useful wrappers that call the main function 
#' with the axis specified. 
#'
#' @param p A ggplot2 object
#' @param order (optional) A numeric vector specifying in which order to reveal the categories
#' @param axis (optional) "x" or "y", to specify the axis to be revealed. "x" is the default
#' @return A list of ggplot2 objects, which can be passed to [reveal_save()]
#' @export
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
#'\dontrun{
#' # Save plots
#' reveal_save(plot_list, "myplot.png", width = 8, height = 4)
#' }
reveal_axis <- function(p, order = NULL, axis = c("x", "y")){

  axis <- rlang::arg_match(axis)
  reveal_aes(p, axis, order)

}

#' @export
#' @rdname reveal_axis
reveal_x <- function(p, order = NULL){

  reveal_aes(p, "x", order)

}

#' @export
#' @rdname reveal_axis
reveal_y <- function(p, order = NULL){

  reveal_aes(p, "y", order)

}



