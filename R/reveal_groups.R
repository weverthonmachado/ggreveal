#' Reveal plot by group
#'
#' Creates a list of plots, showing data incrementally by groups.
#'
#' @param p A ggplot2 object
#' @param order (optional) A numeric vector specifying in which order to reveal the groups. 
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
#' plot_list <- reveal_groups(p)
#' plot_list[[1]]
#' plot_list[[2]]
#' plot_list[[3]]
#'
#'\dontrun{
#' # Save plots
#' reveal_save(plot_list, "myplot.png", width = 8, height = 4)
#' }
reveal_groups <- function(p, order = NULL){

  reveal_aes(p, "group", order)
  
}

