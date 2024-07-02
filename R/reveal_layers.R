#' Reveal plot by layer
#'
#' Creates a list of plots, showing data incrementally by layers.
#'
#' @param p A ggplot2 object, in which a `group` aesthetic is used.
#' @param order (optional) A numeric vector specifying in which order to reveal the layers.  
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
#' plot_list <- reveal_layers(p)
#' plot_list[[1]]
#' plot_list[[2]]
#' plot_list[[3]]
#'
#'\dontrun{
#' # Save plots
#' reveal_save(plot_list, "myplot", width = 8, height = 4)
#' }
reveal_layers <- function(p, order = NULL){

  # Check arguments
  "ggplot" %in% class(p) || rlang::abort(paste(deparse(substitute(p)),
                                               "is not a ggplot object"))
  
    omit_blank <- FALSE
    if (!is.null(order)) {
      if (is.numeric(order)){
        order <- unique(order)
        omit_blank <- -1 %in% order
        order <- order[order != -1]
        order <- order[order <= length(p$layers)] # ignore numbers beyond total of layers
        if (length(order)==0) {
          order <- 1:length(p$layers)
        }
      } else {
        rlang::warn("Argument 'order' is not a numeric vector and will be ignored.")
      }  
    } else {
      order <- 1:length(p$layers)
    }

  p_build <- ggplot2::ggplot_build(p)

  layers_all <- p$layers
  layers_increment <- list()
  plot_list <- list()

  # Make step and append
  if (!omit_blank) {
    p_step <- make_step_by_layer(p, p_build, layers_increment)
    plot_list <- append(plot_list, list(p_step))
  }

  for (i in order) {

    layers_increment <- append(layers_increment, layers_all[i])

    # Make step and append
    p_step <- make_step_by_layer(p, p_build, layers_increment)
    plot_list <- append(plot_list, list(p_step))


  }
  attr(plot_list, "omit_blank") <- omit_blank
  return(plot_list)

}



#' @noRd
make_step_by_layer <- function(p, p_build, layers_increment){

  p_step <- p
  p_step$layers <- layers_increment
  p_step <- ggplot2::ggplot_build(p_step)
  p_step$layout <- p_build$layout
  p_step$plot$guides <- p_build$plot$guides
  p_step$plot$scales <- p_build$plot$scales
  p_step <- ggplotify::as.ggplot(ggplot2::ggplot_gtable(p_step))

}
