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


  # Check arguments
  "ggplot" %in% class(p) || rlang::abort(paste(deparse(substitute(p)),
                                               "is not a ggplot object"))
  
  axis <- rlang::arg_match(axis)

  omit_blank <- FALSE
  if (!is.null(order)) {
    if (is.numeric(order)){
      order <- unique(order)
      omit_blank <- -1 %in% order
      order <- order[order != -1]
      if (length(order)==0) {
        order <- NULL
      }
    } else {
      rlang::warn("Argument 'order' is not a numeric vector and will be ignored.")
    }  
  } 

  p_build <- ggplot2::ggplot_build(p)

  # Check if axis is defined more than once, starting eith the maain fucntion call
  search_list <- list(p)
  search_list <- append(search_list, p$layers)
  def <- sapply(search_list, function(x) {"quosure" %in% class(x$mapping[[axis]])})
  # Stop if:
  # There's more than one x (or y) aes (e.g. in main call and in a layer)
  # There are no x (or y) aes
  if(sum(def) > 1) {
    cli::cli_abort("It seems that the definition of axis '{axis}' varies across layers. Please use reveal_layers() instead.")
  } else if (sum(def) == 0){
    cli::cli_abort("aes '{axis}' is not defined in any layer.")
  }
  axis_var_name <- rlang::quo_name(search_list[def][[1]]$mapping[[axis]])

  # Check whether axis is a factor variable
  # TODO: If numeric, perhaps allow for a maximum number of unique values
  "mapped_discrete" %in% class(p_build$data[[1]][, axis]) || cli::cli_abort("'{axis_var_name}' is not a discrete variable.")

  # Note: gets axis values from all layers
  axis_values_all <- sort(unique(unlist(lapply(p_build$data, function(x) unique(x[, axis])))))

  # if (length(groups_all) <= 1 ){
  #   rlang::warn("Plot is not grouped or there is only one group. Maybe use reveal_panels or reveal_layers?")
  # } else if (!any(explicit)){
  #   rlang::inform("Plot does not explicitly define a group aesthetic. Using default grouping set by ggplot2.")
  # }

  # Reorder group levels
  if (!is.null(order)) {
    axis_values_all <- axis_values_all[order]
  }

  increment <- c()
  plot_list <- list()

  # Make step and append
  if (!omit_blank) {
    p_step <- make_step(p, p_build, axis, increment)
    plot_list <- append(plot_list, list(p_step))
  }


  for (i in seq_along(axis_values_all)) {

    increment <- c(increment,  axis_values_all[i])

    # Make step and append
    p_step <- make_step(p, p_build, axis, increment)
    plot_list <- append(plot_list, list(p_step))

  }
  attr(plot_list, "omit_blank") <- omit_blank
  return(plot_list)

}

#' @export
#' @rdname reveal_axis
reveal_x <- function(p, order = NULL){
  plot_list <- reveal_axis(p, order, "x")
  return(plot_list)
}

#' @export
#' @rdname reveal_axis
reveal_y <- function(p, order = NULL){
  plot_list <- reveal_axis(p, order, "y")
  return(plot_list)
}



