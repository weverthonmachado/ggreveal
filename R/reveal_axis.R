#' Reveal plot by axis
#'
#' @export
reveal_axis <- function(p, order = NULL, which = c("x", "y")){


  # Check arguments
  "ggplot" %in% class(p) || rlang::abort(paste(deparse(substitute(p)),
                                               "is not a ggplot object"))
  
  which <- rla

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

  # Check whether x (or y) is a factor variable
  # Also check whether it's the same in all layers. Maybe restrict to higher level call
  # If numeric, perhaps allow for a maximum number of unique values
  "mapped_discrete" %in% class(p_build$data[[1]]$x) || rlang::abort("Not a discrete var")

  # Note: gets group levels from all layers
  # TODO: make it flexible to get either x or y
  groups_all <- sort(unique(unlist(lapply(p_build$data, function(x) unique(x$x)))))

  # if (length(groups_all) <= 1 ){
  #   rlang::warn("Plot is not grouped or there is only one group. Maybe use reveal_panels or reveal_layers?")
  # } else if (!any(explicit)){
  #   rlang::inform("Plot does not explicitly define a group aesthetic. Using default grouping set by ggplot2.")
  # }

  # Reorder group levels
  if (!is.null(order)) {
    groups_all <- groups_all[order]
  }

  increment <- c()
  plot_list <- list()

  # Make step and append
  if (!omit_blank) {
    p_step <- make_step(p, p_build, which, increment)
    plot_list <- append(plot_list, list(p_step))
  }


  for (i in seq_along(groups_all)) {

    increment <- c(increment,  groups_all[i])

    # Make step and append
    p_step <- make_step(p, p_build, which, increment)
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



