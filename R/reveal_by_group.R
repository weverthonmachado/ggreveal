#' Reveal plot by group
#'
#' Creates a list of plots, showing data incrementally by groups.
#'
#' @param p A ggplot2 object, in which a `group` aesthetic is used.
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
#' plot_list <- reveal_by_group(p)
#' plot_list[[1]]
#' plot_list[[2]]
#' plot_list[[3]]
#'
#'\dontrun{
#' # Save plots
#' reveal_save(plot_list, "myplot", width = 8, height = 4)
#' }
reveal_by_group <- function(p, order = NULL){


  # Check arguments
  "ggplot" %in% class(p) || rlang::abort(paste(deparse(substitute(p)),
                                               "is not a ggplot object"))
  
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

  # Check if there is explicit grouping, starting with th e main function call,
  # then for each layer.
  search_list <- list(p)
  search_list <- append(search_list, p$layers)
  explicit <- sapply(search_list, function(x) {"quosure" %in% class(x$mapping$group)})
  # Stop if:
  # There's more than one group aes (e.g. in main call an in a layer) OR
  # There's only one group aes, which is not defined in the main call AND the plot
  # has more than one layer. (If only layer, the group main call will not matter)
  if(sum(explicit) > 1 | (sum(explicit)==1 & !explicit[1] & length(p$layers)>1)) {
    rlang::abort("It seems that the groups differ across layers. Please use reveal_by_layer() instead.")
  }

  p_build <- ggplot2::ggplot_build(p)

  # Note: gets group levels from all layers
  groups_all <- sort(unique(unlist(lapply(p_build$data, function(x) unique(x$group)))))

  if (length(groups_all) <= 1 ){
    rlang::warn("Plot is not grouped or there is only one group. Maybe use reveal_by_panel or reveal_by_layer?")
  } else if (!any(explicit)){
    rlang::inform("Plot does not explicitly define a group aesthetic. Using default grouping set by ggplot2.")
  }

  # Reorder group levels
  if (!is.null(order)) {
    groups_all <- groups_all[order]
  }

  groups_increment <- c()
  plot_list <- list()

  # Make step and append
  if (!omit_blank) {
    p_step <- make_step_by_group(p, p_build, groups_increment)
    plot_list <- append(plot_list, list(p_step))
  }


  for (i in seq_along(groups_all)) {

    groups_increment <- c(groups_increment,  groups_all[i])

    # Make step and append
    p_step <- make_step_by_group(p, p_build, groups_increment)
    plot_list <- append(plot_list, list(p_step))

  }

  return(plot_list)

}



#' @noRd
make_step_by_group <- function(p, p_build, groups_increment){

  p_step <- p
  p_step <- ggplot2::ggplot_build(p_step)

  for (d in seq_along(p_step$data)) {
    filter <- p_step$data[[d]]$group %in% groups_increment
    p_step$data[[d]] <- p_step$data[[d]][filter,]
  }

  p_step$layout <- p_build$layout
  p_step$plot$guides <- p_build$plot$guides
  p_step$plot$scales <- p_build$plot$scales
  p_step <- ggplotify::as.ggplot(ggplot2::ggplot_gtable(p_step))

  return(p_step)
}


