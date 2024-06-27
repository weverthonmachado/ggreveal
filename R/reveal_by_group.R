reveal_by_group <- function(p){

  # Check arguments
  "ggplot" %in% class(p) || rlang::abort(paste(deparse(substitute(p)),
                                               "is not a ggplot object"))

  p_build <- ggplot2::ggplot_build(p)

  # Note: gets group levels from first layer
  groups_all <- sort(unique(p_build$data[[1]]$group))
  groups_increment <- c()
  plot_list <- list()

  # Make step and append
  p_step <- make_step_by_group(p, p_build, groups_increment)
  plot_list <- append(plot_list, list(p_step))


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


