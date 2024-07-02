#' @noRd
reveal_panels_onlydata <- function(p, order, omit_blank) {

  p_build <- ggplot2::ggplot_build(p)

  panels_all <- sort(unique(p_build$layout$layout$PANEL))[order]
  panels_increment <- c()
  plot_list <- list()

  # Make step and append
  if (!omit_blank){
    p_step <- make_step_by_panel_onlydata(p, p_build, panels_increment)
    plot_list <- append(plot_list, list(p_step))
  }


  for (i in seq_along(panels_all)) {

    panels_increment <- c(panels_increment,  panels_all[i])

    # Make step and append
    p_step <- make_step_by_panel_onlydata(p, p_build, panels_increment)
    plot_list <- append(plot_list, list(p_step))

  }
  attr(plot_list, "omit_blank") <- omit_blank
  return(plot_list)

  }



#' @noRd
make_step_by_panel_onlydata <- function(p, p_build, panels_increment){

  p_step <- p
  p_step <- ggplot2::ggplot_build(p_step)

  for (d in seq_along(p_step$data)) {
    filter <- p_step$data[[d]]$PANEL %in% panels_increment
    p_step$data[[d]] <- p_step$data[[d]][filter,]
  }

  p_step$layout <- p_build$layout
  p_step$plot$guides <- p_build$plot$guides
  p_step$plot$scales <- p_build$plot$scales
  p_step <- ggplotify::as.ggplot(ggplot2::ggplot_gtable(p_step))

  return(p_step)
}


