#' @noRd
reveal_panels_everything <- function(p, order, omit_blank, axis = F, label = F){

  p_gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))
  is_zerogrob <- unlist(lapply(p_gt$grobs, function(x) "zeroGrob" %in% class(x)))
  layout_obj <- p_gt$layout[!is_zerogrob, ]

  panels <- select_sort_elements(layout_obj, "panel")[order]
  strips <- select_sort_elements(layout_obj, "strip")[order]
  axes <- select_sort_elements(layout_obj, "axis")[order]
  rest <- layout_obj$name[!(stringr::str_detect(layout_obj$name, "panel|strip|axis"))]

  panels_increment <- list(rest)
  plot_list <- list()

  # Handle facet titles (strips)
  if (label) {
    strip_list <- strips
  } else {
    strip_list <- as.list(rep("", length(panels)))
    strip_list[[1]] <- unlist(strips)
  }


  # Handle axes
  if (axis) {
    axes_list <- axes
  } else {
    axes_list <- as.list(rep("", length(panels)))
    axes_list[[1]] <- unlist(axes)
  }

  # Make step and append
  if (!omit_blank){
    p_step <- make_step_by_panel_everything(p_gt, panels_increment)
    plot_list <- append(plot_list, list(p_step))
  }


  for (i in seq_along(panels)) {

    panels_increment <- append(panels_increment,
                               list(c(panels[i], strip_list[[i]], axes_list[[i]])))

    # Make step and append
    p_step <- make_step_by_panel_everything(p_gt, panels_increment)
    plot_list <- append(plot_list, list(p_step))

  }
  if (omit_blank) {
    attr(plot_list, "omit_blank") <- omit_blank
  }
  return(plot_list)

}


#' @noRd
make_step_by_panel_everything <- function(p_gt, panels_increment, show_layout = F){

  drop <- !(p_gt$layout$name %in% unlist(panels_increment))
  gt_step <- p_gt
  gt_step$grobs[drop] <- NULL
  gt_step$layout <- gt_step$layout[!drop, ]

  p_step <- ggplotify::as.ggplot(gt_step)

  if (show_layout) {
    lemon::gtable_show_names(gt_step)
  }

  return(p_step)

}

#' @noRd
#' @importFrom rlang .data
select_sort_elements <- function(layout_obj,
                                 element=c("panel","axis", "strip")) {

  element <- rlang::arg_match(element)

  panel_df <- layout_obj
  panel_df <- dplyr::filter(panel_df, stringr::str_detect(.data$name, "panel"))
  panel_df <- dplyr::rename_all(panel_df, ~paste0("panel_", .))


  if (element=="panel"){

    # Sort panels by row,  using t and l coordinates
    # (To sort by col, just do by l then t)
    out <-  dplyr::arrange(panel_df, .data$panel_t, .data$panel_l)
    out <-  dplyr::pull(out, .data$panel_name)


  } else {


    element_df <- layout_obj
    element_df <-  dplyr::filter(element_df, stringr::str_detect(.data$name, element))
    element_df <-  dplyr::rename_all(element_df, ~paste0("element_", .))

    n_panels <- NROW(panel_df)

    element_df_list <- list()
    for (i in 1:n_panels) {
      element_df_list <- append(element_df_list, list(element_df))
    }

    panel_element_df <- panel_df
    panel_element_df <- dplyr::mutate(panel_element_df, elements = element_df_list)
    panel_element_df <- tidyr::unnest(panel_element_df, cols = "elements")

    # Get the closest axis/strip by letter (t, b, l, r)
    # Closest =  minimum sum of t and l coordinates
    v <- panel_element_df
    v <- dplyr::mutate(v,
                        letter = stringr::str_extract(.data$element_name, "\\w(?=-\\d)"),
                        dist_t = abs(.data$panel_t - .data$element_t),
                        dist_l = abs(.data$panel_l - .data$element_l),
                        dist_sum = .data$dist_t + .data$dist_l)
    v <- dplyr::group_by(v,
                          .data$panel_name, .data$letter)
    v <-  dplyr::arrange(v,
                          .data$panel_t, .data$panel_l, .data$letter, .data$dist_sum)
    v <- dplyr::filter(v,
                        dplyr::row_number()==1)
    v <- dplyr::pull(v, .data$element_name)

    # Split vector into list
    out <- split(v, ceiling(seq_along(v) / (length(v)/n_panels)))
    length(out)==n_panels || rlang::abort("length(out) != length(n_panels)")
  }

  return(out)

}
