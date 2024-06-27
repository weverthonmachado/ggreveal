reveal_by_facet_everything <- function(p, axis = F, label = F){

  p_gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))
  layout_obj <- p_gt$layout
  type_facet <- stringr::str_extract(tolower(class(p$facet)[1]), "grid|wrap")

  panels <- select_sort_elements(layout_obj, "panel")
  strips <- select_sort_elements(layout_obj, "strip", type_facet)
  axes <- select_sort_elements(layout_obj, "axis", type_facet)
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
  p_step <- make_step_by_facet_everything(p_gt, panels_increment)
  plot_list <- append(plot_list, list(p_step))


  for (i in seq_along(panels)) {

    panels_increment <- append(panels_increment,
                               list(c(panels[i], strip_list[[i]], axes_list[[i]])))

    # Make step and append
    p_step <- make_step_by_facet_everything(p_gt, panels_increment)
    plot_list <- append(plot_list, list(p_step))

  }

  return(plot_list)

}



make_step_by_facet_everything <- function(p_gt, panels_increment, show_layout = F){

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


select_sort_elements <- function(layout_obj,
                                 element=c("panel","axis", "strip"),
                                 type_facet = c("wrap", "grid")) {

  element <- rlang::arg_match(element)
  type_facet <- rlang::arg_match(type_facet)

  panel_df <- layout_obj
  panel_df <- dplyr::filter(panel_df, stringr::str_detect(name, "panel"))
  panel_df <- dplyr::rename_all(panel_df, ~paste0("panel_", .))


  if (element=="panel"){

    # Sort panels by row,  using t and l coordinates
    # (To sort by col, just do by l then t)
    out <-  dplyr::arrange(panel_df, panel_t, panel_l)
    out <-  dplyr::pull(out, panel_name)


  } else {


    element_df <- layout_obj
    element_df <-  dplyr::filter(element_df, stringr::str_detect(name, element))
    element_df <-  dplyr::rename_all(element_df, ~paste0("element_", .))

    n_panels <- NROW(panel_df)

    element_df_list <- list()
    for (i in 1:n_panels) {
      element_df_list <- append(element_df_list, list(element_df))
    }


    panel_element_df <- panel_df
    panel_element_df <- dplyr::mutate(panel_element_df, elements = element_df_list)
    panel_element_df <- tidyr::unnest(panel_element_df, cols = elements)


    if (type_facet=="grid"){
      # If facet_grid, get the closest axis/strip by letter (t, b, l, r)
      # Closest =  minimum sum of t and l coordinates
      v <- panel_element_df
      v <- dplyr::mutate(v,
                         letter = stringr::str_extract(element_name, "\\w(?=-\\d)"),
                         dist_t = abs(panel_t-element_t),
                         dist_l = abs(panel_l-element_l),
                         dist_sum = dist_t+dist_l)
      v <- dplyr::group_by(v,
                           panel_name, letter)
      v <-  dplyr::arrange(v,
                          panel_t, panel_l, letter, dist_sum)
      v <- dplyr::filter(v,
                         dplyr::row_number()==1)
      v <- dplyr::pull(v,element_name)


    } else if (type_facet=="wrap"){
      # If facet_wrap, get a number of closest axes/strips
      # Closest =  minimum sum of t and l coordinates

      # How many to get per panel?
      n_elements <- NROW(element_df)/n_panels

      v <- panel_element_df
      v <- dplyr::mutate(v,
                          dist_t = abs(panel_t-element_t),
                          dist_l = abs(panel_l-element_l),
                          dist_sum = dist_t+dist_l)
      v <- dplyr::group_by(v, panel_name)
      v <- dplyr::arrange(v, panel_t, panel_l,dist_sum)
      v <- dplyr::filter(v, dplyr::row_number() <= n_elements)
      v <- dplyr::pull(v, element_name)

    }

    # Split vector into list
    out <- split(v, ceiling(seq_along(v) / (length(v)/n_panels)))
    length(out)==n_panels || rlang::abort("length(out) != length(n_panels)")
  }

  return(out)

}
