reveal_patchwork <- function(p){

  reveal_patch(p)

}


#' @noRd
reveal_patch <- function(p, omit_blank=T, return_gt = F){

  rlang::check_installed("patchwork", reason = "to use this function")

  annot <- c()
  recurse_tags <- utils::getFromNamespace('recurse_tags', 'patchwork')
  get_patches <- utils::getFromNamespace('get_patches', 'patchwork')
  build_patchwork <- utils::getFromNamespace('build_patchwork', 'patchwork')
  annotate_table <- utils::getFromNamespace('annotate_table', 'patchwork')

  if ("gtable_patchwork" %in% class(p)){

    p_gt <- p

  } else {

    if (!is.null(p$patches$annotation)){

      annotation <-  p$patches$annotation 
      p <- recurse_tags(p, annotation$tag_levels, annotation$tag_prefix,
                         annotation$tag_suffix, annotation$tag_sep)$patches
    }
    
    plot <- get_patches(p)
    p_gt <- build_patchwork(plot) 
   
    if (!is.null(p$patches$annotation)){

      p_gt <- annotate_table(p_gt, plot$annotation)
      annot <- c("title", "subtitle", "caption")

    } 
    
  }

  is_zerogrob <- unlist(lapply(p_gt$grobs, function(x) "zeroGrob" %in% class(x)))
  layout_obj <- p_gt$layout[!is_zerogrob, ]

  patches <- select_sort_elements(layout_obj, "patch")
  panels <- select_sort_elements(layout_obj, "panel")
  labels <- select_sort_elements(layout_obj, "axis|xlab|ylab|title|strip|tag")
  rest <- layout_obj$name[!(stringr::str_detect(layout_obj$name, 
                                                "panel|axis|xlab|ylab|title|strip|tag|patchwork-table|inset|full"))]
  rest <- c(rest, annot)

  panels_increment <- list(rest)
  plot_list <- list()

  # Make step and append
  if (!omit_blank & !return_gt){
    p_step <- make_step_by_panel_everything(p_gt, panels_increment)
    plot_list <- append(plot_list, list(p_step))
  }
  
  panel_counter = 0

  for (i in seq_along(patches)) {

    if (stringr::str_detect(patches[i], "panel")){

      panel_counter = panel_counter + 1

      panels_increment <- append(panels_increment,
                                list(c(panels[panel_counter], 
                                        labels[[panel_counter]])))
      # Make step and append
      p_step <- make_step_by_panel_everything(p_gt, panels_increment, return_gt = return_gt)
      plot_list <- append(plot_list, list(p_step))


    } else if (stringr::str_detect(patches[i], "patchwork-table")){
      
      patch_table <- p_gt$grobs[p_gt$layout$name==patches[i]][[1]]
      patch_grobs <- reveal_patch(patch_table, omit_blank = T, return_gt = T)

      panels_increment <- append(panels_increment,
                                  list(patches[i]))
      
      for (j in seq_along(patch_grobs)){

        p_step <- make_step_by_panel_everything(p_gt, panels_increment, return_gt = T)
        p_step$grobs[p_step$layout$name==patches[i]][[1]] <- patch_grobs[[j]]
        p_step <- ggplotify::as.ggplot(p_step)
        plot_list <- append(plot_list, list(p_step))
      }

    } else if (stringr::str_detect(patches[i], "inset|full")){
      
      panels_increment <- append(panels_increment,
                                   list(patches[i]))
      # Make step and append
      p_step <- make_step_by_panel_everything(p_gt, panels_increment, return_gt = return_gt)
      plot_list <- append(plot_list, list(p_step))
  
    }

  }
  if (omit_blank) {
    attr(plot_list, "omit_blank") <- omit_blank
  }
  return(plot_list)

}
