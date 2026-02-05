#' Reveal plots in a patchwork object
#'
#' Turns a patchwork into a list of plots that reveal each child plot (including
#' nested patchworks) one-by-one. The function handles arbitrary nesting levels.
#' 
#' @export
reveal_patchwork <- function(p, order = NULL){

  # Check arguments
  "patchwork" %in% class(p) || cli::cli_abort("{deparse(substitute(p))} is not a patchwork object")

  # Collect all plot paths (handles nested patchworks)
  path_info <- collect_all_plot_paths(p)
  plot_paths <- path_info$plot_paths
  top_plot_info <- path_info$top_plot_info
  
  # Add top plot path to list if it exists
  if (!is.null(top_plot_info)) {
    plot_paths <- c(plot_paths, list(top_plot_info$path))
  }

  # Handle order argument (same pattern as reveal_aes and reveal_panels)
  omit_blank <- FALSE
  if (!is.null(order)) {
    if (is.numeric(order)) {
      order <- unique(order)
      omit_blank <- -1 %in% order
      order <- order[order != -1]
      order <- order[order <= length(plot_paths)]
      if (length(order) == 0) {
        order <- seq_len(length(plot_paths))
      }
    } else {
      cli::cli_warn("Argument 'order' is not a numeric vector and will be ignored.")
      order <- seq_len(length(plot_paths))
    }
    plot_paths <- plot_paths[order]
  }

  # Convert patchwork to gtable
  patchwork_gtable <- patchwork::patchworkGrob(p)

  plot_list <- list()

  # Add blank first step unless omit_blank is TRUE
  if (!omit_blank) {
    gtable_blank <- hide_all_plots_except(patchwork_gtable, list(), top_plot_info)
    plot_list <- append(plot_list, list(ggplotify::as.ggplot(gtable_blank)))
  }

  # Build incremental steps
  for (step_index in seq_len(length(plot_paths))) {
    paths_to_reveal <- plot_paths[seq_len(step_index)]
    gtable_step <- hide_all_plots_except(patchwork_gtable, paths_to_reveal, top_plot_info)
    plot_list <- append(plot_list, list(ggplotify::as.ggplot(gtable_step)))
  }

  if (omit_blank) {
    attr(plot_list, "omit_blank") <- omit_blank
  }
  
  return(plot_list)
}



# Collect all plot paths in nested patchwork
# Each path is an integer vector of child indices at each nesting level
# Returns list with plot_paths and top_plot_info
collect_all_plot_paths <- function(patchwork_obj) {
  
  plot_paths <- list()
  
  if (!is.null(patchwork_obj$patches) && !is.null(patchwork_obj$patches$plots)) {
    for (child_index in seq_along(patchwork_obj$patches$plots)) {
      child_plot <- patchwork_obj$patches$plots[[child_index]]
      
      # Check if child is a nested patchwork
      if (!is.null(child_plot$patches) && !is.null(child_plot$patches$plots)) {
        nested_info <- collect_all_plot_paths(child_plot)
        
        # Prepend current child_index to each nested path
        for (nested_path in nested_info$plot_paths) {
          plot_paths <- append(plot_paths, list(c(child_index, nested_path)))
        }
      } else {
        # Regular plot - simple path
        plot_paths <- append(plot_paths, list(c(child_index)))
      }
    }
  }
  
  top_plot_info <- find_top_plot_info(patchwork_obj, length(plot_paths))
  
  return(list(plot_paths = plot_paths, top_plot_info = top_plot_info))
}

# Find the "top plot" (the last plot added to the patchwork)
# Returns NULL if no top plot, or list with path and child_index
find_top_plot_info <- function(patchwork_obj, num_children, current_path = integer(0)) {
  
  # Check if patchwork_obj itself has layers
  if ("patchwork" %in% class(patchwork_obj) && length(patchwork_obj@layers) > 0) {
    # Top plot appears after all children in the gtable
    return(list(
      path = current_path,
      child_index = num_children + 1
    ))
  }
  
  # Recurse through children to find top plot
  if (!is.null(patchwork_obj$patches) && !is.null(patchwork_obj$patches$plots)) {
    for (child_index in seq_along(patchwork_obj$patches$plots)) {
      child_plot <- patchwork_obj$patches$plots[[child_index]]
      
      if ("patchwork" %in% class(child_plot)) {
        # Count children at this level
        child_num_children <- if (!is.null(child_plot$patches) && !is.null(child_plot$patches$plots)) {
          length(child_plot$patches$plots)
        } else {
          0
        }
        
        result <- find_top_plot_info(child_plot, child_num_children, c(current_path, child_index))
        if (!is.null(result)) {
          return(result)
        }
      }
    }
  }
  
  return(NULL)
}

# Hide all plots in gtable except those in paths_to_reveal
hide_all_plots_except <- function(gtable_obj, paths_to_reveal, top_plot_info = NULL, current_path = integer(0)) {
  
  # which child indices to keep at this nesting level?
  child_indices_to_keep <- integer(0)
  if (length(paths_to_reveal) > 0) {
    child_indices_to_keep <- unique(vapply(
      paths_to_reveal, 
      function(path) {
        if (length(path) > 0) as.integer(path[1]) else NA_integer_
      }, 
      integer(1)
    ))
    child_indices_to_keep <- child_indices_to_keep[!is.na(child_indices_to_keep)]
  }
  
  # Check if we're at the top plot level and should reveal it
  at_top_plot_level <- !is.null(top_plot_info) && 
                       length(top_plot_info$path) == length(current_path) &&
                       (length(current_path) == 0 || all(top_plot_info$path == current_path))
  
  reveal_top_plot <- FALSE
  reveal_top_plot_only <- FALSE
  top_plot_index <- NULL
  
  if (at_top_plot_level) {
    top_plot_index <- top_plot_info$child_index
    
    # Check if any path in paths_to_reveal is empty (reveals top plot)
    has_empty_path <- any(vapply(paths_to_reveal, function(path) length(path) == 0, logical(1)))
    
    if (has_empty_path) {
      reveal_top_plot <- TRUE
      
      # If onky the empty path exists, reveal only the top plot
      if (length(paths_to_reveal) == 1) {
        reveal_top_plot_only <- TRUE
        child_indices_to_keep <- top_plot_index
      } else {
        child_indices_to_keep <- unique(c(child_indices_to_keep, top_plot_index))
      }
    }
  }

  # Zero out grobs not in child_indices_to_keep
  for (grob_index in seq_len(length(gtable_obj))) {
    grob_name <- gtable_obj$layout[grob_index, "name"]
    
    # extrat trailing number suffix (e.g., "panel-1-2" -> 2)
    number_match <- stringr::str_extract(grob_name, "-(\\d+)$")
    
    if (!is.na(number_match)) {
      child_index <- as.integer(stringr::str_remove(number_match, "^-"))
      
      if (!(child_index %in% child_indices_to_keep)) {
        gtable_obj$grobs[[grob_index]] <- ggplot2::zeroGrob()
      }
    }
  }

  #process nested gtables (unless revealing top plot only)
  if (length(child_indices_to_keep) > 0 && !reveal_top_plot_only) {
    for (child_index in child_indices_to_keep) {
      
      # Get paths that nested in this child
      deeper_paths <- lapply(paths_to_reveal, function(path) {
        if (length(path) > 0 && path[1] == child_index) {
          path[-1]
        } else {
          NULL
        }
      })
      deeper_paths <- Filter(Negate(is.null), deeper_paths)
      
      if (length(deeper_paths) == 0) next

      # Find nested gtable for this child
      nested_gtable_index <- NULL
      for (grob_index in seq_len(length(gtable_obj))) {
        grob_name <- gtable_obj$layout[grob_index, "name"]
        
        if (stringr::str_detect(grob_name, paste0("-", child_index, "$"))) {
          if ("gtable_patchwork" %in% class(gtable_obj$grobs[[grob_index]])) {
            nested_gtable_index <- grob_index
            break
          }
        }
      }
      
      if (!is.null(nested_gtable_index)) {
        nested_gtable <- gtable_obj$grobs[[nested_gtable_index]]
        nested_gtable_modified <- hide_all_plots_except(
          nested_gtable, 
          deeper_paths, 
          top_plot_info, 
          c(current_path, child_index)
        )
        gtable_obj$grobs[[nested_gtable_index]] <- nested_gtable_modified
      }
    }
  }

  return(gtable_obj)
}