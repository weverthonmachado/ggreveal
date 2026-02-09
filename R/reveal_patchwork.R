#' Reveal plots from a patchwork object
#'
#' Turns a [patchwork][patchwork::patchwork] into a list of plots that reveal each child plot incrementally. 
#' Also works with nesting and insets.
#' 
#' @param pw A patchwork object
#' @param order (optional) A numeric vector specifying in which order to reveal the plots
#'   
#'   For example, if there are three plots in the patchwork, `order = c(3, 2, 1)` will invert the 
#'   order in which they are revealed. 
#' 
#'   Any plot not included in the vector will be omitted from the incremental
#'   plots. E.g.: with `order = c(3, 1)`, the second plot is not shown.
#' 
#'   By default, the first plot returned by this function is blank, showing layout elements
#'   of the patchwork but none of its child plots. To omit the blank plot, include `-1`: e.g. 
#'   `order = c(-1, 3, 1)`, or `order = -1`.
#'   
#' @return A list of ggplot2 objects
#' @export
reveal_patchwork <- function(pw, order = NULL){

  # Check arguments
  "patchwork" %in% class(pw) || cli::cli_abort("{deparse(substitute(pw))} is not a patchwork object")

  # Collect all plot paths (handles nested patchworks)
  path_info <- collect_all_plot_paths(pw)
  plot_paths <- path_info$plot_paths
  all_top_plots <- path_info$all_top_plots
  
  # Add root-level top plot path if it exists
  if (length(all_top_plots) > 0) {
    # Find the top plot at root level (empty path)
    for (top_plot in all_top_plots) {
      if (length(top_plot$path) == 0) {
        plot_paths <- c(plot_paths, list(integer(0)))
        break
      }
    }
  }

  # Handle order argument 
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
  patchwork_gtable <- patchwork::patchworkGrob(pw)

  plot_list <- list()

  # Add blank first step unless omit_blank is TRUE
  if (!omit_blank) {
    gtable_blank <- hide_all_plots_except(patchwork_gtable, list(), all_top_plots)
    plot_list <- append(plot_list, list(ggplotify::as.ggplot(gtable_blank)))
  }

  # Build incremental steps
  for (step_index in seq_len(length(plot_paths))) {
    paths_to_reveal <- plot_paths[seq_len(step_index)]
    gtable_step <- hide_all_plots_except(patchwork_gtable, paths_to_reveal, all_top_plots)
    plot_list <- append(plot_list, list(ggplotify::as.ggplot(gtable_step)))
  }

  if (omit_blank) {
    attr(plot_list, "omit_blank") <- omit_blank
  }
  
  return(plot_list)
}



# Collect all plot paths in nested patchwork
# Each path is an integer vector of child indices at each nesting level
# Returns list with plot_paths and all_top_plots
collect_all_plot_paths <- function(patchwork_obj) {
  
  plot_paths <- list()
  all_top_plots <- list()
  
  # Count how many direct children this patchwork has (not nested paths)
  num_direct_children <- if (!is.null(patchwork_obj$patches) && !is.null(patchwork_obj$patches$plots)) {
    length(patchwork_obj$patches$plots)
  } else {
    0
  }
  
  if (num_direct_children > 0) {
    for (child_index in seq_along(patchwork_obj$patches$plots)) {
      child_plot <- patchwork_obj$patches$plots[[child_index]]
      
      # Check if child is a nested patchwork
      if (!is.null(child_plot$patches) && !is.null(child_plot$patches$plots)) {
        nested_info <- collect_all_plot_paths(child_plot)
        
        # Check if the nested patchwork has a top plot at its own root level (before prepending)
        has_nested_root_top_plot <- FALSE
        if (length(nested_info$all_top_plots) > 0) {
          for (top_plot in nested_info$all_top_plots) {
            if (length(top_plot$path) == 0) {
              has_nested_root_top_plot <- TRUE
              break
            }
          }
        }
        
        # Prepend current child_index to each nested path
        for (nested_path in nested_info$plot_paths) {
          plot_paths <- append(plot_paths, list(c(child_index, nested_path)))
        }
        
        # Collect all nested top plots, prepending current child_index to their paths
        for (top_plot in nested_info$all_top_plots) {
          top_plot$path <- c(child_index, top_plot$path)
          all_top_plots <- append(all_top_plots, list(top_plot))
        }
        
        # If the nested patchwork has a top plot at its root, add the path for it
        if (has_nested_root_top_plot) {
          plot_paths <- append(plot_paths, list(c(child_index)))
        }
      } else {
        # Regular plot (or inset) - simple path
        plot_paths <- append(plot_paths, list(c(child_index)))
      }
    }
  }
  
  # Check if this patchwork itself has a top plot
  if ("patchwork" %in% class(patchwork_obj) && length(patchwork_obj@layers) > 0) {
    top_plot_info <- list(
      path = integer(0),  # Empty path at this level
      child_index = num_direct_children + 1
    )
    all_top_plots <- append(all_top_plots, list(top_plot_info))
  }
  
  return(list(plot_paths = plot_paths, all_top_plots = all_top_plots))
}


# Hide all plots in gtable except those in paths_to_reveal
hide_all_plots_except <- function(gtable_obj, paths_to_reveal, all_top_plots = list(), current_path = integer(0)) {
  
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
  
  # Check if we're at any top plot level and should reveal it
  reveal_top_plot <- FALSE
  reveal_top_plot_only <- FALSE
  top_plot_indices <- integer(0)
  
  if (length(all_top_plots) > 0) {
    # Check if any path in paths_to_reveal is empty (reveals top plot at this level)
    has_empty_path <- any(vapply(paths_to_reveal, function(path) length(path) == 0, logical(1)))
    
    if (has_empty_path) {
      # Find all top plots at this level
      for (top_plot in all_top_plots) {
        at_this_level <- length(top_plot$path) == length(current_path) &&
                        (length(current_path) == 0 || all(top_plot$path == current_path))
        if (at_this_level) {
          reveal_top_plot <- TRUE
          top_plot_indices <- c(top_plot_indices, top_plot$child_index)
        }
      }
      
      if (reveal_top_plot) {
        # If only the empty path exists, reveal only the top plot(s)
        if (length(paths_to_reveal) == 1) {
          reveal_top_plot_only <- TRUE
          child_indices_to_keep <- top_plot_indices
        } else {
          child_indices_to_keep <- unique(c(child_indices_to_keep, top_plot_indices))
        }
      }
    }
  }

  # Zero out grobs not in child_indices_to_keep
  for (grob_index in seq_len(length(gtable_obj))) {
    grob_name <- gtable_obj$layout[grob_index, "name"]
    grob_obj <- gtable_obj$grobs[[grob_index]]
    
    # Handle insets - extract inset number from name like "inset_2-1"
    if ("inset_table" %in% class(grob_obj)) {
      inset_match <- stringr::str_extract(grob_name, "inset_(\\d+)")
      if (!is.na(inset_match)) {
        inset_child_index <- as.integer(stringr::str_remove(inset_match, "inset_"))
        # Zero out inset if its child index is not in child_indices_to_keep
        if (!(inset_child_index %in% child_indices_to_keep)) {
          gtable_obj$grobs[[grob_index]] <- ggplot2::zeroGrob()
        }
      }
      next
    }
    
    # Regular grob - extract number suffix
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
          all_top_plots, 
          c(current_path, child_index)
        )
        gtable_obj$grobs[[nested_gtable_index]] <- nested_gtable_modified
      }
    }
  }

  return(gtable_obj)
}