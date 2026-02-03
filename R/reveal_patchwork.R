# TODO: 
# - deal with insets (and gtables?)
# - deal with order argument for last ("top" plot). 
#   Run reveal_patchwork(patch8, order = c(5:1)) to understand

#' Reveal plots in a patchwork object
#'
#' Turns a patchwork into a list of plots that reveal each child plot (including
#' nested patchworks) one-by-one. The function handles arbitrary nesting levels.
#' 
#' @export
reveal_patchwork <- function(p, order = NULL){

  "patchwork" %in% class(p) || cli::cli_abort("{deparse(substitute(p))} is not a patchwork object")

  # Paths to nested plots. If there is a top-level plot (depends on patchwork layout)
  # the total number of plots is length(paths)+1
  paths <- collect_paths(p)

  omit_blank <- FALSE
  if (!is.null(order)){
    if (is.numeric(order)){
      order <- unique(order)
      omit_blank <- -1 %in% order
      order <- order[order != -1]
      order <- order[order <= length(paths)] # ignore numbers beyond total of plots
      if (length(order)==0) order <- seq_len(length(paths))
    } else {
      cli::cli_warn("Argument 'order' is not a numeric vector and will be ignored.")
      order <- seq_len(length(paths))
    }
    paths <- paths[order]
  }

  # Convert patchwork to a gtable we can edit
  p_gt <- patchwork::patchworkGrob(p)

  plot_list <- list()

  # Optionally add blank first step (no revealed paths)
  if (!omit_blank){
    gt_blank <- apply_reveal_to_gtable(p_gt, list())
    plot_list <- append(plot_list, list(ggplotify::as.ggplot(gt_blank)))
  }

  # Build incremental steps
  for (r in seq_len(length(paths))){
    gt_r <- p_gt
    reveal_paths_r <- paths[seq_len(r)]
    gt_mod <- apply_reveal_to_gtable(gt_r, reveal_paths_r)
    plot_list <- append(plot_list, list(ggplotify::as.ggplot(gt_mod)))
  }
  plot_list <- append(plot_list, list(p))

  if (omit_blank) {
    attr(plot_list, "omit_blank") <- omit_blank
  }
  return(plot_list)
}



# Collect neted paths. Each path is an integer vector describing the child
# indexes at each nesting level, e.g. c(2,1)
collect_paths <- function(pw){
  out <- list()
  for (i in seq_along(pw$patches$plots)){
    child <- pw$patches$plots[[i]]
    if (!is.null(child$patches) && !is.null(child$patches$plots)){
      sub <- collect_paths(child)
      for (s in sub){
        out <- append(out, list(c(i, s)))
      }
    } else {
      out <- append(out, list(c(i)))
    }
  }
  return(out)
}

# Apply reveal to a gtable recursively. `paths_rel` are paths
# relative to this gtable: e.g. list(c(1), c(2,1)).
apply_reveal_to_gtable <- function(gt, paths_rel){

  # Which top-level child indices should be kept (revealed) at this level?
  keep <- if (length(paths_rel)==0) integer(0) else unique(vapply(paths_rel, function(x) x[1], integer(1)))

  # Zero grobs whose layout name ends with -<j> where j not in keep
  for (i in seq_len(length(gt))){
    nm <- gt$layout[i, "name"]
    # detect a trailing -<number> suffix
    m <- stringr::str_extract(nm, "-(\\d+)$")
    if (!is.na(m)){
      idx <- as.integer(stringr::str_remove(m, "^-"))
      if (!(idx %in% keep)){
        gt$grobs[[i]] <- ggplot2::zeroGrob()
      }
    }
  }

  # For kept indices that have deeper paths, find the nested gtable and
  # recurse
  if (length(keep)>0){
    for (k in keep){
      subs <- lapply(paths_rel, function(x) if (x[1]==k) x[-1] else NULL)
      subs <- Filter(Negate(is.null), subs)
      # if there are no deeper elements, nothing to do for nested
      if (length(subs)==0) next

      # find nested gtable corresponding to child k
      nested_idx <- NULL
      for (i in seq_len(length(gt))){
        nm <- gt$layout[i, "name"]
        if (stringr::str_detect(nm, paste0("-", k, "$"))){
          if ("gtable_patchwork" %in% class(gt$grobs[[i]])){
            nested_idx <- i
            break
          }
        }
      }
      if (!is.null(nested_idx)){
        nested_gt <- gt$grobs[[nested_idx]]
        # remove the leading index from subs' paths
        subs_rel <- lapply(subs, function(x) if (length(x)==0) integer(0) else x)
        # In subs_rel each element currently is the remainder after dropping
        # the first element; they are already relative to nested_gt
        nested_gt_mod <- apply_reveal_to_gtable(nested_gt, subs_rel)
        gt$grobs[[nested_idx]] <- nested_gt_mod
      }
    }
  }

  return(gt)
}