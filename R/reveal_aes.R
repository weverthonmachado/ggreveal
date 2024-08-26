#' Reveal plot by aes
#'
#' Turns a ggplot into a list of plots, showing data incrementally by an
#' arbitrary aesthetic. 
#' 
#' @param p A ggplot2 object
#' @param aes which aesthetic to reveal E.g.: group, colour, shape, linetype
#' @param order (optional) A numeric vector specifying in which order to reveal
#' levels of the specified aesthetic.
#' 
#'   For example, if `aes='shape'` and the plot uses three shapes, `order = c(3,
#'   2, 1)` will invert the order in which they are revealed. 
#' 
#'   Any shape not included in the vector will be omitted from the incremental
#'   plots. E.g.: with `order = c(3, 1)`, the second shape is not shown.
#' 
#'   By default, the first plot is blank, showing layout elements (title,
#'   legends, axes, etc) but no data. To omit the blank plot, include `-1`: e.g. 
#'   `order = c(-1, 3, 1)`, or `order = -1`.
#' 
#' @param max maximum number of unique levels of aesthetic to be used
#' @return A list of ggplot2 objects, which can be passed to [reveal_save()]
#' @export
#' @examples
#' # Create full plot
#' library(ggplot2)
#'
#'  p <- mtcars |>
#'    ggplot(aes(mpg, wt,
#'               color = factor(vs),
#'               group = factor(vs))) +
#'    geom_point(aes(shape=factor(am)), size=2) +
#'    geom_smooth(method="lm",
#'                formula = 'y ~ x',
#'                linewidth=1) 
#'  p
#'
#'  plot_list <- reveal_aes(p, "shape")
#'  plot_list[[1]]
#'  plot_list[[2]]
#'  plot_list[[3]]
#'  plot_list[[4]]
#'
#'
#' # Save plots
#' reveal_save(plot_list, "myplot.png", width = 8, height = 4)
#' 
#' # Clean temp files
#' file.remove(list.files(path = tempdir(), pattern = "myplot", full.names = TRUE)) 

reveal_aes <- function(p, aes = "group", order = NULL, max = 20){

  # Check arguments
  "ggplot" %in% class(p) || cli::cli_abort("{deparse(substitute(p))} is not a ggplot object")
  
  is.character(aes) & length(aes)==1 || cli::cli_abort("'aes' should be a string.")

  if (aes=="panel") {aes <- "PANEL"}
  if (aes=="color") {aes <- "colour"}

  # Check whether aes if explicitly (and uniquely) mapped to a variable
  search_list <- list(p)
  search_list  <- append(search_list, p$layers)
  aes_mapping <- sapply(search_list, function(x) {"quosure" %in% class(x$mapping[aes][[1]])})

  # Check whether aes is present in the plot data (in any layer)
  # (basic aes like color, panel and shape are usually
  # present even it they are not mapped to a variable)
  # If present, get its unique values (across al layers)
  p_build <- ggplot2::ggplot_build(p)
  p_build_original <- p_build
  aes_names_data_list <- lapply(p_build$data, names)
  aes_names_data <- unique(unlist(aes_names_data_list))
  if (aes %in% aes_names_data){
    aes_levels <- unique(unlist(lapply(p_build$data, 
      function(x) {
        if (aes %in% names(x)) {
          return(x[, aes])
        }
      })))
    
    if (aes %in% c("x", "y")) {
      aes_levels_original <- aes_levels
      aes_levels <- unique(round(aes_levels))
    }
    
    if (aes %in% c("group", "PANEL", "x", "y")) {
      aes_levels <- sort(aes_levels)
    }
    
  }


  if(sum(aes_mapping) > 1) {
    cli::cli_abort("It seems that the definition of '{aes}' varies across layers. Please use reveal_layers() instead.")
  } else if (sum(aes_mapping) == 0 & !(aes %in% aes_names_data)){
    cli::cli_abort("'{aes}' is not defined in any layer.")
  } else if (sum(aes_mapping) == 0 & (aes %in% aes_names_data) & length(aes_levels)==1){
    m <- if (aes %in% c("group", "PANEL", "colour")) "" else "unique value of "
    cli::cli_warn("There is only one {m}{tolower(aes)} in the plot. Maybe use reveal_layers()?") 
  } else if (sum(aes_mapping) == 0 & (aes %in% aes_names_data) & length(aes_levels)>1 & aes != "PANEL") {
    cli::cli_inform("Plot does not explicitly define '{aes}'. Using default '{aes}' set by ggplot2.")
  }
  
  omit_blank <- FALSE
  if (!is.null(order)) {
    if (is.numeric(order)){
      order <- unique(order)
      omit_blank <- -1 %in% order
      order <- order[order != -1]
      order <- order[order <= length(aes_levels)] # ignore numbers beyond total of levels
      if (length(order)==0) {
        order <- NULL
      }
    } else {
      cli::cli_warn("Argument 'order' is not a numeric vector and will be ignored.")
    }  
  } 

  # Reorder levels
  if (!is.null(order)) {
    aes_levels <- aes_levels[order]
  }

  if (length(aes_levels) > 20 & max == 20){
    m <- if (aes %in% c("group", "PANEL", "colour")) paste0(tolower(aes), "s") else paste0("unique values of ", aes)
    cli::cli_warn("There are more than {max} {m}, and only the first {max} will be used. You can use the argument 'max' to increase the limit.")
    cli::cli_alert_info("Note: creating intermediary plots for many {m} can be slow.")
  }
  if (length(aes_levels) > max) {
    aes_levels <- aes_levels[1:max]
  } 


  levels_increment <- c()
  plot_list <- list()

  # Make step and append
  if (!omit_blank) {
    p_step <- make_step_by_layer(p, p_build, layers_increment = list())
    plot_list <- append(plot_list, list(p_step))
  }

  # By this point, we know aes, whether explicitly defined in the function call
  # or not, is present in at least one layer (i.e. the layer data)
  # has a column for the aes). But it might not present in all layers.
  aes_in_layer <- sapply(aes_names_data_list, function(x) aes %in% x)                                                                                                                  

  if (!all(aes_in_layer)) {
    layers_without_aes <- p$layers[!aes_in_layer]
    # If the first layer does not have the aes, add all layers without the aes 
    # before start revealing by aes
    # If the first layer has the aes, all layers without the aes will be added 
    # in the end
    if (!aes_in_layer[1]) {
      p_step <- make_step_by_layer(p, p_build, layers_increment = layers_without_aes)
      plot_list <- append(plot_list, list(p_step))
    } else {
      for (d in seq_along(p_build$data)) {
          filter <- aes_in_layer[d]
          p_build$data[[d]] <- p_build$data[[d]][filter,]
      }
    }
  }

    
  
  for (i in seq_along(aes_levels)) {
    
    if (aes %in% c("x", "y")) {
      levels_increment <- c(levels_increment, 
                            aes_levels_original[round(aes_levels_original)==aes_levels[i]])
    } else {
      levels_increment <- c(levels_increment,  aes_levels[i])
    }

    # Make step and append
    p_step <- make_step(p_build, p_build_original, aes, levels_increment)
    plot_list <- append(plot_list, list(p_step))

    }
  
  # Adding layers without the aes 
  if (!all(aes_in_layer) & aes_in_layer[1]) {
    plot_list <- append(plot_list, list(p))
  }
  
  if (omit_blank) {
    attr(plot_list, "omit_blank") <- omit_blank
  }
  return(plot_list)

  }
