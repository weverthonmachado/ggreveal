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
  aes_names_data <- unique(unlist(lapply(p_build$data, names)))
  if (aes %in% aes_names_data){
    aes_levels <- sort(unique(unlist(lapply(p_build$data, 
      function(x) {
        if (aes %in% names(x)) {
          return(x[, aes])
        }
      }))))
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
    p_step <- make_step(p, p_build, aes, levels_increment)
    plot_list <- append(plot_list, list(p_step))
  }


  for (i in seq_along(aes_levels)) {

    levels_increment <- c(levels_increment,  aes_levels[i])

    # Make step and append
    p_step <- make_step(p, p_build, aes, levels_increment)
    plot_list <- append(plot_list, list(p_step))

  }
  if (omit_blank) {
    attr(plot_list, "omit_blank") <- omit_blank
  }
  return(plot_list)

  }
