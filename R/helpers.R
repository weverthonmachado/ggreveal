make_step <- function(p_build, p_build_original, var, increment){

  p_step <- p_build

  for (d in seq_along(p_step$data)) {
    if (var %in% names(p_step$data[[d]])) {
      filter <- p_step$data[[d]][, var] %in% increment
      p_step$data[[d]] <- p_step$data[[d]][filter,]
    }
  }
  
  p_step$layout <- p_build_original$layout
  p_step$plot$guides <- p_build_original$plot$guides
  p_step$plot$scales <- p_build_original$plot$scales
  p_step <- ggplotify::as.ggplot(ggplot2::ggplot_gtable(p_step))

  return(p_step)
}


#' @noRd
#' @importFrom rlang .data
make_test_plot <- function(type = c("default", "nogroup", "nolayer", "nofacet", "facet_wrap", "bar", "multiple_axis", "grouped_bar"),
                           custom_aes = NULL) {
  `%+%` <- ggplot2::`%+%`
  type <- rlang::arg_match(type)
  df <- dplyr::filter(ggplot2::diamonds,
                      .data$cut %in% c("Fair", "Good", "Premium"),
                      .data$color %in% c("E", "F", "G"),
                      .data$clarity %in% c("SI2", "SI1",  "VS2")) 
  # Default mapping
  mapping <- ggplot2::aes(.data$carat, .data$price,
                          color = .data$cut,
                          fill = .data$cut,
                          group = .data$cut)
  
  # Apply custom aesthetics if provided
  if (!is.null(custom_aes)) {
    if (is.function(custom_aes)) {
      mapping <- custom_aes(mapping)
    } else if (inherits(custom_aes, "uneval")) {
      mapping <- custom_aes
    }
  }
  
  layers <- list(
    ggplot2::geom_point(),
    ggplot2::geom_smooth(method="lm", formula = "y ~ x"),
    ggplot2::geom_rug()
  )
  facet <- ggplot2::facet_grid(.data$color ~ .data$clarity) 

  if (type=="nogroup") {

    mapping <- ggplot2::aes(.data$carat, .data$price)

  } else if (type=="nolayer") {

    layers <- NULL

  } else if (type=="nofacet") {

    facet <- NULL

  } else if (type=="facet_wrap") {

    facet <- ggplot2::facet_wrap(.data$color ~ .data$clarity) 

  } else if (type=="bar") {

    mapping <- ggplot2::aes(x = .data$color, 
                            color = .data$cut,
                            fill = .data$cut, 
                            group = .data$cut)
    
    layers <- list(
      ggplot2::geom_bar()
      )
                 
    facet <- ggplot2::facet_wrap(~ .data$clarity) 

  } else if (type=="multiple_axis") {

    mapping <- ggplot2::aes(x = .data$color, 
                            color = .data$cut,
                            fill = .data$cut, 
                            group = .data$cut)
    
    layers <- list(
      ggplot2::geom_bar(),
      ggplot2::geom_boxplot(ggplot2::aes(x= .data$cut, y = .data$price))
      )
                 
    facet <- ggplot2::facet_wrap(~ .data$clarity) 

  } else if (type=="grouped_bar") {

    mapping <- ggplot2::aes(x = .data$color, 
                            fill = .data$cut)
    
    layers <- list(
      ggplot2::geom_bar(position = ggplot2::position_dodge(1))
      )
                 
    facet <- ggplot2::facet_wrap(~ .data$clarity) 

  }

  p <- ggplot2::ggplot(df, mapping) %+%
      layers %+%
      facet

  suppressWarnings(return(p))
}



# E.g. https://github.com/gavinsimpson/gratia/issues/93
expect_doppelganger <- function(title, fig, path = NULL, ...) {
  testthat::skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(title, fig,...)
}