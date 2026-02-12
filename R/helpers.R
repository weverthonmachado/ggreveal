make_step <- function(p_build, p_build_original, aes, increment){

  p_step <- p_build

  for (d in seq_along(p_step$data)) {
    # If the layer (i.e. each element of the list p_step$data) has the aes
    # (no matter how it was defined: globally of only for the layer),
    # keep only the levels defined in the object increment
    # If the layers does not have the aes, drop all data
    # (reveal of layers without the aes is handled in reveal_aes.R)
    if (aes %in% names(p_step$data[[d]])) {
      filter <- p_step$data[[d]][, aes] %in% increment
      p_step$data[[d]] <- p_step$data[[d]][filter,]
    } else {
      p_step$data[[d]] <- p_step$data[[d]][FALSE,]
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

  p <- ggplot2::ggplot(df, mapping) +
      layers +
      facet

  suppressWarnings(return(p))
}



# E.g. https://github.com/gavinsimpson/gratia/issues/93
expect_doppelganger <- function(title, fig, path = NULL, ...) {
  testthat::skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(title, fig,...)
}


#' @noRd
make_test_patchwork <- function(type = c("simple", "nested1", "nested2", "inset")){

  type <- rlang::arg_match(type)

  p1 <- ggplot2::ggplot(datasets::mtcars) + 
         ggplot2::geom_point(ggplot2::aes(.data$mpg, .data$disp)) + 
         ggplot2::ggtitle('Plot 1')

  p2 <-  ggplot2::ggplot(datasets::mtcars) + 
          ggplot2::geom_boxplot( ggplot2::aes(.data$gear, .data$disp, group = .data$gear)) + 
          ggplot2::ggtitle('Plot 2')

  p3 <-  ggplot2::ggplot(datasets::mtcars) + 
          ggplot2::geom_point(ggplot2::aes(.data$hp, .data$wt, colour = .data$mpg)) + 
          ggplot2::ggtitle('Plot 3')
  
  p4 <- ggplot2::ggplot(datasets::mtcars) + 
        ggplot2::geom_bar( ggplot2::aes(.data$gear)) + 
        ggplot2::ggtitle('Plot 4')
  
  if (type=="simple"){
    pw <- patchwork::wrap_plots(p1, p2, p3)
  } else if (type=="nested1") {
    pw <- patchwork::wrap_plots(patchwork::wrap_plots(p1, p2, nrow=2), patchwork::wrap_plots(p3, p4, nrow=2))

  } else if (type=="nested2") {
    pw <- patchwork::wrap_plots(p1, 
              patchwork::wrap_plots(p2, 
                        patchwork::wrap_plots(p3, p4, nrow=2)))
    
  } else if (type=="inset") {
    pw <-  patchwork::wrap_plots(p1, 
                patchwork::wrap_plots(p2, 
                                    patchwork::inset_element(p3,  left = 0.6, bottom = 0.6, right = 1, top = 1),  
                                    patchwork::inset_element(p4,  left = 0, bottom = 0, right = 0.6, top = 0.6)),
                                    nrow=2)

  }

  return(pw  + patchwork::plot_annotation(title = "Patchwork", tag_levels = "a"))

}