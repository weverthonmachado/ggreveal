make_step <- function(p, p_build, var, increment){

  p_step <- p
  p_step <- ggplot2::ggplot_build(p_step)

  for (d in seq_along(p_step$data)) {
    filter <- p_step$data[[d]][, var] %in% increment
    p_step$data[[d]] <- p_step$data[[d]][filter,]
  }

  p_step$layout <- p_build$layout
  p_step$plot$guides <- p_build$plot$guides
  p_step$plot$scales <- p_build$plot$scales
  p_step <- ggplotify::as.ggplot(ggplot2::ggplot_gtable(p_step))

  return(p_step)
}



make_test_plot <- function(type = c("default", "nogroup", "nolayer", "nofacet", "facet_wrap", "bar")) {

 `%+%` <- ggplot2::`%+%`

  type <- rlang::arg_match(type)

  df <- dplyr::filter(ggplot2::diamonds,
          cut %in% c("Fair", "Good", "Premium"),
          color %in% c("E", "F", "G"),
          clarity %in% c("SI2", "SI1",  "VS2")) 


  mapping <- ggplot2::aes(carat, price,
                color = cut,
                fill = cut,
                group = cut)

  layers <- list(
             ggplot2::geom_point(),
             ggplot2::geom_smooth(method="lm", formula = "y ~ x"),
             ggplot2::geom_rug()
            )

  facet <- ggplot2::facet_grid(color ~ clarity) 

  if (type=="nogroup") {

    mapping <- ggplot2::aes(carat, price)

  } else if (type=="nolayer") {

    layers <- NULL

  } else if (type=="nofacet") {

    facet <- NULL

  } else if (type=="facet_wrap") {

    facet <- ggplot2::facet_wrap(color ~ clarity) 

  } else if (type=="bar") {

    mapping <- ggplot2::aes(x = color, 
                            color = cut,
                            fill = cut, 
                            group = cut)
    
    layers <- list(
      ggplot2::geom_bar()
      )
                 
    facet <- ggplot2::facet_wrap(~ clarity) 

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