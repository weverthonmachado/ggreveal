make_test_plot <- function(type = c("default", "nogroup", "nolayer", "nofacet")) {

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