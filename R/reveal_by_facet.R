#' Reveal plot by facet
#'
#' Creates a list of plots, showing data incrementally by layers.
#'
#' @param p A ggplot2 object, in which a `group` aesthetic is used.
#' @param what one of `"data"` (the default), `"axis"`, `"label"` or `"everything"`.
#' With `"data"`, the basic graph layout (including axes and facet labels) is shown
#' from the start, and only the data points are shown incrementally. With `"axis"` and `"label"`,
#' both the data and the corresponding axes and facet labels, respectively, are shown
#' incrementally. With `"everything"`, the entire panels are shown incrementally.
#' @return A list of ggplot2 objects, which can be passed to [reveal_save()]
#' @export
#' @examples
#' # Create full plot
#' library(ggplot2)
#' data("mtcars")
#'
#' p <- mtcars |>
#'   ggplot(aes(mpg, wt,
#'              color = factor(vs),
#'              group = factor(vs))) +
#'   geom_point() +
#'   geom_smooth(method="lm",
#'               formula = 'y ~ x',
#'               linewidth=1) +
#'   facet_wrap(~am)
#' p
#'
#' # Only data
#' plot_list <- reveal_by_facet(p, "data")
#' plot_list[[1]]
#' plot_list[[2]]
#' plot_list[[3]]
#'
#' # Everything
#' plot_list <- reveal_by_facet(p, "everything")
#' plot_list[[1]]
#' plot_list[[2]]
#' plot_list[[3]]
#'
#'\dontrun{
#' # Save plots
#' reveal_save(plot_list, "myplot", width = 8, height = 4)
#' }
reveal_by_facet <- function(p, what = c("data", "axis", "label", "everything")){

  # Check arguments
  "ggplot" %in% class(p) || rlang::abort(paste(deparse(substitute(p)),
                                        "is not a ggplot object"))

  !is.na(stringr::str_extract(
            tolower(class(p$facet)[1]),
            "grid|wrap")) ||  rlang::abort(paste("Plot does not use facet_wrap or",
                                                  "facet_grid. Maybe use",
                                                  "reveal_by_group or reveal_by_layer?")
                                                                )

  what <- rlang::arg_match(what)

  if (what=="data") {
    plot_list <- reveal_by_facet_onlydata(p)
  } else {
    axis_opt <- ifelse(what=="everything", TRUE,
                      ifelse(what=="axis", TRUE, FALSE))

    label_opt <- ifelse(what=="everything", TRUE,
                       ifelse(what=="label", TRUE, FALSE))

    plot_list <- reveal_by_facet_everything(p, axis = axis_opt, label = label_opt)
  }

  return(plot_list)
}
