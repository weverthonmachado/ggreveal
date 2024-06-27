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
