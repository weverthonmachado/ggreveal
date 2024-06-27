reveal_by_facet <- function(p, what = c("data", "axis", "label", "everything")){

  # Check arguments
  "ggplot" %in% class(p) || rlang::abort(paste(deparse(substitute(p)),
                                        "is not a ggplot object"))
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
