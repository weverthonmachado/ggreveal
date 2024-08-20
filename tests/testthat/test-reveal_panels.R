test_that("warning if there are no facets in the plot", {
  expect_warning(reveal_panels(make_test_plot("nofacet")))
})

test_that("warning if order is not numeric", {
  expect_warning(reveal_panels(make_test_plot(), order = "a"))
})

test_that("output has correct type", {
  p <- make_test_plot()
  expect_type(reveal_panels(p), "list")
  expect_s3_class(reveal_panels(p)[[1]], "ggplot")
  expect_s3_class(reveal_panels(p, what = "everything")[[1]], "ggplot")
})

test_that("output has correct length", {
  p <- make_test_plot()
  expect_length(reveal_panels(p), 10)
  expect_length(reveal_panels(p, order = -1), 9)
  expect_length(reveal_panels(p, order = c(-1, 4:1)), 4)
  expect_length(reveal_panels(p, order = c(2, 3, 4)), 4)
  expect_length(reveal_panels(p, order = c(2, -1)), 1)
  expect_length(reveal_panels(p, order = c(2, -1), what = "everything"), 1)
})

test_that("plots look like they should - only data",{
  skip_on_ci()
  skip_on_cran()
  plot_list <- reveal_panels(make_test_plot(), order = c(2,1,3))
  expect_doppelganger("plot1 - only data", plot_list[[1]])
  expect_doppelganger("plot2 - only data", plot_list[[2]])
  expect_doppelganger("plot3 - only data", plot_list[[3]])
  expect_doppelganger("plot4 - only data", plot_list[[4]])
})

test_that("plots look like they should - everything - grid",{
  skip_on_ci()
  skip_on_cran()
  plot_list <- reveal_panels(make_test_plot(), order = c(2,1,3), "everything")
  expect_doppelganger("plot1 - everything - grid", plot_list[[1]])
  expect_doppelganger("plot2 - everything - grid", plot_list[[2]])
  expect_doppelganger("plot3 - everything - grid", plot_list[[3]])
  expect_doppelganger("plot4 - everything - grid", plot_list[[4]])
})

test_that("plots look like they should - everything - wrap",{
  skip_on_ci()
  skip_on_cran()
  plot_list <- reveal_panels(make_test_plot("facet_wrap"), order = c(2,1,3), "everything")
  expect_doppelganger("plot1 - everything - wrap", plot_list[[1]])
  expect_doppelganger("plot2 - everything - wrap", plot_list[[2]])
  expect_doppelganger("plot3 - everything - wrap", plot_list[[3]])
  expect_doppelganger("plot4 - everything - wrap", plot_list[[4]])
})
