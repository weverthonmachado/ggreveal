test_that("execution fails if there are no facets in the plot", {
  expect_error(reveal_panels(make_test_plot("nopanel")))
})

test_that("output has correct type", {
  p <- make_test_plot()
  expect_type(reveal_panels(p), "list")
  expect_s3_class(reveal_panels(p)[[1]], "ggplot")
})

test_that("output has correct length", {
  p <- make_test_plot()
  expect_length(reveal_panels(p), 10)
  expect_length(reveal_panels(p, order = -1), 9)
  expect_length(reveal_panels(p, order = c(-1, 4:1)), 4)
  expect_length(reveal_panels(p, order = c(2, 3, 4)), 4)
  expect_length(reveal_panels(p, order = c(2, -1)), 1)
})

test_that("plots look like they should - only data",{
  plot_list <- reveal_panels(make_test_plot(), order = c(2,1,3))
  expect_doppelganger("plot1 - only data", plot_list[[1]])
  expect_doppelganger("plot2 - only data", plot_list[[2]])
  expect_doppelganger("plot3 - only data", plot_list[[3]])
  expect_doppelganger("plot4 - only data", plot_list[[4]])
})

test_that("plots look like they should - everything",{
  skip_on_ci()
  plot_list <- reveal_panels(make_test_plot(), order = c(2,1,3), "everything")
  expect_doppelganger("plot1 - everything", plot_list[[1]])
  expect_doppelganger("plot2 - everything", plot_list[[2]])
  expect_doppelganger("plot3 - everything", plot_list[[3]])
  expect_doppelganger("plot4 - everything", plot_list[[4]])
})
