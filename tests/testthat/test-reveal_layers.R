test_that("execution fails if there are no layers in the plot", {
  expect_error(reveal_layers(make_test_plot("nolayer")))
})

test_that("output has correct type", {
  p <- make_test_plot()
  expect_type(reveal_layers(p), "list")
  expect_s3_class(reveal_layers(p)[[1]], "ggplot")
})

test_that("output has correct length", {
  p <- make_test_plot()
  expect_length(reveal_layers(p), 4)
  expect_length(reveal_layers(p, order = -1), 3)
  expect_length(reveal_layers(p, order = c(2,1)), 3)
  expect_length(reveal_layers(p, order = c(2)), 2)
  expect_length(reveal_layers(p, order = c(2, -1)), 1)
})

test_that("plots look like they should",{
  skip_on_ci()
  skip_on_cran()
  plot_list <- reveal_layers(make_test_plot(), order = c(2,1,3))
  expect_doppelganger("plot1", plot_list[[1]])
  expect_doppelganger("plot2", plot_list[[2]])
  expect_doppelganger("plot3", plot_list[[3]])
  expect_doppelganger("plot4", plot_list[[4]])
})
