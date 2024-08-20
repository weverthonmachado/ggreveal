test_that("warning if axis is not defined", {
  expect_warning((reveal_y(make_test_plot("bar"))))
})

test_that("output has correct type", {
  p <- make_test_plot("bar")
  expect_type(reveal_x(p), "list")
  expect_s3_class(reveal_x(p)[[1]], "ggplot")
})

test_that("output has correct length", {
  p <- make_test_plot("bar")
  expect_length(reveal_axis(p), 4)
  expect_length(reveal_axis(p, order = -1), 3)
  expect_length(reveal_axis(p, order = c(2,1)), 3)
  expect_length(reveal_axis(p, order = c(2)), 2)
  expect_length(reveal_axis(p, order = c(2, -1)), 1)
})

test_that("plots look like they should - stacked bar",{
  skip_on_ci()
  skip_on_cran()
  plot_list <- reveal_axis(make_test_plot("bar"), order = c(2,1,3))
  expect_doppelganger("plot1", plot_list[[1]])
  expect_doppelganger("plot2", plot_list[[2]])
  expect_doppelganger("plot3", plot_list[[3]])
  expect_doppelganger("plot4", plot_list[[4]])
})

test_that("plots look like they should - dodged bar",{
  skip_on_ci()
  skip_on_cran()
  plot_list <- reveal_axis(make_test_plot("grouped_bar"), order = c(2,1,3))
  expect_doppelganger("plot1 - grouped_bar", plot_list[[1]])
  expect_doppelganger("plot2 - grouped_bar", plot_list[[2]])
  expect_doppelganger("plot3 - grouped_bar", plot_list[[3]])
  expect_doppelganger("plot4 - grouped_bar", plot_list[[4]])
})

test_that("fails with multiple definitions of axis", {
  p <- make_test_plot("multiple_axis")
  expect_error(reveal_axis(p))
})
