test_that("warn if there are no groups in the plot", {
  expect_warning((reveal_groups(make_test_plot("nogroup"))))
})

test_that("output has correct type", {
  p <- make_test_plot()
  expect_type(reveal_groups(p), "list")
  expect_s3_class(reveal_groups(p)[[1]], "ggplot")
})

test_that("output has correct length", {
  p <- make_test_plot()
  expect_length(reveal_groups(p), 4)
  expect_length(reveal_groups(p, order = -1), 3)
  expect_length(reveal_groups(p, order = c(2,1)), 3)
  expect_length(reveal_groups(p, order = c(2)), 2)
  expect_length(reveal_groups(p, order = c(2, -1)), 1)
})

test_that("plots look like they should",{
  skip_on_ci()
  skip_on_cran()
  plot_list <- reveal_groups(make_test_plot(), order = c(2,1,3))
  expect_doppelganger("plot1", plot_list[[1]])
  expect_doppelganger("plot2", plot_list[[2]])
  expect_doppelganger("plot3", plot_list[[3]])
  expect_doppelganger("plot4", plot_list[[4]])
})
