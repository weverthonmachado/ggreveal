test_that("output has correct type", {
  pw <- make_test_patchwork()
  expect_type(result, "list")
  expect_s3_class(result[[1]], "ggplot")
})

test_that("output has correct length - simple", {
  pw <- make_test_patchwork()
  expect_length(reveal_patchwork(pw), 4)
  expect_length(reveal_patchwork(pw, order = -1), 3)
  expect_length(reveal_patchwork(pw, order = c(2,1)), 3)
  expect_length(reveal_patchwork(pw, order = c(2)), 2)
  expect_length(reveal_patchwork(pw, order = c(2, -1)), 1)
})

test_that("output has correct length - nested2", {
  pw <- make_test_patchwork("nested2")
  expect_length(reveal_patchwork(pw), 5)
  expect_length(reveal_patchwork(pw, order = -1), 4)
  expect_length(reveal_patchwork(pw, order = c(2,1)), 3)
  expect_length(reveal_patchwork(pw, order = c(2)), 2)
  expect_length(reveal_patchwork(pw, order = c(2, -1)), 1)
})

test_that("plots look like they should - simple",{
  skip_on_ci()
  skip_on_cran()
  plot_list <- reveal_patchwork(make_test_patchwork(), order = c(2,1,3))
  expect_doppelganger("plot1 - simple", plot_list[[1]])
  expect_doppelganger("plot2 - simple", plot_list[[2]])
  expect_doppelganger("plot3 - simple", plot_list[[3]])
  expect_doppelganger("plot4 - simple", plot_list[[4]])
})

test_that("plots look like they should - nested2",{
  skip_on_ci()
  skip_on_cran()
  plot_list <- reveal_patchwork(make_test_patchwork("nested2"), order = c(3,4,1))
  expect_doppelganger("plot1 - nested2", plot_list[[1]])
  expect_doppelganger("plot2 - nested2", plot_list[[2]])
  expect_doppelganger("plot3 - snested2imple", plot_list[[3]])
  expect_doppelganger("plot4 - nested2", plot_list[[4]])
})
