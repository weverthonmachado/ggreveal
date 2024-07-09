test_that("reveal_aes works with 'group' aesthetic", {
  p <- make_test_plot()
  result <- reveal_aes(p, aes = "group")
  expect_type(result, "list")
  expect_true(length(result) > 0)
})

test_that("reveal_aes works with 'color' aesthetic", {
  p <- make_test_plot()
  result <- reveal_aes(p, aes = "color")
  expect_type(result, "list")
  expect_true(length(result) > 0)
})

test_that("reveal_aes works with 'shape' aesthetic", {
  custom_shape_mapping <- function(mapping) {
    modifyList(mapping, ggplot2::aes(shape = as.character(clarity)) )
  }
  p <-  make_test_plot(custom_aes = custom_shape_mapping)
  result <- reveal_aes(p, aes = "shape")
  expect_type(result, "list")
  expect_true(length(result) == 4)
})

test_that("reveal_aes handles invalid 'aes' value correctly", {
  p <- make_test_plot(type = "default")
  expect_error(reveal_aes(p, aes = "invalid_aes"))
})

test_that("reveal_aes respects 'order' parameter", {
  p <- make_test_plot(type = "bar")
  result <- reveal_aes(p, aes = "color", order = c(2, 1))
  expect_type(result, "list")
  expect_true(length(result) > 0)
  expect_warning(reveal_aes(p, aes = "color", order = "a"))
})

test_that("reveal_aes limits levels with 'max' parameter", {
  p <- make_test_plot(type = "bar")
  suppressWarnings(result <- reveal_aes(p, aes = "color", max = 2))
  expect_type(result, "list")
  expect_equal(length(result), 2 + 1) # Including the initial empty plot
})
