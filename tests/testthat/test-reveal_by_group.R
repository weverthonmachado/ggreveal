library(ggplot2)
data("mtcars")

p_fail <- ggplot(mtcars, aes(mpg, hp)) +
           geom_point() +
           facet_wrap(~am)

p <- ggplot(mtcars, aes(mpg, hp, group=am)) +
     geom_point()

test_that("execution fails if there are no groups in the plot", {
  expect_error(reveal_by_group(p_fail))
})

test_that("output has correct type", {
  expect_type(reveal_by_group(p), "list")
  expect_s3_class(reveal_by_group(p)[[1]], "ggplot")
})

test_that("output has correct length", {
  expect_length(reveal_by_group(p), 3)
  expect_length(reveal_by_group(p, order = -1), 2)
  expect_length(reveal_by_group(p, order = c(2,1)), 3)
  expect_length(reveal_by_group(p, order = c(2)), 2)
  expect_length(reveal_by_group(p, order = c(2, -1)), 1)
})
