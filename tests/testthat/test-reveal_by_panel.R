library(ggplot2)
data("mtcars")

p_fail <- ggplot(mtcars, aes(mpg, hp, group=am)) +
          geom_point()

p <- ggplot(mtcars, aes(mpg, hp)) +
      geom_point() +
      facet_wrap(vs~am)


test_that("execution fails if there are no facets in the plot", {
  expect_error(reveal_by_panel(p_fail))
})


test_that("output has correct type", {
  expect_type(reveal_by_panel(p), "list")
  expect_s3_class(reveal_by_panel(p)[[1]], "ggplot")
})


test_that("output has correct length", {
  expect_length(reveal_by_panel(p), 5)
  expect_length(reveal_by_panel(p, order = -1), 4)
  expect_length(reveal_by_panel(p, order = c(-1, 4:1)), 4)
  expect_length(reveal_by_panel(p, order = c(2, 3, 4)), 4)
  expect_length(reveal_by_panel(p, order = c(2, -1)), 1)
})
