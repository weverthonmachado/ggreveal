library(ggplot2)
data("mtcars")

p_fail <- ggplot(mtcars, aes(mpg, hp, group=am)) +
          geom_point()

p <- ggplot(mtcars, aes(mpg, hp)) +
      geom_point() +
      facet_wrap(~am)


test_that("execution fails if there are no facets in the plot", {
  expect_error(reveal_by_panel(p_fail))
})


test_that("output has correct type", {
  expect_type(reveal_by_panel(p), "list")
  expect_s3_class(reveal_by_panel(p)[[1]], "ggplot")
})
