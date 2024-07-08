test_that("reveal_save handles a single plot correctly", {
  p <- make_test_plot()
  plot_list <- reveal_groups(p)
  single_plot_list <- plot_list[1]
  basename <- "test_plot.png"
  
  # Mock ggsave to avoid actually saving the file
  mockery::stub(reveal_save, 'ggplot2::ggsave', function(...) invisible("test_plot_0_last.png"))
  
  paths <- reveal_save(single_plot_list, basename)
  
  expect_equal(length(paths), 1)
  expect_match(paths[1], "test_plot_0_last.png")
})

test_that("reveal_save handles multiple plots correctly", {
  p <- make_test_plot()
  plot_list <- reveal_groups(p)
  multiple_plot_list <- plot_list[1:3]
  basename <- "test_plot.png"
  
  # Mock ggsave to avoid actually saving the file
  mockery::stub(reveal_save, 'ggplot2::ggsave', function(filename, plot, ...) invisible(paste0(filename)))
  
  paths <- reveal_save(multiple_plot_list, basename)
  
  expect_equal(length(paths), 3)
  expect_match(paths[1], "test_plot_0.png")
  expect_match(paths[2], "test_plot_1.png")
  expect_match(paths[3], "test_plot_2_last.png")
})

test_that("reveal_save handles plots with omit_blank attribute correctly", {
  p <- make_test_plot()
  plot_list <- reveal_groups(p)
  omit_blank_plot_list <- plot_list
  attr(omit_blank_plot_list, "omit_blank") <- TRUE
  basename <- "test_plot.png"
  
  # Mock ggsave to avoid actually saving the file
  mockery::stub(reveal_save, 'ggplot2::ggsave', function(filename, plot, ...) invisible(paste0(filename)))
  
  paths <- reveal_save(omit_blank_plot_list, basename)
  
  expect_equal(length(paths), length(omit_blank_plot_list))
  expect_match(paths[1], "test_plot_1.png")
  expect_match(paths[length(paths)], "test_plot_4_last.png")
})


test_that("reveal_save generates correct file extensions", {
  p <- make_test_plot()
  plot_list <- reveal_groups(p)
  multiple_plot_list <- plot_list[1:3]
  basename <- "test_plot.pdf"
  
  # Mock ggsave to avoid actually saving the file
  mockery::stub(reveal_save, 'ggplot2::ggsave', function(filename, plot, ...) invisible(paste0(filename)))
  
  paths <- reveal_save(multiple_plot_list, basename)
  
  expect_equal(length(paths), 3)
  expect_match(paths[1], "test_plot_0.pdf")
  expect_match(paths[2], "test_plot_1.pdf")
  expect_match(paths[3], "test_plot_2_last.pdf")
})


test_that("reveal_save returns invisible paths", {
  p <- make_test_plot()
  plot_list <- reveal_groups(p)
  multiple_plot_list <- plot_list[1:3]
  basename <- "test_plot.png"
  
  # Mock ggsave to avoid actually saving the file
  mockery::stub(reveal_save, 'ggplot2::ggsave', function(filename, plot, ...) invisible(paste0(filename)))
  
  paths <- reveal_save(multiple_plot_list, basename)
  
  expect_silent(paths)
})
