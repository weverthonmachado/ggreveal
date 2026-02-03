# Reveal plot by axis

Turns a ggplot into a list of plots, showing data incrementally by the
categories in the x or y axis.

## Usage

``` r
reveal_x(p, order = NULL)

reveal_y(p, order = NULL)
```

## Arguments

- p:

  A ggplot2 object

- order:

  (optional) A numeric vector specifying in which order to reveal the
  categories

  For example, if there are three categories in the axis,
  `order = c(3, 2, 1)` will invert the order in which they are revealed.

  Any category not included in the vector will be omitted from the
  incremental plots. E.g.: with `order = c(3, 1)`, the second category
  is not shown.

  By default, the first plot is blank, showing layout elements (title,
  legends, axes, etc) but no data. To omit the blank plot, include `-1`:
  e.g. `order = c(-1, 3, 1)`, or `order = -1`.

## Value

A list of ggplot2 objects, which can be passed to
[`reveal_save()`](http://www.weverthon.com/ggreveal/reference/reveal_save.md)

## Examples

``` r
# Create full plot
library(ggplot2)
data("mtcars")

p <- ggplot(mtcars, aes(factor(vs), 
             color = gear,
             fill= gear, 
             group = gear)) +
  geom_bar() +
  facet_wrap(~am)
p


plot_list <- reveal_x(p)
plot_list[[1]]

plot_list[[2]]

plot_list[[3]]


# Save plots
reveal_save(plot_list, "myplot.png", width = 8, height = 4, path = tempdir())
#> 
#> ── Saving incremental plots ──
#> 
#> ✔ /tmp/RtmpXzWhqn/myplot_0.png
#> ✔ /tmp/RtmpXzWhqn/myplot_1.png
#> ✔ /tmp/RtmpXzWhqn/myplot_2_last.png

# Clean temp files
file.remove(list.files(path = tempdir(), pattern = "myplot", full.names = TRUE)) 
#> [1] TRUE TRUE TRUE
```
