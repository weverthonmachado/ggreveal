# Reveal plot by aes

Turns a ggplot into a list of plots, showing data incrementally by an
arbitrary aesthetic.

## Usage

``` r
reveal_aes(p, aes = "group", order = NULL, max = 20)
```

## Arguments

- p:

  A ggplot2 object

- aes:

  which aesthetic to reveal E.g.: group, colour, shape, linetype

- order:

  (optional) A numeric vector specifying in which order to reveal levels
  of the specified aesthetic.

  For example, if `aes='shape'` and the plot uses three shapes,
  `order = c(3, 2, 1)` will invert the order in which they are revealed.

  Any shape not included in the vector will be omitted from the
  incremental plots. E.g.: with `order = c(3, 1)`, the second shape is
  not shown.

  By default, the first plot is blank, showing layout elements (title,
  legends, axes, etc) but no data. To omit the blank plot, include `-1`:
  e.g. `order = c(-1, 3, 1)`, or `order = -1`.

- max:

  maximum number of unique levels of aesthetic to be used

## Value

A list of ggplot2 objects, which can be passed to
[`reveal_save()`](http://www.weverthon.com/ggreveal/reference/reveal_save.md)

## Examples

``` r
# Create full plot
library(ggplot2)

 p <- ggplot(mtcars, aes(mpg, wt,
              color = factor(vs),
              group = factor(vs))) +
   geom_point(aes(shape=factor(am)), size=2) +
   geom_smooth(method="lm",
               formula = 'y ~ x',
               linewidth=1) 
 p


 plot_list <- reveal_aes(p, "shape")
 plot_list[[1]]

 plot_list[[2]]

 plot_list[[3]]

 plot_list[[4]]



# Save plots
reveal_save(plot_list, "myplot.png", width = 8, height = 4, path = tempdir())
#> 
#> ── Saving incremental plots ──
#> 
#> ✔ /tmp/RtmpRjqXG9/myplot_0.png
#> ✔ /tmp/RtmpRjqXG9/myplot_1.png
#> ✔ /tmp/RtmpRjqXG9/myplot_2.png
#> ✔ /tmp/RtmpRjqXG9/myplot_3_last.png

# Clean temp files
file.remove(list.files(path = tempdir(), pattern = "myplot", full.names = TRUE)) 
#> [1] TRUE TRUE TRUE TRUE
```
