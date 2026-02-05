# Saves incremental plots

Saves incremental plots

## Usage

``` r
reveal_save(plot_list, basename, ...)
```

## Arguments

- plot_list:

  A list of plots created by one of the `reveal_*` functions (e.g.
  [`reveal_groups()`](http://www.weverthon.com/ggreveal/reference/reveal_groups.md),
  [`reveal_layers()`](http://www.weverthon.com/ggreveal/reference/reveal_layers.md),
  [`reveal_aes()`](http://www.weverthon.com/ggreveal/reference/reveal_aes.md)\]

- basename:

  The base file name that will be used for saving.

- ...:

  Additional arguments (e.g. width, height) to be passed to
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)

## Value

The paths of the saved plots, invisibly

## Examples

``` r
# Create full plot
library(ggplot2)
data("mtcars")

p <- ggplot(mtcars, aes(mpg, wt,
             color = factor(vs),
             group = factor(vs))) +
  geom_point() +
  geom_smooth(method="lm",
              formula = 'y ~ x',
              linewidth=1) +
  facet_wrap(~am)
p


plot_list <- reveal_groups(p)
plot_list[[1]]

plot_list[[2]]

plot_list[[3]]


# Save plots
reveal_save(plot_list, "myplot.png", width = 8, height = 4, path = tempdir())
#> 
#> ── Saving incremental plots ──
#> 
#> ✔ /tmp/RtmpILxhE1/myplot_0.png
#> ✔ /tmp/RtmpILxhE1/myplot_1.png
#> ✔ /tmp/RtmpILxhE1/myplot_2_last.png

# Clean temp files
file.remove(list.files(path = tempdir(), pattern = "myplot", full.names = TRUE)) 
#> [1] TRUE TRUE TRUE
```
