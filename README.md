
<!-- README.md is generated from README.Rmd. Please edit that file -->

\[WORK IN PROGRESS\]

# ggreveal <img src="man/figures/magic.gif" align="right" alt="" width="200" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/weverthonmachado/ggreveal/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/weverthonmachado/ggreveal/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of `ggreveal` is to make it easy to present data on ggplot
graphs incrementally. *Why* would you want to do that? Because it’s fun,
and often useful in teaching and academic presentations.

You can reveal the data by group:

By panel:

Or by layer:

See [below](#why) for an explanation of why these functions are needed.

## Installation

``` r
remotes::install_github("weverthonmachado/ggreveal")
```

## Usage

Create a ggplot as you would usually do.

``` r
library(palmerpenguins)
library(ggplot2)

p <-  ggplot(penguins[!is.na(penguins$sex),],
             aes(body_mass_g, bill_length_mm,
                 group=sex, color=sex)) +
      geom_point() +
      geom_smooth(method="lm", formula = 'y ~ x', linewidth=1) +
      facet_wrap(~species) +
      theme_minimal()
p
```

<img src="man/figures/example-1.png" width="100%" />

Then use one of `reveal_panels()`, `reveal_groups()` or
`reveal_layers()` to obtain a list of plots that show elements
incrementally.

``` r
library(ggreveal)
plot_list <- reveal_groups(p)
plot_list
#> [[1]]
```

<img src="man/figures/unnamed-chunk-2-1.png" width="70%" />

    #> 
    #> [[2]]

<img src="man/figures/unnamed-chunk-2-2.png" width="70%" />

    #> 
    #> [[3]]

<img src="man/figures/unnamed-chunk-2-3.png" width="70%" />

You can save these graphs using `reveal_save()`, so you can, e.g.,
include them later in slides:

``` r
reveal_save(plot_list, "myplot", width = 8, height = 4)
#> 
#> ── Saving incremental plots ──
#> 
#> ✔ ']8;;file://C:/Users/Macha010/Dropbox/code/ggreveal/myplot_1.pngmyplot_1.png]8;;'
#> ✔ ']8;;file://C:/Users/Macha010/Dropbox/code/ggreveal/myplot_2.pngmyplot_2.png]8;;'
#> ✔ ']8;;file://C:/Users/Macha010/Dropbox/code/ggreveal/myplot_3_full.pngmyplot_3_full.png]8;;'
```

# <a id="why"></a> Wait, can’t ggplot2 do this already?

Yes and no. `ggplot` is composable by design, so it is straightforward
to do some of the things in the package manually, especially the reveal
by layer. For example, you can create the graph in steps and save the
steps separately:

``` r
data("mtcars")
p1 <- ggplot(mtcars, 
            aes(mpg, wt)) +
     geom_point() 
     
p2 <- p1 + geom_smooth() 
```

The problem is: as you add layers and other elements, several visual
aspects of the graph — e.g. range of axes, legends — can (and will
often) change. Some of this is easily solvable (e.g. set `limits` to fix
the axis range), some are not. Showing changes in graph layout as you
add elements is useful if you are teaching *how to make graphs in
ggplot2* (see package
[`flipbookr`](https://github.com/EvaMaeRey/flipbookr), which makes it
even easier), but it is distracting when you want to focus on the
*results* presented in the plot.
