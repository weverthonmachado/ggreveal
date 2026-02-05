# Reveal plots from a patchwork object

Turns a
[patchwork](https://patchwork.data-imaginist.com/reference/patchwork-package.html)
into a list of plots that reveal each child plot incrementally.

## Usage

``` r
reveal_patchwork(pw, order = NULL)
```

## Arguments

- pw:

  A patchwork object

- order:

  (optional) A numeric vector specifying in which order to reveal the
  plots

  For example, if there are three plots in the patchwork,
  `order = c(3, 2, 1)` will invert the order in which they are revealed.

  Any plot not included in the vector will be omitted from the
  incremental plots. E.g.: with `order = c(3, 1)`, the second plot is
  not shown.

  By default, the first plot returned by this function is blank, showing
  layout elements of the patchwork but none of its child plots. To omit
  the blank plot, include `-1`: e.g. `order = c(-1, 3, 1)`, or
  `order = -1`.

## Value

A list of ggplot2 objects
