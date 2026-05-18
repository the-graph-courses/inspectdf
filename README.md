# inspectdf

Minimal replacement for the course functions used from `{inspectdf}`:

- `inspect_cat()`
- `inspect_num()`
- `show_plot(inspect_cat(...))`
- `show_plot(inspect_num(...))`

The GitHub repository and installed R package are both named `{inspectdf}`, so
existing course code can keep using `library(inspectdf)` and
`inspectdf::inspect_cat()`.

## Why this exists

The upstream `{inspectdf}` package has been fragile with newer `{dplyr}`
versions in course examples like:

```r
inspect_cat(my_data) |>
  show_plot()
```

This package avoids the upstream dplyr-heavy plotting path and implements only
the course workflow we need.

## Install

Remove the old `{inspectdf}` first, then install this replacement from GitHub
with `{pacman}`.

```r
if ("inspectdf" %in% rownames(installed.packages())) {
  remove.packages("inspectdf")
}

pacman::p_install_gh("the-graph-courses/inspectdf")
library(inspectdf)
```

If you prefer `{pak}`:

```r
if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak")
}

pak::pak("the-graph-courses/inspectdf")
library(inspectdf)
```

## Examples

```r
inspect_cat(iris) |>
  show_plot()

inspect_num(iris) |>
  show_plot()
```

Existing course examples also work with the tidyverse pipe:

```r
library(dplyr)
library(inspectdf)

starwars %>%
  inspect_cat() %>%
  show_plot()
```
