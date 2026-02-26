# ggplotStudioR

`ggplotStudioR` is a minimal R package MVP for interactive editing of `ggplot2` objects with Shiny.

## Features

- Live plot preview while editing
- Label controls: title, subtitle, caption, x/y axis labels
- Theme presets: gray, bw, minimal, classic, light, dark, void
- Palette controls for mapped color/fill aesthetics (`Set1`, `Dark2`, `viridis`)
- Point and line size controls
- Generated ggplot2 code view
- Export code to file and copy to clipboard
- Extensible architecture hooks for future drag gestures (`editor_extensions()`)

## Install Locally

```r
# from workspace root
install.packages("ggplotStudioR", repos = NULL, type = "source")
```

## Usage

```r
library(ggplot2)
library(ggplotStudioR)

p <- ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
  geom_point() +
  geom_smooth(se = FALSE)

launch(p)
```

## Documentation

- pkgdown article: [End-to-end demo](articles/end-to-end-demo.html)
- Source vignette: [`vignettes/end-to-end-demo.Rmd`](vignettes/end-to-end-demo.Rmd)

## Notes

Generated code assumes your original plot object is named `p` and includes comments for geometry sizing persistence.
