# ggplotStudioR

`ggplotStudioR` is an interactive editor for existing `ggplot2` objects.

This redesign moves beyond fixed sliders into a **selection-driven editor**:

- Click elements on the canvas (title, axes, panel, grid, legend, layers)
- Edit via a context-aware inspector
- Persist edits in a structured spec model
- Export reproducible code in two modes

## What is new in this redesign

- **Spec model** (`studio_spec_*`) for deterministic, serializable edits
- **Apply engine** (`apply_studio_spec()`) that replays spec onto the base plot
- **SVG + registry scene model** (`build_svg_scene()`) used for click mapping
- **Canvas module** with click-to-select and overlap cycling
- **Inspector module** for context-sensitive controls
- **Codegen module** with:
  - `additive` mode (create `edited_plot` from base plot)
  - `patch` mode (mutate plot object in-place)

## Editable targets currently supported

- Labels/text: title, subtitle, caption, x title, y title
- Theme blocks:
  - panel background / plot background
  - panel grid major / minor
  - legend container / legend title / legend text
- Layers (coarse layer-level):
  - visibility
  - `alpha`, `size`, `linewidth`, `linetype`

## Representability constraints (important)

`ggplot2` is declarative and some rendered details are not uniquely reversible.
This version intentionally supports a robust representable subset:

- Layer selection is **coarse layer-level** (not per-point drag editing)
- Layer hit-testing is based on built data ranges and panel mapping (Cartesian-first)
- Unknown/unsupported aesthetic parameters are preserved, not rewritten
- Hidden layers are represented via layer filtering in generated code

These constraints are documented so future drag/point-level editing can be layered in cleanly.

## Install locally

```r
# from workspace root
install.packages("ggplotStudioR", repos = NULL, type = "source")
```

## Usage

```r
library(ggplot2)
library(ggplotStudioR)

p <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
  geom_point() +
  geom_smooth(se = FALSE)

launch(p)
```

## Export modes

Inside the app, choose one mode:

- **Additive ggplot**: writes code that constructs `edited_plot` from your base object
- **Patch script**: writes code that mutates your base plot symbol directly

Both modes support clipboard copy + file export.

## Walkthrough and docs

- Article: `vignettes/end-to-end-demo.Rmd`
- Architecture notes: `vignettes/architecture-and-representability.Rmd`

### Demo GIF

![ggplotStudioR live demo](vignettes/figures/ggplotStudioR-live-demo.gif)
