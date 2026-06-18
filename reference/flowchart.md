# Render an Enrollment Flowchart

Computes counts from the pipeline, lays out nodes, and draws an
EQUATOR-style enrollment diagram. This is the primary rendering function
for interactive use; for saving to file with auto-sized dimensions, see
[`flowsave()`](https://phmcc.codeberg.page/selecta/reference/flowsave.md).

## Usage

``` r
flowchart(.flow, engine = c("grid", "dot"), count_first = FALSE, ...)

# S3 method for class 'selecta'
plot(x, engine = c("grid", "dot"), ...)
```

## Arguments

- .flow:

  A `selecta` object created by
  [`enroll()`](https://phmcc.codeberg.page/selecta/reference/enroll.md)
  or
  [`sources()`](https://phmcc.codeberg.page/selecta/reference/sources.md)
  and populated with pipeline steps.

- engine:

  Character. Rendering engine: `"grid"` (default) for base R graphics,
  or `"dot"` to return a Graphviz DOT string (for use with DiagrammeR or
  a locally installed executable).

- count_first:

  Logical. If `TRUE`, side-box labels are rendered as
  `"214 Discontinued"` (bold count before label) rather than the default
  `"Discontinued (n = 214)"`. Applies to all box types. Default `FALSE`.

- ...:

  Additional styling and formatting arguments forwarded to the selected
  engine; arguments an engine does not recognize are ignored.

  For `engine = "grid"`:

  cex, cex_side, cex_phase

  :   Font-size multipliers for the main, side-box, and phase text

  box_fill, phase_fill

  :   Fill colors for boxes and phase strips

  vpad, margin

  :   Vertical spacing between elements and the outer margin, in inches

  font_family

  :   Font family for text

  number_format

  :   Locale-aware count formatter

  For `engine = "dot"`:

  formatting

  :   Label markup: `"plain"` (default) for robust, pixel-accurate
      centering across all fonts, or `"rich"` for HTML-like inline bold
      and italic that match the `grid` engine's typography at the cost
      of small centering drift on non-Helvetica fonts

  bullets

  :   Prefix side-box sub-reasons with a bullet; defaults on under
      `"plain"` (where indentation alone is a weak cue) and off under
      `"rich"`

  font_family, padding_pt, padding_adjust

  :   Font family (default `"Helvetica"`) and the uniform horizontal
      label padding in points (default 14) with its fine adjustment

  ortho

  :   Use orthogonal (right-angled) edge routing

  box_fill, side_fill, border_col, arrow_col

  :   Box, side-box, border, and arrow colors

  source_fill, source_header_fill, source_header_text

  :   Source-box fill, header fill, and header text color

  phase_labels, phase_fill, phase_text_col

  :   Toggle and color the phase-band labels (on by default when the
      flow defines phases; the `dot` engine draws them as horizontal
      left-margin bands rather than the `grid` engine's vertical strips)

  side_gap_in, rank_sep, node_sep

  :   Spacing of side boxes, ranks, and nodes, in inches

  number_format

  :   Locale-aware count formatter, shared with the `grid` engine

- x:

  A `selecta` object.

## Value

For `engine = "grid"`: invisibly returns the computed graph structure (a
list of `nodes`, `edges`, and `phases` data.tables). For
`engine = "dot"`: returns a DOT-language string.

## Details

`flowchart()` is the primary rendering entry point and accepts a
completed pipeline object. The `grid` engine draws the diagram to the
active graphics device using the grid system and is intended for
publication-quality figures with phase strips, precise dimensions, and
locale-aware counts; the `dot` engine instead returns a Graphviz
DOT-language string for prototyping or rendering through external
Graphviz tooling, and draws nothing itself. Styling, font, and
number-format options are forwarded to the chosen engine through `...`;
options unsupported by an engine (for example the phase strips, which
the `dot` engine does not draw) are ignored. `flowchart()` is normally
the last call in a pipeline; for direct file output use
[`flowsave()`](https://phmcc.codeberg.page/selecta/reference/flowsave.md),
and to size a canvas use `recdims`.

## See also

[`flowsave`](https://phmcc.codeberg.page/selecta/reference/flowsave.md)
for saving to file,
[`recdims`](https://phmcc.codeberg.page/selecta/reference/recdims.md)
for dimension recommendations, `plot.selecta` for S3 plot method

Other flowchart output functions:
[`flowsave()`](https://phmcc.codeberg.page/selecta/reference/flowsave.md),
[`print.selecta()`](https://phmcc.codeberg.page/selecta/reference/print.selecta.md),
[`recdims()`](https://phmcc.codeberg.page/selecta/reference/recdims.md),
[`summary.selecta()`](https://phmcc.codeberg.page/selecta/reference/summary.selecta.md)

## Examples

``` r
# Build a flow once, then render it. Most of the package's pipeline
# functions are modular and intended to be composed like this rather
# than run in isolation; see the vignettes for fuller treatments.
flow <- enroll(n = 1200) |>
  phase("Enrollment") |>
  exclude("Excluded", n = 150,
    reasons = c("Did not meet criteria" = 55,
                "Declined to participate" = 48,
                "Other reasons" = 47)) |>
  phase("Allocation") |>
  allocate(labels = c("Treatment", "Control"),
           n = c(520, 530)) |>
  phase("Analysis") |>
  endpoint("Final Analysis")

# The "dot" engine returns a Graphviz DOT string and draws nothing,
# so it runs anywhere without opening a graphics device.
dot <- flowchart(flow, engine = "dot")
substr(dot, 1, 50)
#> [1] "digraph selecta {\n  rankdir=TB;\n  splines=ortho;\n "

# The "grid" engine draws to the active graphics device. These calls are
# guarded with interactive() so they render in an interactive session but
# are skipped during non-interactive documentation builds, where the
# diagram cannot be sized to the page and would render incorrectly.
if (interactive()) {
  flowchart(flow)            # draws to the active device
  plot(flow)                 # plot() is a thin wrapper around flowchart()

  # Locale-aware counts: a European thousands separator.
  enroll(n = 12500) |>
    exclude("Excluded", n = 1450) |>
    endpoint("Analyzed") |>
    flowchart(number_format = "eu")
}
```
