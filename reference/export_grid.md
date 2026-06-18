# Draw Enrollment Diagram via Grid Graphics

Computes all layout in inches using physical text measurements, then
renders the diagram within a fixed-margin viewport. Intended to be
called by
[`flowchart()`](https://phmcc.codeberg.page/selecta/reference/flowchart.md)
or
[`flowsave()`](https://phmcc.codeberg.page/selecta/reference/flowsave.md)
rather than directly.

## Usage

``` r
export_grid(
  graph,
  cex = 0.85,
  cex_side = NULL,
  cex_phase = 0.9,
  box_fill = "white",
  side_fill = "white",
  border_col = "black",
  arrow_col = "black",
  phase_fill = "black",
  phase_text_col = "white",
  lwd = 1,
  count_first = FALSE,
  newpage = TRUE,
  vpad = getOption("selecta.vpad", 0.25),
  pad = 0.08,
  line_height = 0.2,
  margin = 0.25,
  phase_width = 0.22,
  phase_multiline = TRUE,
  phase_max_lines = 3L,
  font_family = "Helvetica",
  number_format = NULL,
  measure_only = FALSE
)
```

## Arguments

- graph:

  A laid-out graph (output of
  [`layout_nodes()`](https://phmcc.codeberg.page/selecta/reference/layout_nodes.md)).

- cex:

  Numeric. Font size multiplier for main box text. Default 0.85.

- cex_side:

  Numeric. Font size multiplier for side box text. Defaults to the value
  of `cex`.

- cex_phase:

  Numeric. Font size multiplier for phase labels. Default 0.9.

- box_fill:

  Character. Fill color for main boxes. Default `"white"`.

- side_fill:

  Character. Fill color for side (exclusion) boxes. Default `"white"`.

- border_col:

  Character. Border color for all boxes. Default `"black"`.

- arrow_col:

  Character. Color for arrows and connector lines. Default `"black"`.

- phase_fill:

  Character. Background color for phase label boxes. Default `"black"`.

- phase_text_col:

  Character. Text color for phase labels. Default `"white"`.

- lwd:

  Numeric. Line width for borders and arrows. Default 1.

- count_first:

  Logical. If `TRUE`, side-box labels are rendered as
  `"214 Discontinued"` (bold count before label) rather than the default
  `"Discontinued (n = 214)"`. Default `FALSE`.

- newpage:

  Logical. If `TRUE`, calls `grid.newpage()` before drawing. Default
  `TRUE`.

- vpad:

  Numeric. Vertical spacing between elements in inches. Controls the
  uniform gap between any box edge and the next adjacent element.
  Default 0.25; override globally with `options(selecta.vpad = 0.35)`.

- pad:

  Numeric. Internal padding within boxes in inches. Default 0.08.

- line_height:

  Numeric. Vertical line spacing in inches, controlling box heights for
  both main and side boxes. Scales proportionally with `cex`. Default
  0.20.

- margin:

  Numeric. Fixed margin on all four sides of the canvas in inches.
  Default 0.25.

- phase_width:

  Numeric. Width of phase label boxes in inches. Default 0.22. When
  `phase_multiline = TRUE` the strip is widened automatically to fit the
  wrapped lines, so this acts as a per-line minimum rather than a hard
  cap.

- phase_multiline:

  Logical. If `TRUE` (the default), a phase label longer than the
  vertical extent of the boxes it spans is word-wrapped across multiple
  stacked lines (drawn rotated in the strip), trading strip width for
  height so the diagram is not stretched vertically to fit a long
  rotated label. Set to `FALSE` to force every label onto a single line,
  in which case a label taller than its band stretches the diagram
  instead. A label that cannot be wrapped (a single word taller than its
  band) falls back to stretching either way. Labels containing an
  explicit newline (`"\n"`) are always split on it regardless of this
  setting. Default `TRUE`.

- phase_max_lines:

  Integer. Maximum number of wrapped lines per phase label when wrapping
  is active; any overflow is collapsed into the final line. Default 3.

- font_family:

  Character. Font family used for all text in the diagram. Default
  `"Helvetica"`. Set to `""` to use the device default.

- number_format:

  Character string or two-element character vector. Locale-aware
  formatting for participant counts: `"us"` (default, `1,234`), `"eu"`
  (`1.234`), `"space"` (`1\u202F234`), `"none"` (`1234`), or a custom
  `c(big.mark, decimal.mark)` pair. Falls back to
  `getOption("selecta.number_format", "us")` when `NULL`.

- measure_only:

  Logical. When `TRUE`, the function computes the layout and canvas
  dimensions but returns before issuing any drawing primitives, so no
  graphics output is produced. Used internally by
  [`recdims()`](https://phmcc.codeberg.page/selecta/reference/recdims.md)
  to size the canvas without the cost of rendering. Defaults to `FALSE`.

## Value

Invisibly returns the graph, augmented with computed layout dimensions
(`diagram_width_in`, `diagram_height_in`).
