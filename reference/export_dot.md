# Convert Graph to Graphviz DOT String

Generates a Graphviz DOT-language representation of a computed graph.
Node fill colors match the grid engine: a darker gray with bold black
text for source-column headers, white for source boxes, light gray for
side (exclusion) boxes, and white for everything else. Exclusion
sub-reasons, endpoint breakdowns, and the per-source counts of a
multi-source flow are rendered inside their boxes, so the DOT output
carries the same detail as the grid output.

## Usage

``` r
export_dot(
  graph,
  number_format = NULL,
  count_first = FALSE,
  ortho = TRUE,
  formatting = c("plain", "rich"),
  bullets = NULL,
  font_family = "Helvetica",
  padding_pt = 14,
  padding_adjust = 0,
  box_fill = "#FFFFFF",
  side_fill = "#FFFFFF",
  border_col = "black",
  arrow_col = "black",
  source_fill = "#FFFFFF",
  source_header_fill = "#D0D0D0",
  source_header_text = "black",
  phase_labels = NULL,
  phase_fill = "#000000",
  phase_text_col = "#FFFFFF",
  rank_sep = 0.4,
  node_sep = 0.5
)
```

## Arguments

- graph:

  A computed and laid-out graph.

- number_format:

  Locale-aware count formatter (see
  [`flowchart()`](https://phmcc.codeberg.page/selecta/reference/flowchart.md)).
  Defaults to the `selecta.number_format` option.

- count_first:

  Logical. If `TRUE`, the count appears before the label text in each
  box (*e.g.,* `200 Excluded` instead of `Excluded, n = 200`), matching
  the count-first layout available in the grid engine. Default `FALSE`.

- ortho:

  Logical. If `TRUE` (default), edges are routed at right angles via
  Graphviz's `splines=ortho` attribute. This underpins the canonical
  CONSORT look, in which an exclusion side box hangs off a tick on the
  vertical spine rather than from a diagonal edge. Set to `FALSE` only
  to fall back to spline routing.

- formatting:

  Character string, either `"plain"` (default) or `"rich"`. See Details.

- bullets:

  Logical or `NULL`. Controls whether exclusion sub-reasons (and other
  left-aligned breakdowns inside side and source boxes) are prefixed
  with a bullet. `NULL` (default) selects by mode: `TRUE` for
  `formatting = "plain"`, where indentation alone barely separates a
  sub-reason from its parent label, and `FALSE` for
  `formatting = "rich"`, whose bold parent label already conveys the
  hierarchy. An explicit `TRUE` or `FALSE` overrides the per-mode
  default. Centered breakdowns beneath main and endpoint boxes are never
  bulleted.

- font_family:

  Character string. Graphviz `fontname` value for the body text. Default
  `"Helvetica"`.

- padding_pt:

  Numeric. Horizontal padding applied uniformly on each side of every
  node's text, in points. Default 14.

- padding_adjust:

  Numeric. Additive offset to `padding_pt` for fine-tuning, in points.
  Default 0.

- box_fill:

  Character. Fill color for main boxes. Default `"#FFFFFF"`.

- side_fill:

  Character. Fill color for side (exclusion) boxes. Default `"#FFFFFF"`
  (white), following the EQUATOR convention of plain white boxes
  throughout; set a gray such as `"#F0F0F0"` to shade exclusion boxes.

- border_col:

  Character. Border color for all boxes. Default `"black"`.

- arrow_col:

  Character. Color for arrows and connector lines. Default `"black"`.

- source_fill:

  Character. Fill color for source boxes in multi-source diagrams
  (PRISMA, MOOSE). Default `"#FFFFFF"`, matching the grid engine.

- source_header_fill:

  Character. Fill color for source-column header boxes. Default
  `"#D0D0D0"`, matching the grid engine.

- source_header_text:

  Character. Text color for source-column header labels. Default
  `"black"`, matching the grid engine.

- phase_labels:

  Logical or `NULL`. Whether to render phase labels as left-margin band
  labels. `NULL` (default) auto-selects: on whenever the flow defines
  any phases via
  [`phase()`](https://phmcc.codeberg.page/selecta/reference/phase.md),
  off otherwise. Unlike the grid engine's rotated vertical strips, the
  DOT labels are horizontal (Graphviz cannot rotate node text), placed
  in a left-hand column and rank-aligned to the first row of each band.

- phase_fill:

  Character. Fill color for phase label boxes. Default `"#000000"`
  (black), following the grid standard's black band labels.

- phase_text_col:

  Character. Text color for phase labels. Default `"#FFFFFF"` (white).

- rank_sep:

  Numeric. Graphviz `ranksep` in inches, the vertical separation between
  successive rows (and the half-rows introduced by tick joints). Default
  0.4. Lower values produce a more compact diagram.

- node_sep:

  Numeric. Graphviz `nodesep` in inches, the minimum horizontal
  separation between nodes sharing a rank (arms, source columns, a side
  box and its joint). Default 0.5. This also sets the length of a side
  box's connector arrow (the box hangs one `nodesep` from its stem) and,
  for a box seated in the channel between two arms, the equal gap on
  each side – so the box stays centered between the arms.

## Value

A character string in DOT format.

## Details

The engine has two label-formatting modes selected by the `formatting`
argument:

- `"plain"` (default):

  Labels are emitted as plain DOT text without inline markup. Graphviz
  handles plain text reliably across all backends, producing
  exactly-centered labels at every font and zoom level. Source headers
  receive a bold typeface via a whole-node `fontname` (*e.g.,*
  `"Helvetica-Bold"`) rather than inline `<B>` markup; this preserves
  the visual emphasis without invoking Graphviz's HTML-label code path.

- `"rich"`:

  Labels use HTML-like markup with inline bold for the descriptive text
  and italic for the lowercase *n* in "n = X", matching the typographic
  conventions used by the grid engine and by published EQUATOR diagrams.
  This mode invokes Graphviz's HTML-label code path, whose text-width
  estimator drifts slightly from the actually-rendered glyph widths.
  Width measurement uses embedded Adobe Font Metric (AFM) tables for the
  rendered Helvetica and Times families, with trailing- whitespace
  compensation to recenter the visible glyphs. The result is
  sub-pixel-accurate centering for Helvetica and exact centering for
  Times; other fonts (Courier, system sans-serifs) may show small
  residual drift since their Graphviz HTML-label metrics differ from
  what browsers actually render.

Most users should accept the default `"plain"` formatting, which is the
more robust choice for prototyping and web embedding. The `"rich"` mode
is available for diagrams where the inline italic-*n* and bold-label
typography is essential.
