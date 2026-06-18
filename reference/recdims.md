# Recommended Figure Dimensions

Computes recommended width and height in inches based on diagram
content. A throwaway graphics device is opened to obtain accurate text
measurements, then closed immediately.

## Usage

``` r
recdims(
  x,
  vpad = getOption("selecta.vpad", 0.25),
  pad = 0.08,
  line_height = 0.2,
  count_first = FALSE,
  cex = 0.85,
  cex_side = NULL,
  cex_phase = 0.9,
  phase_width = 0.22,
  margin = 0.25,
  phase_multiline = TRUE,
  phase_max_lines = 3L,
  font_family = "Helvetica",
  number_format = NULL,
  ...,
  .measure_dev = NULL,
  .return_graph = FALSE
)
```

## Arguments

- x:

  A `selecta` object.

- vpad:

  Numeric. Vertical spacing between elements in inches. Default 0.25;
  override globally with `options(selecta.vpad = 0.35)`.

- pad:

  Numeric. Internal padding within boxes in inches. Default 0.08.

- line_height:

  Numeric. Vertical line spacing in inches. Default 0.20.

- count_first:

  Logical. If `TRUE`, measure using the count-first label layout.
  Default `FALSE`.

- cex:

  Numeric. Font size multiplier for main text. Default 0.85.

- cex_side:

  Numeric. Font size multiplier for side box text. Defaults to the value
  of `cex`.

- cex_phase:

  Numeric. Font size multiplier for phase labels. Default 0.9.

- phase_width:

  Numeric. Width of phase label boxes in inches. Default 0.22.

- margin:

  Numeric. Fixed margin on all four sides in inches. Default 0.25.

- phase_multiline:

  Logical. If `TRUE` (the default), long phase labels wrap across
  stacked lines to fit their band; must match the draw-time value for
  accurate dimensions. Default `TRUE`.

- phase_max_lines:

  Integer. Maximum wrapped lines per phase label when wrapping is
  active. Default 3.

- font_family:

  Character. Font family for text measurement. Default `"Helvetica"`.
  Must match the value used at draw time for accurate dimensions.

- number_format:

  Character string or two-element character vector. Locale-aware count
  formatter passed through to
  [`export_grid()`](https://phmcc.codeberg.page/selecta/reference/export_grid.md)
  for accurate text measurement. See
  [`flowchart()`](https://phmcc.codeberg.page/selecta/reference/flowchart.md)
  for accepted values.

- ...:

  Additional arguments. Styling-only parameters that do not affect text
  measurement (such as `box_fill`, `phase_fill`, `border_col`) are
  silently ignored, allowing the same call signature to be shared with
  [`flowchart()`](https://phmcc.codeberg.page/selecta/reference/flowchart.md)
  and
  [`flowsave()`](https://phmcc.codeberg.page/selecta/reference/flowsave.md).

- .measure_dev:

  Optional zero-argument function that opens a graphics device for text
  measurement, matching the device that will render the diagram. When
  `NULL` (the default) a pdf device is used. Advanced use only; see
  Details.

- .return_graph:

  Logical. If `TRUE`, attaches the pre-computed graph as an attribute
  for reuse by
  [`flowsave()`](https://phmcc.codeberg.page/selecta/reference/flowsave.md).
  Default `FALSE`. Internal use only.

## Value

A named numeric vector with elements `width` and `height` (in inches),
rounded up to the nearest tenth.

## Details

`recdims()` computes the canvas size a flow needs at a given typography
and layout, so the figure is neither clipped nor surrounded by excess
whitespace. It lays the diagram out and measures it on a throwaway
graphics device, returning width and height in inches without drawing
anything visible. Because text metrics are font- and device-dependent,
any sizing parameter passed here (`cex`, `font_family`,
`phase_multiline`, `number_format`, and so on) should match the values
used at render time; styling-only parameters are ignored so the same
call can be shared across `recdims()`,
[`flowchart()`](https://phmcc.codeberg.page/selecta/reference/flowchart.md),
and
[`flowsave()`](https://phmcc.codeberg.page/selecta/reference/flowsave.md).
The advanced `.measure_dev` argument supplies a custom device opener
when measurement must match a non-default device.
[`flowsave()`](https://phmcc.codeberg.page/selecta/reference/flowsave.md)
calls `recdims()` internally when `width` or `height` is left
unspecified, so explicit use is only needed when the dimensions
themselves are wanted.

## See also

[`flowsave`](https://phmcc.codeberg.page/selecta/reference/flowsave.md)
for saving to file,
[`flowchart`](https://phmcc.codeberg.page/selecta/reference/flowchart.md)
for interactive rendering

Other flowchart output functions:
[`flowchart()`](https://phmcc.codeberg.page/selecta/reference/flowchart.md),
[`flowsave()`](https://phmcc.codeberg.page/selecta/reference/flowsave.md),
[`print.selecta()`](https://phmcc.codeberg.page/selecta/reference/print.selecta.md),
[`summary.selecta()`](https://phmcc.codeberg.page/selecta/reference/summary.selecta.md)

## Examples

``` r
flow <- enroll(n = 500) |>
  exclude("Ineligible", n = 65) |>
  allocate(labels = c("Drug A", "Placebo"), n = c(220, 215)) |>
  endpoint("Analyzed")

recdims(flow)
#>  width height 
#>    3.6    4.4 
```
