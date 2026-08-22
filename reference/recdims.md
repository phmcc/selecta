# Recommended Figure Dimensions

Computes recommended width and height in inches based on diagram
content. A throwaway graphics device is opened to obtain accurate text
measurements, then closed immediately.

## Usage

``` r
recdims(
  x,
  vpad = NULL,
  pad = NULL,
  line_height = NULL,
  count_first = NULL,
  cex = NULL,
  cex_side = NULL,
  cex_phase = NULL,
  phase_width = NULL,
  margin = NULL,
  phase_multiline = NULL,
  phase_max_lines = NULL,
  font_family = NULL,
  number_format = NULL,
  units = c("in", "cm", "mm"),
  ...,
  .measure_dev = NULL,
  .return_graph = FALSE
)
```

## Arguments

- x:

  A `selecta` object.

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

- count_first:

  Logical. If `TRUE`, side-box labels are rendered as
  `"214 Discontinued"` (bold count before label) rather than the default
  `"Discontinued (n = 214)"`. Default `FALSE`.

- cex:

  Numeric. Font size multiplier for main box text. Default 0.85.

- cex_side:

  Numeric. Font size multiplier for side box text. Defaults to the value
  of `cex`.

- cex_phase:

  Numeric. Font size multiplier for phase labels. Default 0.9.

- phase_width:

  Numeric. Width of phase label boxes in inches. Default 0.22. When
  `phase_multiline = TRUE` the strip is widened automatically to fit the
  wrapped lines, so this acts as a per-line minimum rather than a hard
  cap.

- margin:

  Numeric. Fixed margin on all four sides of the canvas in inches.
  Default 0.25.

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

- units:

  Character string giving the units the dimensions are returned in:
  `"in"` (inches, the default), `"cm"`, or `"mm"`.

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

A named numeric vector with elements `width` and `height`, expressed in
`units` and rounded up to the nearest tenth. The units are recorded on
the result as a `"units"` attribute, so a value carried between
functions remains self-describing.

## Details

`recdims()` computes the canvas size a flow needs at a given typography
and layout, so the figure is neither clipped nor surrounded by excess
whitespace. It lays the diagram out and measures it on a throwaway
graphics device, returning width and height without drawing anything
visible. Measurement is performed in inches and the result converted to
`units`, with the rounding applied after conversion so the returned
tenth is a tenth of the reported unit. Because text metrics are font-
and device-dependent, any sizing parameter passed here (`cex`,
`font_family`, `phase_multiline`, `number_format`, and so on) should
match the values used at render time; styling-only parameters are
ignored so the same call can be shared across `recdims()`,
[`flowchart()`](https://phmcc.codeberg.page/selecta/reference/flowchart.md),
and
[`flowsave()`](https://phmcc.codeberg.page/selecta/reference/flowsave.md).
A parameter left unspecified is not defaulted here but forwarded unset,
so it is measured at exactly the value the drawing routine will apply.
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
#> attr(,"units")
#> [1] "in"

# Journals commonly specify figure widths in millimeters.
recdims(flow, units = "mm")
#>  width height 
#>   90.8  110.5 
#> attr(,"units")
#> [1] "mm"
```
