# Save Diagram to File

Renders the enrollment diagram and saves it to a file. Supported formats
are PDF, PNG, SVG, and TIFF (inferred from the file extension). The
`grid` engine renders via R graphics devices; the `dot` engine pipes
Graphviz output through the system `dot` binary. Dimensions are computed
automatically from diagram content via
[`recdims()`](https://phmcc.codeberg.page/selecta/reference/recdims.md)
unless overridden.

## Usage

``` r
flowsave(
  x,
  file,
  engine = c("grid", "dot"),
  width = NULL,
  height = NULL,
  units = c("in", "cm", "mm"),
  dpi = 300,
  sans_serif = TRUE,
  quiet = FALSE,
  ...
)
```

## Arguments

- x:

  A `selecta` object.

- file:

  Character string. Output file path. The format is inferred from the
  file extension. Supported extensions: `.pdf`, `.png`, `.svg`,
  `.tif`/`.tiff` (all engines); `.dot` (`dot` engine only, writes the
  raw DOT source).

- engine:

  Character string. One of `"grid"` (the default, uses R's grid
  graphics) or `"dot"` (uses the system Graphviz binary). The `dot`
  engine requires `dot` to be installed and on the system `PATH`.

- width:

  Numeric or `NULL`. Width in `units`. If `NULL` (default), computed
  automatically. For the `dot` engine, omit to let Graphviz determine
  dimensions from layout.

- height:

  Numeric or `NULL`. Height in `units`. If `NULL` (default), computed
  automatically. For the `dot` engine, omit to let Graphviz determine
  dimensions from layout.

- units:

  Character string giving the units of `width` and `height`, and of the
  dimensions computed when either is left unspecified: `"in"` (inches,
  the default), `"cm"`, or `"mm"`. Graphics devices are driven in inches
  regardless, so the conversion is internal. Ignored by the `dot`
  engine, which takes no dimensions.

- dpi:

  Integer. Resolution in dots per inch for raster formats (PNG, TIFF).
  Default 300. Honored by both engines. Mirrors the `dpi` argument of
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).

- sans_serif:

  Logical. `dot` engine only. If `TRUE` (default), the rendered SVG/PDF
  text is displayed in a sans-serif fallback chain
  (`Helvetica, Arial, "Liberation Sans", "DejaVu Sans", sans-serif`)
  regardless of the layout font. Layout boxes are still sized using the
  metrics of the font set via `font_family`, so the result preserves all
  margins. Set to `FALSE` to retain the layout font as the displayed
  font.

- quiet:

  Logical. Suppress the message reporting the file written and the
  dimensions used. The `dot` engine reports neither, so the setting has
  no effect there. Default `FALSE`.

- ...:

  Additional styling and formatting arguments forwarded to the selected
  engine; see
  [`flowchart()`](https://phmcc.codeberg.page/selecta/reference/flowchart.md)
  for the full descriptions.

  `engine = "grid"`

  :   `cex`, `cex_side`, `cex_phase`, `box_fill`, `phase_fill`, `vpad`,
      `margin`, `font_family`, `number_format`

  `engine = "dot"`

  :   `formatting`, `bullets`, `count_first`, `number_format`, `ortho`,
      `font_family`, `padding_pt`, `padding_adjust`, `box_fill`,
      `side_fill`, `border_col`, `arrow_col`, `source_fill`,
      `source_header_fill`, `source_header_text`, `phase_labels`,
      `phase_fill`, `phase_text_col`, `rank_sep`, `node_sep`

## Value

Invisibly returns the output file path.

## Details

`flowsave()` renders a flow directly to a file, inferring the format
from the extension and choosing dimensions automatically unless `width`
and `height` are given. With `engine = "grid"` it draws through R's
graphics devices, producing either vector formats (`.pdf`, `.svg`) or
raster formats (`.png`, `.tiff`).

For raster formats, `flowsave()` prefers the ragg device when installed,
with fallback to the base
[`png()`](https://rdrr.io/r/grDevices/png.html)/[`tiff()`](https://rdrr.io/r/grDevices/png.html)
devices otherwise. Using these devices is generally advised for raster
output over other devices such as cairo since some cairo configurations
drop the plotmath italics in the count labels. The `dpi` argument
mirrors
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
for raster resolution.

With `engine = "dot"`, `flowsave()` renders a graphic based on a
Graphviz DOT string: a `.dot` extension writes the source text directly
and needs no external software, whereas image output shells out to the
system `dot` binary and therefore requires Graphviz on the `PATH`.

When sizing automatically, `flowsave()` calls
[`recdims()`](https://phmcc.codeberg.page/selecta/reference/recdims.md)
once and reuses the computed layout, so a separate
[`recdims()`](https://phmcc.codeberg.page/selecta/reference/recdims.md)
call is unnecessary. With the `grid` engine, the file written and the
dimensions used are reported through a
[`message()`](https://rdrr.io/r/base/message.html) unless
`quiet = TRUE`, whether those dimensions were computed or supplied, so
that a figure written at an unexpected size is apparent at the point it
is written. The `dot` engine instead lets Graphviz size the output from
the layout, so it reports nothing.

## See also

[`flowchart`](https://phmcc.codeberg.page/selecta/reference/flowchart.md)
for interactive rendering,
[`recdims`](https://phmcc.codeberg.page/selecta/reference/recdims.md)
for dimension recommendations

Other flowchart output functions:
[`flowchart()`](https://phmcc.codeberg.page/selecta/reference/flowchart.md),
[`print.selecta()`](https://phmcc.codeberg.page/selecta/reference/print.selecta.md),
[`recdims()`](https://phmcc.codeberg.page/selecta/reference/recdims.md),
[`summary.selecta()`](https://phmcc.codeberg.page/selecta/reference/summary.selecta.md)

## Examples

``` r
flow <- enroll(n = 500) |>
  exclude("Ineligible", n = 50) |>
  endpoint("Analysis")

# \donttest{
# Grid engine (default). Files are written under tempdir() here so
# the example respects CRAN's no-write policy; in practice any
# desired path may be supplied.
flowsave(flow, file.path(tempdir(), "consort.pdf"))
#> Flowchart saved to /tmp/RtmpjE74Cz/consort.pdf (width = 3.4 in, height = 2.5 in)
flowsave(flow, file.path(tempdir(), "consort.png"),
         width = 8, height = 10)
#> Flowchart saved to /tmp/RtmpjE74Cz/consort.png (width = 8.0 in, height = 10.0 in)

# Dimensions may be given, or computed, in metric units.
flowsave(flow, file.path(tempdir(), "consort_metric.pdf"),
         width = 180, height = 240, units = "mm")
#> Flowchart saved to /tmp/RtmpjE74Cz/consort_metric.pdf (width = 180.0 mm, height = 240.0 mm)

# Suppress the message reporting the file written.
flowsave(flow, file.path(tempdir(), "consort_quiet.pdf"), quiet = TRUE)
# }

# \donttest{
# DOT engine writing a .dot source file requires no external software.
flowsave(flow, file.path(tempdir(), "consort.dot"), engine = "dot")

# Rasterized DOT output (.svg, .png, .pdf) requires the Graphviz 'dot'
# binary on the system PATH.
if (nzchar(Sys.which("dot"))) {
  flowsave(flow, file.path(tempdir(), "consort.svg"), engine = "dot")

  # DOT engine with Times typography for serif environments.
  flowsave(flow, file.path(tempdir(), "consort_times.svg"), engine = "dot",
           font_family = "Times-Roman",
           sans_serif  = FALSE)
}
# }
```
