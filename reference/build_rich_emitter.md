# Build a Rich HTML-Label DOT Node Emitter

Emits HTML-like labels with inline bold/italic markup and a calibrated
trailing-whitespace span compensating for Graphviz's bold-text width
underestimate on the SVG backend. Width measurement uses embedded AFM
tables for the supported font families.

## Usage

``` r
build_rich_emitter(
  fn,
  count_first,
  is_times,
  is_courier,
  font_family,
  padding_pt,
  font_size_pt,
  box_fill,
  side_fill,
  source_fill,
  source_header_fill,
  source_header_text,
  bullets = FALSE
)
```

## Arguments

- fn:

  Count-formatting function.

- count_first:

  Logical; place the count before the label text.

- is_times, is_courier:

  Logical flags for the active font family.

- font_family:

  Character body font family.

- padding_pt, font_size_pt:

  Numeric horizontal padding and font size in points.

- box_fill, side_fill, source_fill:

  Fill colors for main, side, and source boxes.

- source_header_fill, source_header_text:

  Fill and text colors for source-header boxes.

## Value

A function of a single node row returning a DOT node-statement.
