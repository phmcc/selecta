# Build a Plain-Label DOT Node Emitter

Produces a closure emitting one plain DOT node-statement per call.
Source headers receive a bold variant of the body font via the per-node
`fontname`, which Graphviz measures accurately.

## Usage

``` r
build_plain_emitter(
  fn,
  count_first,
  font_family,
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

- font_family:

  Character body font family.

- box_fill, side_fill, source_fill:

  Fill colors for main, side, and source boxes.

- source_header_fill, source_header_text:

  Fill and text colors for source-header boxes.

## Value

A function of a single node row returning a DOT node-statement.
