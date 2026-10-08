# Layout Nodes for Grid Rendering

Assigns row (vertical position) and preliminary x (horizontal) positions
to all nodes. Handles multi-source streams (from
[`sources()`](https://phmcc.codefloe.page/selecta/reference/sources.md)),
arm splits (from
[`stratify()`](https://phmcc.codefloe.page/selecta/reference/stratify.md)),
and classification grids.

## Usage

``` r
layout_nodes(graph)
```

## Arguments

- graph:

  List from
  [`compute()`](https://phmcc.codefloe.page/selecta/reference/compute.md).

## Value

The graph with `x` and `row` columns on `nodes`.
