# Place Rows in Inches (Top-Down)

Single monotone top-down placement of every node in distance-from-top
inches, used by the phase-fit pass to measure phase extents from actual
node positions. Anchoring (non-side) boxes sit at their row centers;
side boxes hang `vpad_in` below their exclude-edge parent and stack
downward, exactly as in the main rendering pass – so a phase's measured
extent includes side boxes that hang off a neighboring phase's row.

## Usage

``` r
place_rows_in(
  nodes,
  edges,
  row_h_in,
  pair_gap_in,
  n_rows,
  vpad_in,
  lead_in = 0
)
```

## Arguments

- nodes:

  Node `data.table` with `node_id`, `role`, `row`, `bh_inches`.

- edges:

  Edge `data.table` with `edge_type`, `from`, `to`.

- row_h_in:

  Numeric vector of row heights (inches), length n_rows.

- pair_gap_in:

  Numeric vector of gaps below each row (inches).

- n_rows:

  Integer number of rows.

- vpad_in:

  Numeric vertical pad (inches).

- lead_in:

  Numeric leading pad above the first row (inches).

## Value

A list with `top`, `bot` (numeric vectors aligned to `nodes` row order),
`d_row`, and `bottom_in`.
