# Per-Phase Band Deficits

Lays the rows out naturally (in inches) and returns, for each phase, the
vertical deficit `D_i = max(0, label_height_i - natural_band_i)`. The
natural band is the phase's slice of the diagram: the two terminal
phases extend `vpad_in/4` past the outermost node, and interior
boundaries fall at the half-way line between neighboring phase content
but stop `ph_gap_in/2` short on each side so adjacent strips are
separated by `ph_gap_in`. Phase extents are measured from final node
positions, so a side box hanging off a neighboring phase's row is
attributed to its own phase. These deficits are consumed by
[`apply_phase_bands()`](https://phmcc.codeberg.page/selecta/reference/apply_phase_bands.md);
their sum is the extra canvas height needed.

## Usage

``` r
phase_band_deficits(
  nodes,
  edges,
  phases,
  row_h_in,
  pair_gap_in,
  n_rows,
  vpad_in,
  ph_gap_in,
  label_h_in
)
```

## Arguments

- nodes, edges, phases:

  Graph components.

- row_h_in, pair_gap_in:

  Natural row heights and gaps (inches).

- n_rows:

  Integer row count.

- vpad_in:

  Numeric vertical pad (inches); terminal overhang is `vpad_in/4`.

- ph_gap_in:

  Numeric separation between adjacent strips (inches).

- label_h_in:

  Numeric vector (one per phase) of required band heights (rotated label
  height incl. padding).

## Value

Numeric vector of length `nrow(phases)` of deficits (in).
