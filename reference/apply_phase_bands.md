# Apply Phase Bands (Grow, Translate, Place Content)

Given nodes already positioned in content NPC and a per-phase deficit
vector, grows each phase band by its own deficit, rigidly translates
every later phase downward by the cumulative deficit above it, and
vertically recenters the whole (taller) diagram. Band geometry mirrors
[`phase_band_deficits()`](https://phmcc.codeberg.page/selecta/reference/phase_band_deficits.md):
the two terminal phases overhang the outermost node by `vpad/4`, and
adjacent strips are separated by `ph_gap`. Within a band the content is
placed by:

- **no deficit** – natural node positions are preserved (so the terminal
  overhang stays exactly `vpad/4`); the block is simply translated into
  its grown/recenterd band.

- **deficit** – the band's elements (distinct rows, a two-arm row
  counting as one) are spread to *equal gaps*: with \\m\\ elements there
  are \\m+1\\ equal slots (above, between each pair, and below), so e.g.
  a two-element phase seats its boxes at the 1/3 and 2/3 marks.

Because each band grows only by its own deficit and neighbors are merely
translated, growing one phase never alters another's band height (no
bystander stretch). Node `y` values are updated in place; per-phase band
top/bottom edges (NPC) are returned for the strip-drawing pass.

## Usage

``` r
apply_phase_bands(
  nodes,
  edges,
  phases,
  deficit_in,
  to_npc_h,
  to_npc_w,
  vpad_in,
  ph_gap_in
)
```

## Arguments

- nodes:

  Node `data.table` with `y`, `box_h`, `row`, `phase`, `role`, `node_id`
  (modified in place).

- edges:

  Edge `data.table` (`edge_type`, `from`, `to`); currently unused for
  placement but kept for signature stability with
  [`phase_band_deficits()`](https://phmcc.codeberg.page/selecta/reference/phase_band_deficits.md).

- phases:

  Phase table with `phase_start`, `phase_end`.

- deficit_in:

  Numeric per-phase deficit (inches) from
  [`phase_band_deficits()`](https://phmcc.codeberg.page/selecta/reference/phase_band_deficits.md).

- to_npc_h, to_npc_w:

  Inch-\>NPC converters (height, width).

- vpad_in:

  Numeric vertical pad (inches); terminal overhang is `vpad_in/4`.

- ph_gap_in:

  Numeric separation between adjacent strips (inches).

## Value

A list with `band_top` and `band_bot`: numeric vectors (length
`nrow(phases)`) of each phase strip's top and bottom edge in NPC.
