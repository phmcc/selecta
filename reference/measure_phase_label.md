# Measure a (Possibly Wrapped) Phase Label

Returns the rotated-height demand of a phase label and the lines it
splits to. Phase labels are drawn rotated 90 degrees, so the relevant
demand on the strip is the unrotated width of the longest line, plus
vertical padding. Explicit `"\n"` newlines are ALWAYS honored and are
never collapsed. Greedy word-wrapping is applied to each hard-split
segment only when `wrap = TRUE` (with a `max_width_in` cap); the
`max_lines` cap then limits only the *wrap*-generated lines within a
segment, never merging across explicit newlines. Leading/trailing
whitespace around each line is trimmed so a stray space (e.g.
`"A \n test"`) does not inflate the measured width or the rendered line.

## Usage

``` r
measure_phase_label(
  label,
  gp,
  pad_v,
  tw,
  wrap = FALSE,
  max_lines = NA_integer_,
  max_width_in = NULL
)
```

## Arguments

- label:

  Character scalar phase label.

- gp:

  A `gpar` for measurement (font face/size/family).

- pad_v:

  Numeric. Vertical padding added to both ends (inches).

- tw:

  A measurement function `function(label, gp)` returning the unrotated
  text width in inches.

- wrap:

  Logical. If `TRUE`, word-wrap over-long segments. Default `FALSE`
  (explicit newlines still split).

- max_lines:

  Integer or `NA`. Cap on wrap-generated lines per hard segment;
  overflow is collapsed into that segment's final line. `NA` (default)
  means no cap.

- max_width_in:

  Numeric or `NULL`. Wrap cap (inches).

## Value

A list with `lines` (character vector), `n_lines` (integer), and
`height_in` (numeric, the rotated strip height).
