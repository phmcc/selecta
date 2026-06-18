# Merge Parallel Streams

Converges all active parallel streams into a single flow. Used to handle
either source convergence or split-and-recombine topologies. After
[`stratify()`](https://phmcc.codeberg.page/selecta/reference/stratify.md),
recombines strata that were characterized independently back into a
unified downstream flow.

## Usage

``` r
combine(.flow, label, sublabel = NULL, n = NULL, reasons = NULL)
```

## Arguments

- .flow:

  A `selecta` object with active parallel streams (from
  [`sources()`](https://phmcc.codeberg.page/selecta/reference/sources.md)
  or
  [`stratify()`](https://phmcc.codeberg.page/selecta/reference/stratify.md)).

- label:

  Character string for the merged node.

- sublabel:

  Optional character string rendered below `label` inside the same box.
  Useful for describing the recombined cohort.

- n:

  Integer. Explicit post-merge count (manual mode). If omitted, computed
  as the sum of all active stream counts.

- reasons:

  Optional named integer vector of sub-items displayed below the count
  (*e.g.,* outcome categories).

## Value

The updated `selecta` object with a combine step appended. All
subsequent steps operate on the single merged stream.

## Details

`combine()` converges the active parallel streams into one node and is
the counterpart to both entry splits. After
[`sources()`](https://phmcc.codeberg.page/selecta/reference/sources.md),
it pools the identification streams of a systematic review; after
[`stratify()`](https://phmcc.codeberg.page/selecta/reference/stratify.md)
(or
[`allocate()`](https://phmcc.codeberg.page/selecta/reference/stratify.md)),
it recombines strata that were handled independently, producing a
split-and-recombine diagram.

By default, the merged count is the sum of the incoming streams after
any per-arm exclusions applied since the split—an explicit `n` overrides
this in manual mode. In such situations, an additional option is
provided (`getOption("selecta.check_arithmetic")`, default `TRUE`),
which will check arithmetic and raise an advisory warning if there is a
discrepancy between counts.

The optional `sublabel` parameter prints on a second line inside the
merged box, which is convenient for naming the recombined cohort.

## See also

[`sources`](https://phmcc.codeberg.page/selecta/reference/sources.md)
for multi-source entry,
[`stratify`](https://phmcc.codeberg.page/selecta/reference/stratify.md)
for split-and-recombine flows

Other flow construction functions:
[`assess()`](https://phmcc.codeberg.page/selecta/reference/assess.md),
[`endpoint()`](https://phmcc.codeberg.page/selecta/reference/endpoint.md),
[`enroll()`](https://phmcc.codeberg.page/selecta/reference/enroll.md),
[`exclude()`](https://phmcc.codeberg.page/selecta/reference/exclude.md),
[`phase()`](https://phmcc.codeberg.page/selecta/reference/phase.md),
[`sources()`](https://phmcc.codeberg.page/selecta/reference/sources.md),
[`stratify()`](https://phmcc.codeberg.page/selecta/reference/stratify.md)

## Examples

``` r
# PRISMA: merge identification sources
sources(PubMed = 1234, Embase = 567) |>
  combine("Records after deduplication") |>
  exclude("Records removed", n = 352, show_count = FALSE,
          reasons = c("Duplicates" = 340, "Automation" = 12))
#> selecta flow (manual mode)
#>   Starting N: 1,801
#>   Steps: 3
#>   [1] sources:
#>          PubMed (n = 1,234)
#>          Embase (n = 567)
#>   [2] combine: "Records after deduplication"
#>   [3] exclude: "Records removed" (n = 352)
#>          • Duplicates = 340
#>          • Automation = 12
#> 

# Split-and-recombine: stratify, then combine
enroll(n = 158) |>
  stratify(labels = c("Not screened", "Screened"), n = c(82, 76),
           label = "Screening status") |>
  exclude("Condition not confirmed", n = c(44, 66)) |>
  combine("Confirmed cohort",
          sublabel = "Participants with confirmed diagnosis") |>
  exclude("Incomplete records", n = 7) |>
  endpoint("Final cohort")
#> selecta flow (manual mode)
#>   Starting N: 158
#>   Steps: 5
#>   [1] stratify: Not screened, Screened
#>          label: "Screening status"
#>   [2] exclude: "Condition not confirmed" (n = 110)
#>   [3] combine: "Confirmed cohort"
#>          "Participants with confirmed diagnosis"
#>   [4] exclude: "Incomplete records" (n = 7)
#>   [5] endpoint: "Final cohort"
```
