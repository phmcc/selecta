# Initialize a Multi-Source Flow

Entry point for flows that begin with multiple parallel identification
streams, such as systematic review diagrams. Each named argument defines
a source *group* (column). Individual databases or registers within each
group are listed as sub-items inside a single box, mirroring the format
of exclusion reasons.

## Usage

``` r
sources(..., headers = NULL)
```

## Arguments

- ...:

  Named integer vectors specifying sources. Each argument name
  identifies a group and its named elements are individual sources
  (*e.g.,* `databases = c("PubMed" = 1234, "Embase" = 567)`). Scalar
  named arguments are treated as individual sources in a single default
  group.

- headers:

  Named character vector mapping group names to column header labels.
  For example,
  `headers = c(databases = "Databases and registers", other = "Other methods")`.
  If omitted, the argument names are title-cased and used as headers.

## Value

An object of class `"selecta"` with a `sources` step pre-loaded. The
total starting count is the sum of all source counts across all groups.

## Details

`sources()` initializes a multi-source flow of the kind used in the
identification stage of systematic-review diagrams (PRISMA, MOOSE),
where records arrive from several origins and are pooled. Counts are
supplied as named numeric values; passing named vectors instead of
scalars groups the sources into labeled columns, and at most three
groups are supported, matching the standard PRISMA layout. A `sources()`
flow is operated in manual mode and is normally followed by
[`combine()`](https://phmcc.codeberg.page/selecta/reference/combine.md)
to merge the streams into a single downstream node. For a conventional
single-entry study, use
[`enroll()`](https://phmcc.codeberg.page/selecta/reference/enroll.md)
instead.

## See also

[`enroll`](https://phmcc.codeberg.page/selecta/reference/enroll.md) for
single-source entry,
[`combine`](https://phmcc.codeberg.page/selecta/reference/combine.md) to
merge parallel streams into a single flow

Other flow construction functions:
[`assess()`](https://phmcc.codeberg.page/selecta/reference/assess.md),
[`combine()`](https://phmcc.codeberg.page/selecta/reference/combine.md),
[`endpoint()`](https://phmcc.codeberg.page/selecta/reference/endpoint.md),
[`enroll()`](https://phmcc.codeberg.page/selecta/reference/enroll.md),
[`exclude()`](https://phmcc.codeberg.page/selecta/reference/exclude.md),
[`phase()`](https://phmcc.codeberg.page/selecta/reference/phase.md),
[`stratify()`](https://phmcc.codeberg.page/selecta/reference/stratify.md)

## Examples

``` r
# Simple multi-source (one column, no header)
sources(PubMed = 1234, Embase = 567, CENTRAL = 89)
#> selecta flow (manual mode)
#>   Starting N: 1.890
#>   Steps: 1
#>   [1] sources:
#>          PubMed (n = 1.234)
#>          Embase (n = 567)
#>          CENTRAL (n = 89)

# Grouped sources (PRISMA two-column layout)
sources(
  databases = c("PubMed" = 1234, "Embase" = 567, "CENTRAL" = 89),
  other     = c("Citation search" = 55, "Websites" = 34)
)
#> selecta flow (manual mode)
#>   Starting N: 1.979
#>   Steps: 1
#>   [1] sources (Databases):
#>          PubMed (n = 1.234)
#>          Embase (n = 567)
#>          CENTRAL (n = 89)
#>   [1] sources (Other):
#>          Citation search (n = 55)
#>          Websites (n = 34)

# Three columns with custom headers
sources(
  previous  = c("Previous review" = 12, "Previous reports" = 15),
  databases = c("PubMed" = 1234, "Embase" = 567, "CENTRAL" = 89),
  other     = c("Citation search" = 55, "Websites" = 34),
  headers   = c(previous  = "Previous studies",
                databases = "Databases and registers",
                other     = "Other methods")
) |>
  combine("Records after deduplication") |>
  exclude("Records removed", n = 352, show_count = FALSE,
          reasons = c("Duplicates" = 340,
                      "Marked ineligible" = 12))
#> selecta flow (manual mode)
#>   Starting N: 2.006
#>   Steps: 3
#>   [1] sources (Previous studies):
#>          Previous review (n = 12)
#>          Previous reports (n = 15)
#>   [1] sources (Databases and registers):
#>          PubMed (n = 1.234)
#>          Embase (n = 567)
#>          CENTRAL (n = 89)
#>   [1] sources (Other methods):
#>          Citation search (n = 55)
#>          Websites (n = 34)
#>   [2] combine: "Records after deduplication"
#>   [3] exclude: "Records removed" (n = 352)
#>          • Duplicates = 340
#>          • Marked ineligible = 12
#> 
```
