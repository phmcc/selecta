# selecta [![selecta website](reference/figures/selecta.svg)](https://phmcc.codeberg.page/selecta/)

> ***selecta*** \| /seˈlɛk.ta/ \| *Latin, n. pl. of* selectum*, past
> participle of* seligere*: things chosen out*
>
> Declarative EQUATOR-style diagrams for clinical studies.

## Overview

The `selecta` package provides a pipe-friendly, declarative interface
for constructing EQUATOR-style flow diagrams. A diagram is specified as
a sequence of operations—enrollment, exclusion, stratification,
recombination, and endpoint—that mirrors the natural language
description of a study’s participant flow. The package supports multiple
reporting guidelines (CONSORT, STROBE, STARD, PRISMA, MOOSE), hybrid or
custom topologies, automatic count computation, arithmetic checking,
diagram render via `grid` graphics or Graphviz DOT, and optionally
return of the analysis-ready cohort at any stage of the selection
process.

For a more comprehensive description of this package and its features,
see the [full documentation and
vignettes](https://phmcc.codeberg.page/selecta/).

![Flow diagram showcasing multi-source confluence, split-and-recombine
flow, and factorial topology](reference/figures/README_hero.png)

## Installation

This package is not yet available on CRAN. Install it from GitHub
(stable) or Codeberg (development):

``` r
# Stable release
devtools::install_github("phmcc/selecta")

# Development version
devtools::install_git("https://codeberg.org/phmcc/selecta.git")
```

## Package Composition

### Design Principles

The architecture of `selecta` reflects four guiding principles:

1.  **Declarative specification.** Diagrams are defined as a linear
    sequence of named operations. The code reads top-to-bottom, matching
    the visual top-to-bottom flow of an EQUATOR diagram and the
    narrative order in which investigators describe their enrollment
    process.

2.  **Dual-mode operation.** The same API supports both *data* mode
    (counts computed automatically from a data frame) and *manual* mode
    (counts supplied directly as integers). This permits use at any
    stage of a project—from protocol drafting through final manuscript
    preparation.

3.  **Diagram–data duality.** A `selecta` object is simultaneously a
    diagram specification and a data pipeline.
    [`flowchart()`](https://phmcc.codeberg.page/selecta/reference/flowchart.md)
    renders the visual output;
    [`cohort()`](https://phmcc.codeberg.page/selecta/reference/cohort.md)
    extracts the resulting dataset. The same object serves both
    reporting and analysis.

4.  **Minimal dependencies.** The package requires only `data.table` for
    data manipulation and `grid` (part of base R) for rendering. No
    external graphics libraries, web frameworks, or layout engines are
    imposed.

These principles manifest in the standard calling convention:

``` r
flow <- enroll(data, id = "patient_id") |>
  exclude("Excluded", criterion = <condition>) |>
  allocate("treatment_variable") |>
  endpoint("Final Analysis")

flowchart(flow)  # render the diagram
cohort(flow)     # extract the analysis-ready dataset
```

### Supported Guidelines

`selecta` provides dedicated functions for each guideline’s specific
structural requirements, all composable within the same pipe-oriented
framework:

| Guideline | Study type | Key functions |
|:---|:---|:---|
| **CONSORT** | Randomized trials | [`enroll()`](https://phmcc.codeberg.page/selecta/reference/enroll.md), [`allocate()`](https://phmcc.codeberg.page/selecta/reference/stratify.md) |
| **STROBE** | Observational cohorts | [`enroll()`](https://phmcc.codeberg.page/selecta/reference/enroll.md), [`stratify()`](https://phmcc.codeberg.page/selecta/reference/stratify.md) |
| **STARD** | Diagnostic accuracy | [`assess()`](https://phmcc.codeberg.page/selecta/reference/assess.md), [`stratify()`](https://phmcc.codeberg.page/selecta/reference/stratify.md), [`endpoint()`](https://phmcc.codeberg.page/selecta/reference/endpoint.md) |
| **PRISMA** | Systematic reviews | [`sources()`](https://phmcc.codeberg.page/selecta/reference/sources.md), [`combine()`](https://phmcc.codeberg.page/selecta/reference/combine.md) |
| **MOOSE** | Meta-analyses of observational studies | [`sources()`](https://phmcc.codeberg.page/selecta/reference/sources.md), [`combine()`](https://phmcc.codeberg.page/selecta/reference/combine.md), [`stratify()`](https://phmcc.codeberg.page/selecta/reference/stratify.md) |

### Functional Reference

#### Diagram construction

Functions for building the enrollment flow. Each returns a modified
`selecta` object and is designed for use in a pipe chain.

| Function | Purpose | Guideline |
|:---|:---|:---|
| [`enroll()`](https://phmcc.codeberg.page/selecta/reference/enroll.md) | Initialize a flow from data (`data`, `id`) or counts (`n`) | CONSORT, STROBE, STARD, split-and-recombine |
| [`sources()`](https://phmcc.codeberg.page/selecta/reference/sources.md) | Initialize a multi-source flow with parallel columns | PRISMA, MOOSE |
| [`exclude()`](https://phmcc.codeberg.page/selecta/reference/exclude.md) | Remove participants matching a criterion, with optional sub-reasons | All |
| [`allocate()`](https://phmcc.codeberg.page/selecta/reference/stratify.md) | Split into randomized arms (alias for [`stratify()`](https://phmcc.codeberg.page/selecta/reference/stratify.md)) | CONSORT |
| [`stratify()`](https://phmcc.codeberg.page/selecta/reference/stratify.md) | Split into parallel strata by any characteristic | STROBE, STARD, MOOSE |
| [`assess()`](https://phmcc.codeberg.page/selecta/reference/assess.md) | Record a test/procedure receipt step | STARD |
| [`combine()`](https://phmcc.codeberg.page/selecta/reference/combine.md) | Merge parallel streams into a single flow | PRISMA, MOOSE, split-and-recombine |
| [`endpoint()`](https://phmcc.codeberg.page/selecta/reference/endpoint.md) | Designate the terminal node(s) | All |
| [`phase()`](https://phmcc.codeberg.page/selecta/reference/phase.md) | Label a study phase (vertical text in left margin) | All |

#### Rendering and export

| Function | Purpose |
|:---|:---|
| [`flowchart()`](https://phmcc.codeberg.page/selecta/reference/flowchart.md) | Render the diagram (grid graphics or Graphviz DOT) |
| [`plot()`](https://rdrr.io/r/graphics/plot.default.html) | S3 alias for [`flowchart()`](https://phmcc.codeberg.page/selecta/reference/flowchart.md) |
| [`flowsave()`](https://phmcc.codeberg.page/selecta/reference/flowsave.md) | Save to file (PDF, PNG, SVG, TIFF) with auto-computed dimensions |
| [`recdims()`](https://phmcc.codeberg.page/selecta/reference/recdims.md) | Compute recommended figure dimensions from diagram content |

#### Data extraction

| Function | Purpose |
|:---|:---|
| [`cohort()`](https://phmcc.codeberg.page/selecta/reference/cohort.md) | Extract the final analysis-ready dataset |
| [`cohorts()`](https://phmcc.codeberg.page/selecta/reference/cohorts.md) | Extract datasets at every intermediate stage |

#### Inspection

| Function | Purpose |
|:---|:---|
| [`print()`](https://rdrr.io/r/base/print.html) | Display a text summary of the flow |
| [`summary()`](https://rdrr.io/r/base/summary.html) | Return a `data.table` of all nodes with counts |

### Operating Modes

`selecta` supports two modes, selected automatically by the arguments
passed to
[`enroll()`](https://phmcc.codeberg.page/selecta/reference/enroll.md):

| Workflow | Data mode | Manual mode |
|:---|:---|:---|
| **Initialization** | `enroll(data, id = "patient_id")` | `enroll(n = 1200)` |
| **Exclusions** | `exclude("Label", criterion = <condition>)` | `exclude("Label", n = 50)` |
| **Arms** | `allocate("treatment_column")` | `allocate(labels = c("A", "B"), n = c(300, 300))` |
| **Sub-reasons** | Tabulated from one or two columns (`reasons`) | Named vector, or named list for nested reasons (`reasons`) |
| **Cohort extraction** | Available via [`cohort()`](https://phmcc.codeberg.page/selecta/reference/cohort.md) | Not applicable |

### Flow Topologies

`selecta` supports three distinct flow topologies, distinguished by what
happens after a split:

| Topology | Split | Merge | Use case |
|:---|:---|:---|:---|
| Permanent arms | [`allocate()`](https://phmcc.codeberg.page/selecta/reference/stratify.md) / [`stratify()`](https://phmcc.codeberg.page/selecta/reference/stratify.md) | None | CONSORT, STROBE |
| Source convergence | [`sources()`](https://phmcc.codeberg.page/selecta/reference/sources.md) | [`combine()`](https://phmcc.codeberg.page/selecta/reference/combine.md) | PRISMA, MOOSE |
| Split-and-recombine | [`stratify()`](https://phmcc.codeberg.page/selecta/reference/stratify.md) | [`combine()`](https://phmcc.codeberg.page/selecta/reference/combine.md) | Screening validation, exposure classification |
| Factorial (nested split) | [`allocate()`](https://phmcc.codeberg.page/selecta/reference/stratify.md) / [`stratify()`](https://phmcc.codeberg.page/selecta/reference/stratify.md), twice | Optional [`combine()`](https://phmcc.codeberg.page/selecta/reference/combine.md) | Factorial trials, cross-classified cohorts |

### Visual Customization

All rendering functions accept the following styling parameters:

| Parameter | Purpose | Default |
|:---|:---|:---|
| `cex`, `cex_side`, `cex_phase` | Font size multipliers (main, side, phase) | 0.85, same as `cex`, 0.9 |
| `box_fill` | Fill color for main flow boxes | `"white"` |
| `side_fill` | Fill color for side (exclusion) boxes | `"white"` |
| `border_col` | Border color for all boxes | `"black"` |
| `arrow_col` | Color for connector arrows | `"black"` |
| `phase_fill` | Fill color for vertical phase strips | `"black"` |
| `phase_text_col` | Text color for phase labels | `"white"` |
| `font_family` | Font family for all text | `"Helvetica"` |
| `count_first` | Bold count before label in all boxes | `FALSE` |
| `number_format` | Locale-aware count formatting | `"us"` (or global option) |
| `vpad`, `margin` | Vertical spacing and outer margin (inches) | 0.25, 0.25 |

### Regional number formatting

The `number_format` argument accepts named presets (`"us"`, `"eu"`,
`"space"`, `"none"`) or a custom `c(big.mark, decimal.mark)` vector. The
choice may be set globally for a session:

``` r
options(selecta.number_format = "eu")   # 1.234 instead of 1,234
options(selecta.vpad = 0.35)            # increase default vertical spacing
```

### Diagnostic output

For inspecting flow diagram layout specifics, `selecta` can emit a
structured trace of its internal computation. The trace is controlled by
a single session option:

``` r
options(selecta.debug_layout = TRUE)
```

## Comparison with Related Packages

The R ecosystem includes several packages for generating CONSORT
diagrams. The following comparison identifies areas of overlap and
distinction:

| Capability | selecta | consort | ggconsort | PRISMAstatement |
|:---|:--:|:--:|:--:|:--:|
| Pipe-friendly declarative API | ✓ | — | ✓ | — |
| Data-driven automatic counting | ✓ | ✓ | ✓ | — |
| Manual count entry | ✓ | ✓ | — | ✓ |
| Multi-guideline support (CONSORT, STROBE, STARD, PRISMA) | ✓ | — | — | ◐ |
| Multi-source entry (PRISMA) | ✓ | — | — | ✓ |
| Split-and-recombine topology | ✓ | — | — | — |
| Cohort extraction for analysis | ✓ | — | — | — |
| Phase labels (CONSORT standard) | ✓ | ✓ | — | — |
| Exclusion sub-reasons | ✓ | ✓ | — | — |
| Factorial (nested-split) designs | ✓ | ◐ | — | — |
| Hierarchical (nested) sub-reasons | ✓ | — | — | — |
| Multi-format export | ✓ | ◐ | ◐ | — |
| Graphviz/HTML output | ✓ | ✓ | — | ✓ |

_(✓ Full support \| ◐ Partial support \| — Not available)

A detailed feature comparison is available in the [package
documentation](https://phmcc.codeberg.page/selecta/articles/feature_comparison.html).

## Illustrative Example

The `selectaex*` datasets included with this package provide simulated
clinical trial selection cohorts with various inclusion/exclusion
criteria, as well as different arm allocation criteria. The following
example demonstrates how `selecta` functions can be used to generate a
CONSORT diagram from the two-armed dataset `selectaex2`, using
data-driven counts, count-first formatting, and automatic subcohort
extraction.

### **Step 0:** Data Preparation

Prior to analysis, load the package and the dataset:

``` r
library(selecta)

# Load example data
data("selectaex2")
```

### **Step 1:** Flowchart Creation

Use a pipe-based workflow to sequentially string together the various
elements of the flowchart, from top to bottom. The
[`exclude()`](https://phmcc.codeberg.page/selecta/reference/exclude.md)
function pares down the dataset based on the condition supplied to the
`criterion` parameter, whereas
[`allocate()`](https://phmcc.codeberg.page/selecta/reference/stratify.md)/[`stratify()`](https://phmcc.codeberg.page/selecta/reference/stratify.md)
sets arms. Export the output using the
[`flowsave()`](https://phmcc.codeberg.page/selecta/reference/flowsave.md)
function.

``` r
flow <- enroll(selectaex2, id = "patient_id") |>
    phase("Screening") |>
    exclude("Duplicate records", criterion = is_duplicate == TRUE,
            included_label = "Unique records") |>
    exclude("Failed eligibility", criterion = eligible == FALSE,
            reasons = "exclusion_reason",
            included_label = "Eligible cohort") |>
    phase("Allocation") |>
    allocate("treatment") |>
    phase("Follow-up") |>
    exclude("Discontinued", criterion = discontinued == TRUE,
            reasons = "discontinuation_reason") |>
    phase("Analysis") |>
    endpoint("Analysis cohort")

flowsave(flow, "consort.pdf", count_first = TRUE)
```

![Two-arm CONSORT diagram](reference/figures/README_CONSORT_2arm.png)

### **Step 2:** Cohort Extraction

The diagram is not merely a figure—`selecta` maintains the dataset state
at every step, allowing direct extraction of the analysis-ready cohort:

``` r
# The final cohort
final <- cohort(flow)

# Split by arm
by_arm <- cohort(flow, split = TRUE)

# A single arm
drug_a <- cohort(flow, arm = "Drug A")
```

Moreover, every intermediate stage is accessible via
[`cohorts()`](https://phmcc.codeberg.page/selecta/reference/cohorts.md),
enabling inspection of participants removed at each step:

``` r
stages <- cohorts(flow)

# Participants excluded for failing eligibility
stages[["Failed eligibility"]]$excluded

# Dataset remaining after the eligibility exclusion
stages[["Failed eligibility"]]$included
```

## Development

### Repository

- **Primary development**:
  [codeberg.org/phmcc/selecta](https://codeberg.org/phmcc/selecta)
- **GitHub releases**:
  [github.com/phmcc/selecta](https://github.com/phmcc/selecta)

### Contributing

Bug reports and feature requests may be submitted via the issue tracker
([Codeberg](https://codeberg.org/phmcc/selecta/issues) or
[GitHub](https://github.com/phmcc/selecta/issues)). Contributions are
welcome; prospective contributors are directed to the contributing
guidelines prior to submitting pull requests.

## Acknowledgments

The design of `selecta` draws inspiration from several existing packages
and reference standards:

- **EQUATOR Network** — Reporting-guideline standards (CONSORT, STROBE,
  STARD, PRISMA, MOOSE)
- **consort** (Alim Dayim) — CONSORT diagram conventions in R
- **stard** (Chiara Herzog) — STARD diagram conventions in R
- **DiagrammeR** (Iannone) — R Graphviz/DOT rendering
- **data.table** (Dowle & Srinivasan) — High-performance data operations

## License

GPL (\>= 3.0)

## Citation

``` r
citation("selecta")

To cite selecta in publications, use:

  McClelland PH (2026). _selecta: EQUATOR-Style Enrollment Diagrams
  for Clinical Studies_. R package version 0.6.0,
  <https://phmcc.codeberg.page/selecta/>.

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {selecta: EQUATOR-Style Enrollment Diagrams for Clinical Studies},
    author = {Paul Hsin-ti McClelland},
    year = {2026},
    note = {R package version 0.6.0},
    url = {https://phmcc.codeberg.page/selecta/},
  }
```

## Further Resources

- **Function documentation**: `?function_name` or the [reference
  index](https://phmcc.codeberg.page/selecta/reference/index.html)
- **Companion package**:
  [`summata`](https://phmcc.codeberg.page/summata/) for
  publication-ready summary tables
- **Issue tracker**: [Codeberg
  Issues](https://codeberg.org/phmcc/selecta/issues), [GitHub
  Issues](https://github.com/phmcc/selecta/issues)

------------------------------------------------------------------------

_(The `selecta` package is under active development. Any breaking changes to the API will be reported in the changelog.)
