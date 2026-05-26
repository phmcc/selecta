# <span class="pkg-name">selecta</span> <a href="https://phmcc.codeberg.page/selecta/"><img src="man/figures/selecta.svg" align="right" height="139" alt="selecta website" /></a>

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

> ***selecta*** | /seˈlɛk.ta/ | *Latin, n. pl. of* selectum*, past participle of* seligere*: things chosen out*
>
> Declarative EQUATOR-style diagrams for clinical studies.

## Overview

The `selecta` package provides a pipe-friendly, declarative interface for constructing EQUATOR-style enrollment diagrams. A diagram is specified as a sequence of operations—enrollment, exclusion, stratification, recombination, and endpoint—that mirrors the natural language description of a study's participant flow. The package supports multiple reporting guidelines (CONSORT, STROBE, STARD, PRISMA, MOOSE), computes counts either automatically from a supplied dataset or from user-provided integers, renders the diagram via `grid` graphics or Graphviz DOT, and optionally returns the analysis-ready cohort at any stage of the selection process.

For a more comprehensive description of this package and its features, see the [full documentation and vignettes](https://phmcc.codeberg.page/selecta/).

## Installation

This package is not yet on CRAN. Install it from GitHub (stable) or Codeberg (development):

```r
# Stable release
devtools::install_github("phmcc/selecta")

# Development version
devtools::install_git("https://codeberg.org/phmcc/selecta.git")
```

`selecta` requires R (>= 4.1.0).

## Package Composition

### Design Principles

The architecture of `selecta` reflects four guiding principles:

1. **Declarative specification.** Diagrams are defined as a linear sequence of named operations. The code reads top-to-bottom, matching the visual top-to-bottom flow of an EQUATOR diagram and the narrative order in which investigators describe their enrollment process.

2. **Dual-mode operation.** The same API supports both *data* mode (counts computed automatically from a data frame) and *manual* mode (counts supplied directly as integers). This permits use at any stage of a project—from protocol drafting through final manuscript preparation.

3. **Diagram–data duality.** A `selecta` object is simultaneously a diagram specification and a data pipeline. `flowchart()` renders the visual output; `cohort()` extracts the resulting dataset. The same object serves both reporting and analysis.

4. **Minimal dependencies.** The package requires only `data.table` for data manipulation and `grid` (part of base R) for rendering. No external graphics libraries, web frameworks, or layout engines are imposed.

These principles manifest in the standard calling convention:

```r
flow <- enroll(data, id = "patient_id") |>
  exclude("Excluded", criteria = x | y | z) |>
  allocate("treatment_variable") |>
  endpoint("Final Analysis")

flowchart(flow)  # render the diagram
cohort(flow)     # extract the analysis-ready dataset
```

### Supported Guidelines

`selecta` provides dedicated functions for each guideline's specific structural requirements, all composable within the same pipe-oriented framework:

| Guideline | Study type | Key functions |
|:----------|:-----------|:-------------|
| **CONSORT** | Randomized trials | `enroll()`, `allocate()`, `phase()` |
| **STROBE** | Observational cohorts | `enroll()`, `stratify()` |
| **STARD** | Diagnostic accuracy | `assess()`, `classify()` |
| **PRISMA** | Systematic reviews | `sources()`, `combine()` |
| **MOOSE** | Meta-analyses of observational studies | `sources()`, `combine()`, `stratify()` |

### Functional Reference

#### Diagram construction

Functions for building the enrollment flow. Each returns a modified `selecta` object and is designed for use in a pipe chain.

| Function | Purpose | Guideline |
|:---------|:--------|:----------|
| `enroll()` | Initialize a flow from data (`data`, `id`) or counts (`n`) | All |
| `sources()` | Initialize a multi-source flow with parallel columns | PRISMA |
| `exclude()` | Remove participants matching a criterion, with optional sub-reasons | All |
| `assess()` | Record a test/procedure receipt step | STARD |
| `phase()` | Label a study phase (vertical text in left margin) | CONSORT |
| `stratify()` | Split into parallel strata by any characteristic | STROBE, MOOSE |
| `allocate()` | Split into randomized arms (alias for `stratify()`) | CONSORT |
| `combine()` | Merge parallel streams into a single flow | PRISMA, split-and-recombine |
| `classify()` | Add a terminal cross-classification grid | STARD |
| `endpoint()` | Designate the terminal node(s) | All |

#### Rendering and export

| Function | Purpose |
|:---------|:--------|
| `flowchart()` | Render the diagram (grid graphics or Graphviz DOT) |
| `plot()` | S3 alias for `flowchart()` |
| `flowsave()` | Save to file (PDF, PNG, SVG, TIFF) with auto-computed dimensions |
| `recdims()` | Compute recommended figure dimensions from diagram content |

#### Data extraction

| Function | Purpose |
|:---------|:--------|
| `cohort()` | Extract the final analysis-ready dataset |
| `cohorts()` | Extract datasets at every intermediate stage |

#### Inspection

| Function | Purpose |
|:---------|:--------|
| `print()` | Display a text summary of the flow |
| `summary()` | Return a `data.table` of all nodes with counts |

### Operating Modes

`selecta` supports two modes, selected automatically by the arguments passed to `enroll()`:

| Workflow | Data mode | Manual mode |
|:---|:---|:---|
| **Initialization** | `enroll(data, id = "patient_id")` | `enroll(n = 1200)` |
| **Exclusions** | `exclude("Label", criteria = <condition>)` | `exclude("Label", n = 50)` |
| **Arms** | `allocate("treatment_column")` | `allocate(labels = c("A", "B"), n = c(300, 300))` |
| **Sub-reasons** | Tabulated from a column (`reasons_var`) | Supplied as a named vector (`reasons`) |
| **Cohort extraction** | Available via `cohort()` | Not applicable |

### Flow Topologies

`selecta` supports three distinct flow topologies, distinguished by what happens after a split:

| Topology | Split | Merge | Use case |
|:---------|:------|:------|:---------|
| Permanent arms | `allocate()` / `stratify()` | None | CONSORT, STROBE |
| Source convergence | `sources()` | `combine()` | PRISMA, MOOSE |
| Split-and-recombine | `stratify()` | `combine()` | Screening validation, exposure classification |

The split-and-recombine topology fans the population out into strata in the middle of the diagram, characterizes each stratum independently, and reconverges to a single stream before the endpoint—producing the diamond-shaped layout common in screening-validation studies.

### Visual Customization

All rendering functions accept the following styling parameters:

| Parameter | Purpose | Default |
|:----------|:--------|:--------|
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

The `number_format` argument accepts named presets (`"us"`, `"eu"`, `"space"`, `"none"`) or a custom `c(big.mark, decimal.mark)` vector. The choice may be set globally for a session:

```r
options(selecta.number_format = "eu")   # 1.234 instead of 1,234
options(selecta.vpad = 0.35)            # increase default vertical spacing
```

## Comparison with Related Packages

The R ecosystem includes several packages for generating CONSORT diagrams. The following comparison identifies areas of overlap and distinction:

| Capability | selecta | consort | ggconsort | PRISMAstatement |
|:-----------|:-------:|:-------:|:---------:|:---------------:|
| Pipe-friendly declarative API | ✓ | — | ✓ | — |
| Data-driven automatic counting | ✓ | ✓ | ✓ | — |
| Manual count entry | ✓ | — | — | ✓ |
| Multi-guideline support (CONSORT, STROBE, STARD, PRISMA) | ✓ | — | — | ◐ |
| Multi-source entry (PRISMA) | ✓ | — | — | ✓ |
| Split-and-recombine topology | ✓ | — | — | — |
| Cohort extraction for analysis | ✓ | — | — | — |
| Phase labels (CONSORT standard) | ✓ | ✓ | — | — |
| Exclusion sub-reasons | ✓ | — | — | — |
| Multi-format export | ✓ | ◐ | ◐ | — |
| Graphviz/HTML output | ✓ | — | — | — |

<sub>✓ Full support | ◐ Partial support | — Not available</sub>

A detailed feature comparison is available in the [package documentation](https://phmcc.codeberg.page/selecta/articles/feature_comparison.html).

## Illustrative Example

The `rctselect*` datasets included with this package provide simulated clinical trial selection cohorts with various inclusion/exclusion criteria, as well as different arm allocation criteria. The following example demonstrates how `selecta` functions can be used to generate a CONSORT diagram from the two-armed dataset `rctselect2`, using data-driven counts, count-first formatting, and automatic subcohort extraction.

### **Step 0:** Data Preparation

Prior to analysis, load the package and the dataset:

``` r
library(selecta)

# Load example data
data("rctselect2")
```

### **Step 1:** Flowchart Creation

Use a pipe-based workflow to sequentially string together the various elements of the flowchart, from top to bottom. The `exclude()` function pares down the dataset based on the condition supplied to the `criteria` parameter, whereas `allocate()`/`stratify()` sets arms. Export the output using the `flowsave()` function.

``` r
flow <- enroll(rctselect2, id = "patient_id") |>
    phase("Screening") |>
    exclude("Duplicate records", criteria = is_duplicate == TRUE,
            included_label = "Unique records") |>
    exclude("Failed eligibility", criteria = eligible == FALSE,
            reasons = "exclusion_reason",
            included_label = "Eligible cohort") |>
    phase("Allocation") |>
    allocate("treatment") |>
    phase("Follow-up") |>
    exclude("Discontinued", criteria = discontinued == TRUE,
            reasons = "discontinuation_reason") |>
    phase("Analysis") |>
    endpoint("Analysis cohort")

flowsave(flow, "consort.pdf", count_first = TRUE)
```

<img src="man/figures/README_CONSORT_2arm.png" alt="Two-arm CONSORT diagram" width="100%">

### **Step 2:** Cohort Extraction

The diagram is not merely a figure—`selecta` maintains the dataset state at every step, allowing direct extraction of the analysis-ready cohort:

```r
# The final cohort
final <- cohort(flow)

# Split by arm
by_arm <- cohort(flow, split = TRUE)

# A single arm
drug_a <- cohort(flow, arm = "Drug A")
```

Moreover, every intermediate stage is accessible via `cohorts()`, enabling inspection of participants removed at each step:

```r
stages <- cohorts(flow)

# Participants excluded for failing eligibility
stages[["Failed eligibility"]]$excluded

# Dataset remaining after the eligibility exclusion
stages[["Failed eligibility"]]$remaining
```

## Development

### Repository

- **Primary development**: [codeberg.org/phmcc/selecta](https://codeberg.org/phmcc/selecta)
- **GitHub releases**: [github.com/phmcc/selecta](https://github.com/phmcc/selecta)

### Contributing

Bug reports and feature requests may be submitted via the issue tracker ([Codeberg](https://codeberg.org/phmcc/selecta/issues) or [GitHub](https://github.com/phmcc/selecta/issues)). Contributions are welcome; prospective contributors are directed to the contributing guidelines prior to submitting pull requests.

## Acknowledgments

The design of `selecta` draws inspiration from several existing packages and reference standards:

- **EQUATOR Network** — Reporting-guideline standards (CONSORT, STROBE, STARD, PRISMA, MOOSE)
- **consort** (Dayim) — CONSORT diagram conventions
- **DiagrammeR** (Iannone) — Graphviz/DOT rendering approach
- **data.table** (Dowle & Srinivasan) — High-performance data operations

## License

GPL (>= 3.0)

## Citation

```r
citation("selecta")

To cite selecta in publications, use:

  McClelland PH (2026). _selecta: EQUATOR-Style Enrollment Diagrams
  for Clinical Studies_. R package version 0.4.0,
  <https://phmcc.codeberg.page/selecta/>.

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {selecta: EQUATOR-Style Enrollment Diagrams for Clinical Studies},
    author = {Paul Hsin-ti McClelland},
    year = {2026},
    note = {R package version 0.4.0},
    url = {https://phmcc.codeberg.page/selecta/},
  }
```

## Further Resources

- **Function documentation**: `?function_name` or the [reference index](https://phmcc.codeberg.page/selecta/reference/index.html)
- **Companion package**: [`summata`](https://phmcc.codeberg.page/summata/) for publication-ready summary tables
- **Issue tracker**: [Codeberg Issues](https://codeberg.org/phmcc/selecta/issues), [GitHub Issues](https://github.com/phmcc/selecta/issues)

---

<sub>The `selecta` package is under active development. Any breaking changes to the API will be reported in the changelog.</sub>
