# <span class="pkg-name">selecta</span> <a href="https://phmcc.github.io/selecta/"><img src="man/figures/selecta.svg" align="right" height="139" alt="selecta website" /></a>

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

> ***selecta*** | /seˈlɛk.ta/ | *Latin, n. pl. of* selectum*, past participle of* seligere*: things chosen out*
>
> Declarative EQUATOR-style diagrams for clinical studies.

## Overview

The `selecta` package provides a pipe-friendly, declarative interface for constructing EQUATOR-style enrollment diagrams. A diagram is specified as a sequence of operations—enrollment, exclusion, stratification, and endpoint—that mirrors the natural language description of a study's participant flow. The package supports multiple reporting guidelines (CONSORT, STROBE, STARD, PRISMA, MOOSE), computes counts either automatically from a supplied dataset or from user-provided integers, renders the diagram via `grid` graphics, and optionally returns the analysis-ready cohort at any stage of the selection process.

## Installation

This package is not yet on CRAN. Install it from GitHub (stable) or Codeberg (development):

```r
# Stable release
devtools::install_github("phmcc/selecta")

# Development version
devtools::install_git("https://codeberg.org/phmcc/selecta.git")
```

## Package Composition

### Design Principles

The architecture of `selecta` reflects four guiding principles:

1. **Declarative specification.** Diagrams are defined as a linear sequence of named operations. The code reads top-to-bottom, matching the visual top-to-bottom flow of an EQUATOR diagram and the narrative order in which investigators describe their enrollment process.

2. **Dual-mode operation.** The same API supports both *data-driven* mode (counts computed automatically from a data frame) and *manual* mode (counts supplied directly as integers). This permits use at any stage of a project—from protocol drafting through final manuscript preparation.

3. **Diagram–data duality.** A `selecta` object is simultaneously a diagram specification and a data pipeline. `flowchart()` renders the visual output; `cohort()` extracts the resulting dataset. The same object serves both reporting and analysis.

4. **Minimal dependencies.** The package requires only `data.table` for data manipulation and `grid` (part of base R) for rendering. No external graphics libraries, web frameworks, or layout engines are imposed.

These principles manifest in the standard calling convention:

```r
flow <- enroll(data, id = "patient_id") |>
  exclude("Criterion", expr = <expression>) |>
  allocate("treatment_variable") |>
  endpoint("Final Analysis")

flowchart(flow)    # render the diagram
cohort(flow)       # extract the analysis-ready dataset
```

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
| `combine()` | Merge parallel streams into a single flow | PRISMA |
| `classify()` | Add a terminal cross-classification grid | STARD |
| `endpoint()` | Designate the terminal node(s) | All |

#### Rendering and export

| Function | Purpose |
|:---------|:--------|
| `flowchart()` | Render the diagram (grid graphics or Graphviz DOT) |
| `plot()` | S3 alias for `flowchart()` |
| `autoflow()` | Save to file (PDF, PNG, SVG, TIFF) with auto-computed dimensions |
| `suggest_size()` | Compute recommended figure dimensions from diagram content |

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

### Supported Guidelines

`selecta` provides dedicated functions for each guideline's specific structural requirements, all composable within the same pipe-oriented framework:

| Guideline | Study type | Key functions |
|:----------|:-----------|:-------------|
| **CONSORT** | Randomized trials | `enroll()`, `allocate()`, `phase()` |
| **STROBE** | Observational cohorts | `enroll()`, `stratify()` |
| **STARD** | Diagnostic accuracy | `assess()`, `classify()` |
| **PRISMA** | Systematic reviews | `sources()`, `combine()` |
| **MOOSE** | Meta-analyses of observational studies | `sources()`, `combine()`, `stratify()` |

### Operating Modes

`selecta` supports two modes, selected automatically by the arguments passed to `enroll()`:

| Workflow | Data-driven mode | Manual mode |
|:---|:---|:---|
| **Initialization** | `enroll(data, id = "patient_id")` | `enroll(n = 1200)` |
| **Exclusions** | `exclude("Label", expr = <condition>)` | `exclude("Label", n = 50)` |
| **Arms** | `allocate("treatment_column")` | `allocate(labels = c("A", "B"), n = c(300, 300))` |
| **Sub-reasons** | Tabulated from a column (`reasons_var`) | Supplied as a named vector (`reasons`) |
| **Cohort extraction** | Available via `cohort()` | Not applicable |

## Comparison with Related Packages

The R ecosystem includes several packages for generating CONSORT diagrams. The following comparison identifies areas of overlap and distinction:

| Capability | selecta | consort | ggconsort | PRISMAstatement |
|:-----------|:-------:|:-------:|:---------:|:---------------:|
| Pipe-friendly declarative API | ✓ | — | ✓ | — |
| Data-driven automatic counting | ✓ | ✓ | ✓ | — |
| Manual count entry | ✓ | — | — | ✓ |
| Multi-guideline support (CONSORT, STROBE, STARD, PRISMA) | ✓ | — | — | ◐ |
| Multi-source entry (PRISMA) | ✓ | — | — | ✓ |
| Diagnostic accuracy grids (STARD) | ✓ | — | — | — |
| Cohort extraction for analysis | ✓ | — | — | — |
| Intermediate stage inspection | ✓ | — | — | — |
| Phase labels (CONSORT standard) | ✓ | ✓ | — | — |
| Exclusion sub-reasons | ✓ | — | — | — |
| Zero-count display control | ✓ | — | — | — |
| Multi-format export | ✓ | ◐ | ◐ | — |
| Graphviz/HTML output | ✓ | — | — | — |
| Minimal dependencies | ✓ | — | — | ✓ |

<sub>✓ Full support | ◐ Partial support | — Not available</sub>

A detailed feature comparison is available in the [package documentation](https://phmcc.github.io/selecta/articles/feature_comparison.html).

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

Use a pipe-based workflow to sequentially string together the various elements of the flowchart, from top to bottom. The `exclude()` function pares down the dataset based on criteria set in the `expr` parameter, whereas `allocate()`/`stratify()` sets arms. Export the output using the `autoflow()` function.

``` r
flow <- enroll(rctselect2, id = "patient_id") |>
    phase("Screening") |>
    exclude("Duplicate records", expr = is_duplicate == TRUE,
            included_label = "Unique records") |>
    exclude("Failed eligibility", expr = eligible == FALSE,
            reasons = "exclusion_reason",
            included_label = "Eligible cohort") |>
    phase("Allocation") |>
    allocate("treatment") |>
    phase("Follow-up") |>
    exclude("Discontinued", expr = discontinued == TRUE,
            reasons = "discontinuation_reason") |>
    phase("Study") |>
    endpoint("Analysis cohort")

autoflow(flow, "consort.pdf", count_first = TRUE)
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

Bug reports and feature requests may be submitted via the [issue tracker](https://github.com/phmcc/selecta/issues). Contributions are welcome; please consult the contributing guidelines prior to submitting pull requests.

## License

GPL (>= 3.0)

## Citation

```r
citation("selecta")

To cite selecta in publications, use:

  McClelland PH (2026). _selecta: EQUATOR-Style Enrollment Diagrams
  for Clinical Studies_. R package version 0.2.0,
  <https://phmcc.github.io/selecta/>.

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {selecta: EQUATOR-Style Enrollment Diagrams for Clinical Studies},
    author = {Paul Hsin-ti McClelland},
    year = {2026},
    note = {R package version 0.2.0},
    url = {https://phmcc.github.io/selecta/},
  }
```

## Further Resources

- **Function documentation**: `?function_name` or the [reference index](reference/index.html)
- **Companion package**: [`summata`](https://phmcc.github.io/summata/) for publication-ready summary tables
- **Issue tracker**: [GitHub Issues](https://github.com/phmcc/selecta/issues)

---

<sub>The `selecta` package is under active development. The API may change prior to CRAN submission.</sub>
