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
| `autodiagram()` | Save to file (PDF, PNG, SVG, TIFF) with auto-computed dimensions |
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

`selecta` provides dedicated functions for each guideline's specific structural requirements, all composable within the same pipe-friendly framework:

| Guideline | Study type | Key functions |
|:----------|:-----------|:-------------|
| **CONSORT** | Randomized trials | `enroll()`, `allocate()`, `phase()` |
| **STROBE** | Observational cohorts | `enroll()`, `stratify()` |
| **STARD** | Diagnostic accuracy | `assess()`, `classify()` |
| **PRISMA** | Systematic reviews | `sources()`, `combine()` |
| **MOOSE** | Meta-analyses of observational studies | `sources()`, `combine()`, `stratify()` |

### Operating Modes

`selecta` supports two modes, selected automatically by the arguments passed to `enroll()`:

| | Data-driven mode | Manual mode |
|:---|:---|:---|
| **Initialization** | `enroll(data, id = "patient_id")` | `enroll(n = 1200)` |
| **Exclusions** | `exclude("Label", expr = <condition>)` | `exclude("Label", n = 50)` |
| **Arms** | `allocate("treatment_column")` | `allocate(labels = c("A", "B"), n = c(300, 300))` |
| **Sub-reasons** | Tabulated from a column (`reasons_var`) | Supplied as a named vector (`reasons`) |
| **Cohort extraction** | Available via `cohort()` | Not applicable |

## Usage

### CONSORT — Randomized Trial

When exact counts are known, manual mode accepts integers directly:

```r
library(selecta)

enroll(n = 1200, label = "Records identified") |>
  phase("Enrollment") |>
  exclude("Duplicates removed", n = 84) |>
  exclude("Age < 18 or > 85", n = 63) |>
  exclude("Missing staging data", n = 41) |>
  phase("Allocation") |>
  allocate(labels = c("Neoadjuvant therapy", "Upfront surgery"),
      n = c(498, 514)) |>
  phase("Follow-up") |>
  exclude("Lost to follow-up", n = c(23, 31)) |>
  exclude("Incomplete data", n = c(12, 8)) |>
  phase("Analysis") |>
  endpoint("Included in analysis") |>
  flowchart()
```

When the study dataset is available, `selecta` computes all counts by evaluating exclusion expressions against the data. This ensures that the diagram and the analysis derive from the same source:

```r
flow <- enroll(rctselect2, id = "id") |>
  phase("Screening") |>
  exclude("Failed eligibility", expr = eligible == FALSE,
          reasons = "exclusion_reason") |>
  phase("Allocation") |>
  allocate("treatment") |>
  phase("Follow-up") |>
  exclude("Discontinued", expr = discontinued == TRUE) |>
  phase("Analysis") |>
  endpoint("Completed study")

flowchart(flow)
```

### STROBE — Observational Cohort

```r
enroll(n = 3860, label = "Identified from registry") |>
  exclude("Excluded", n = 420,
    reasons = c("Missing exposure data" = 215,
                "Prior diagnosis" = 140,
                "Lost to follow-up" = 65)) |>
  stratify(labels = c("Exposed", "Unexposed"), n = c(1680, 1760),
           label = "Classified by exposure status") |>
  exclude(c("Treatment discontinued", "Initiated treatment"),
          n = c(85, 92)) |>
  endpoint("Included in analysis") |>
  flowchart()
```

### STARD — Diagnostic Accuracy Study

```r
enroll(n = 520, label = "Eligible patients") |>
  assess("Index test", not_received = 22,
         reasons = c("Refused" = 12, "Contraindicated" = 10)) |>
  assess("Reference standard", not_received = 18,
         reasons = c("Lost to follow-up" = 11, "Inconclusive" = 7)) |>
  classify(
    rows = c("Target condition present", "Target condition absent"),
    cols = c("Index test +", "Index test \u2212"),
    n = matrix(c(285, 15, 20, 160), nrow = 2)
  ) |>
  flowchart()
```

### PRISMA — Systematic Review

```r
sources(
  previous  = c("Previous review" = 92),
  databases = c("PubMed" = 1234, "Embase" = 567, "CENTRAL" = 89),
  other     = c("Citation search" = 55, "Grey literature" = 34),
  headers   = c(previous  = "Previous studies",
                databases = "Databases and registers",
                other     = "Other methods")
) |>
  combine("Records after deduplication", n = 1450) |>
  exclude("Records removed before screening", n = 352,
          reasons = c("Duplicates" = 340, "Automation" = 12)) |>
  exclude("Records excluded at title/abstract", n = 820) |>
  exclude("Reports not retrieved", n = 45) |>
  exclude("Reports excluded at full text", n = 178,
          reasons = c("Wrong population" = 65,
                      "Wrong intervention" = 52,
                      "Wrong outcome" = 38,
                      "Wrong study design" = 23)) |>
  endpoint("Studies included in review") |>
  flowchart()
```

### Exclusion Reasons

Side boxes can itemize why participants were excluded. These appear as indented lines beneath the total count.

In manual mode, sub-reasons are supplied as a named integer vector:

```r
enroll(n = 800, label = "Assessed for eligibility") |>
  exclude("Excluded", n = 150,
    reasons = c("Progressive disease" = 55,
                "Unacceptable comorbidities" = 48,
                "Declined surgery" = 32,
                "Lost to follow-up" = 15)) |>
  allocate(labels = c("Arm A", "Arm B"), n = c(325, 325)) |>
  endpoint("Final Analysis") |>
  flowchart()
```

In data-driven mode, reasons are tabulated automatically from a column in the dataset via the `reasons` argument. Zero-count categories are suppressed by default; set `show_zero = TRUE` to display all pre-specified categories (as may be required for protocol-mandated complete reporting).

### Cohort Extraction

The diagram is not merely a figure — `selecta` maintains the dataset state at every step, allowing direct extraction of the analysis-ready cohort:

```r
# The final cohort
final <- cohort(flow)

# Split by arm
by_arm <- cohort(flow, split = TRUE)

# A single arm
drug_a <- cohort(flow, arm = "Drug A")
```

Every intermediate stage is accessible via `cohorts()`, enabling inspection of participants removed at each step:

```r
stages <- cohorts(flow)

# Participants excluded for failing eligibility
stages[["Failed eligibility"]]$excluded

# Dataset remaining after the eligibility exclusion
stages[["Failed eligibility"]]$remaining
```

### Export

Diagrams can be saved to file with dimensions computed automatically from diagram content:

```r
autodiagram(flow, "consort.pdf")
autodiagram(flow, "consort.png", width = 8, height = 10, res = 300)

# Inspect recommended dimensions without saving
suggest_size(flow)
```

### Graphviz Output

For integration with HTML documents or external layout tools, diagrams can be rendered as Graphviz DOT strings:

```r
dot_string <- enroll(n = 500) |>
  exclude("Excluded", n = 60) |>
  endpoint("Analyzed") |>
  flowchart(engine = "dot")

# Render via DiagrammeR
DiagrammeR::grViz(dot_string)
```

## Dependencies

**Required:**

- `data.table` — data manipulation
- `grid` — rendering (part of base R)

**Optional:**

- `DiagrammeR` — for Graphviz rendering via `flowchart(engine = "dot")`
- `testthat` (>= 3.0.0) — for running the test suite

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

## Development

### Repository

- **Primary development**: [codeberg.org/phmcc/selecta](https://codeberg.org/phmcc/selecta)
- **GitHub releases**: [github.com/phmcc/selecta](https://github.com/phmcc/selecta)

### Testing

The package includes a comprehensive `testthat` suite with 117 tests covering core pipeline functions, the stream model, the compute engine, rendering output, and S3 methods. Run the tests with:

```r
devtools::test()
```

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
