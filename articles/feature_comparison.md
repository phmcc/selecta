# Feature Comparison

This article compares `selecta` with other R packages for generating
clinical study flow diagrams. The R ecosystem includes several packages
targeting specific EQUATOR guidelines, as well as general-purpose
flowchart tools. The following comparison identifies areas of overlap
and distinction.

------------------------------------------------------------------------

## Similar Packages

| Package | Primary Focus |
|:---|:---|
| **selecta** | Multi-guideline enrollment diagrams (CONSORT, STROBE, STARD, PRISMA, MOOSE) |
| **consort** | CONSORT diagrams from disposition data |
| **flowchart** | Tidyverse-style participant flow diagrams |
| **dtrackr** | Data pipeline tracking with CONSORT/STROBE output |
| **ggconsort** | CONSORT diagrams with ggplot2 |
| **PRISMA2020** | Interactive PRISMA 2020 flow diagrams |
| **PRISMAstatement** | Simple PRISMA flow charts (pre-2020 template) |
| **stard** | STARD diagnostic-accuracy flow diagrams |
| **prismadiagramR** | PRISMA diagrams via DiagrammeR |
| **metagear** | Meta-analysis toolkit with `plot_PRISMA()` function |
| **consortr** | Shiny app for interactive CONSORT creation |

------------------------------------------------------------------------

### Core Feature Matrix

| Feature | selecta | consort | flowchart | dtrackr | ggconsort | PRISMA2020 |
|:---|:--:|:--:|:--:|:--:|:--:|:--:|
| CONSORT (randomized trials) | ✓ | ✓ | ✓ | ✓ | ✓ | — |
| STROBE (observational cohorts) | ✓ | ◐ | ◐ | ✓ | — | — |
| STARD (diagnostic accuracy) | ✓ | — | — | — | — | — |
| PRISMA (systematic reviews) | ✓ | — | — | — | — | ✓ |
| MOOSE (observational meta-analysis) | ✓ | — | — | — | — | ◐ |
| Data-driven counts | ✓ | ✓ | ✓ | ✓ | ✓ | — |
| Manual counts | ✓ | ✓ | ◐ | — | ✓ | ✓ |
| Multi-arm layouts (3+) | ✓ | ◐ | ✓ | ✓ | ◐ | — |
| Factorial (nested-split) designs | ✓ | ◐ | — | — | — | — |
| Multi-source convergence | ✓ | — | — | — | — | ✓ |
| Split-and-recombine topology | ✓ | — | — | — | — | — |
| Phase labels | ✓ | ✓ | — | ◐ | — | ✓ |
| Exclusion sub-reasons | ✓ | ✓ | ◐ | ✓ | ◐ | ✓ |
| Per-arm exclusion reasons | ✓ | ◐ | — | ✓ | — | — |
| Hierarchical (nested) sub-reasons | ✓ | — | — | — | — | — |
| Cohort extraction | ✓ | — | — | ✓ | ✓ | — |
| Count-first display mode | ✓ | — | — | — | — | — |
| Auto-sized file export | ✓ | — | ✓ | — | — | — |
| Graphviz/DOT output | ✓ | ✓ | — | ✓ | — | — |
| Interactive HTML output^(†) | — | — | — | — | — | ✓ |
| Pipe-friendly API | ✓ | — | ✓ | ✓ | ✓ | — |

**Legend:** ✓ Full support \| ◐ Partial support \| — Not available

^(†) Currently, `selecta` DOT engine output is embeddable as an HTML
widget via
[`DiagrammeR::grViz()`](https://rich-iannone.github.io/DiagrammeR/reference/grViz.html),
but widget content is a static SVG without native tooltips or clickable
hyperlinks.

------------------------------------------------------------------------

### Feature Definitions

| Feature | Description |
|:---|:---|
| **CONSORT** | Flow diagrams for randomized controlled trials with allocation and per-arm follow-up |
| **STROBE** | Flow diagrams for observational cohort studies with exposure stratification |
| **STARD** | Diagnostic accuracy diagrams with inverted assessment labels and cross-classification |
| **PRISMA** | Systematic review diagrams with multi-source identification and convergence |
| **MOOSE** | Observational meta-analysis diagrams (structurally similar to PRISMA) |
| **Data-driven counts** | Participant counts computed automatically from a dataset |
| **Manual counts** | Participant counts supplied directly by the analyst |
| **Multi-arm layouts (3+)** | Support for three or more parallel arms with automatic layout |
| **Factorial (nested-split) designs** | Cross-classification by two factors via chained [`allocate()`](https://phmcc.codeberg.page/selecta/reference/stratify.md)/[`stratify()`](https://phmcc.codeberg.page/selecta/reference/stratify.md), with parent boxes centered over their sub-arms |
| **Multi-source convergence** | Parallel identification streams merging into a single screening flow |
| **Split-and-recombine topology** | Fanning a population into strata for independent characterization, then converging to a single stream before the endpoint |
| **Phase labels** | Vertical labels (*e.g.,* Enrollment, Allocation, Follow-up, Analysis) |
| **Exclusion sub-reasons** | Itemized counts within exclusion boxes (*e.g.,* “Declined: 12, Other: 8”) |
| **Per-arm exclusion reasons** | Different sub-reason breakdowns for each treatment arm |
| **Hierarchical (nested) sub-reasons** | Two-level exclusion breakdowns grouping itemized sub-reasons under broader categories |
| **Cohort extraction** | Return the analysis-ready dataset after all exclusions |
| **Count-first display mode** | Bold count before label (*e.g.,* “450 Drug A” instead of “Drug A, n = 450”) |
| **Auto-sized file export** | Automatic dimension calculation when saving to PDF/PNG/SVG/TIFF |
| **Graphviz/DOT output** | Export diagram as a Graphviz DOT string for external rendering |
| **Interactive HTML output** | Clickable HTML widgets with tooltips and hyperlinks |
| **Pipe-friendly API** | Pipeline construction with `|>` or `%>%` operators |

------------------------------------------------------------------------

## Unique Strengths of *selecta*

The following features distinguish `selecta` from comparable packages:

### Multi-Guideline Support

Most existing packages target a single EQUATOR guideline—typically
CONSORT or PRISMA—requiring different tools for different study types.
`selecta` provides a unified API across five guidelines (CONSORT,
STROBE, STARD, PRISMA, MOOSE), with guideline-specific functions
([`allocate()`](https://phmcc.codeberg.page/selecta/reference/stratify.md)
vs. [`stratify()`](https://phmcc.codeberg.page/selecta/reference/stratify.md),
[`assess()`](https://phmcc.codeberg.page/selecta/reference/assess.md),
[`sources()`](https://phmcc.codeberg.page/selecta/reference/sources.md),
[`combine()`](https://phmcc.codeberg.page/selecta/reference/combine.md))
that share common conventions for exclusions, phases, and rendering.

``` r
# Same rendering pipeline, different study types
enroll(n = 500) |> allocate(...) |> endpoint("Analyzed") |> flowchart()  # CONSORT
enroll(n = 500) |> stratify(...) |> endpoint("Analyzed") |> flowchart()  # STROBE
enroll(n = 500) |> assess(...) |> endpoint("Final") |> flowchart()       # STARD
sources(...) |> combine(...) |> endpoint("Included") |> flowchart()      # PRISMA
```

### Declarative Pipeline Syntax

The pipeline reads top-to-bottom, matching the visual layout of the
diagram and the narrative order in which investigators describe their
enrollment process. Each function corresponds to a single structural
element (enrollment, exclusion, phase, allocation, endpoint), making the
code self-documenting.

### Factorial and Recombination Topologies

`selecta` renders flow shapes that single-split tools cannot express.
Two
[`allocate()`](https://phmcc.codeberg.page/selecta/reference/stratify.md)
or
[`stratify()`](https://phmcc.codeberg.page/selecta/reference/stratify.md)
calls in sequence produce a factorial (nested-split) design,
cross-classifying each arm by a second factor with parent boxes centered
over their sub-arms.
[`combine()`](https://phmcc.codeberg.page/selecta/reference/combine.md)
reconverges parallel streams, supporting both split-and-recombine layout
(stratify, characterize, recombine) and the stepwise pooling of a
crossed design; it may be applied more than once to collapse a factorial
into a single analysis cohort.

``` r
# 2x2 factorial, then pool the second factor back into the first
enroll(n = 480) |>
    allocate(labels = c("Drug A", "Drug B"), n = c(240, 240)) |>
    allocate(labels = c("Vaccine", "Placebo"), n = c(120, 120, 120, 120)) |>
    combine("Pooled by antiviral") |>
    endpoint("Analyzed") |>
    flowchart()
```

### Hierarchical Exclusion Reasons

Exclusion reasons may be itemized one level deep or grouped two levels
deep, with broad categories subdividing into specific sub-reasons. A
named list (manual mode) or a pair of columns (data mode) records the
hierarchy, which both rendering engines display with sub-reasons
indented or bulleted beneath each category.

``` r
enroll(n = 1000) |>
    exclude("Excluded", n = 250,
            reasons = list(
                "Ineligible"     = c("Out of age range" = 70, "Comorbidity" = 55),
                "Declined"       = c("Time commitment" = 40, "Travel burden" = 20),
                "Administrative" = 65)) |>
    endpoint("Enrolled") |>
    flowchart()
```

### Cohort Extraction

The specialized
[`cohort()`](https://phmcc.codeberg.page/selecta/reference/cohort.md)
function returns the dataset remaining after all exclusion criteria have
been applied, enabling a seamless transition from diagram construction
to statistical analysis.

``` r
flow <- enroll(data, id = "patient_id") |>
    exclude("Ineligible", criteria = eligible == FALSE) |>
    allocate("treatment") |>
    endpoint("Analyzed")

# Render the diagram
flowsave(flow, "consort.pdf")

# Extract the analysis-ready cohort
final_data <- cohort(flow)
```

### Performance

Built on `data.table` with memoized text measurement, vectorized box and
edge drawing, and single-pass layout computation. Diagram rendering
completes in under one second for typical clinical trial configurations.

------------------------------------------------------------------------

## Additional Resources

- [Gallery](https://phmcc.codeberg.page/selecta/articles/gallery.md) —
  Example diagrams across all supported guidelines
- [consort documentation](https://cran.r-project.org/package=consort)
- [flowchart
  documentation](https://cran.r-project.org/package=flowchart)
- [dtrackr documentation](https://cran.r-project.org/package=dtrackr)
- [PRISMA2020
  documentation](https://cran.r-project.org/package=PRISMA2020)
- [PRISMAstatement
  documentation](https://cran.r-project.org/package=PRISMAstatement)
