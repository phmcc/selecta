# Development Roadmap

This article outlines planned features for future versions of `selecta`,
informed by capabilities available in related packages, EQUATOR
guideline requirements, and user feedback. The roadmap is tentative and
subject to change based on development priorities and community input.

------------------------------------------------------------------------

## Planned Features

The following features are under consideration for future releases,
organized by priority level.

### Higher Priority

Features expected in near-term releases:

| Feature | Description | Reference |
|:---|:---|:---|
| R Markdown / Quarto integration | Native chunk output for [`flowchart()`](https://phmcc.codeberg.page/selecta/reference/flowchart.md) without the `queue_flow()` workaround | `flowchart`, `consort` |
| Interactive SVG / HTML output | Optional per-node `tooltip=` / `URL=` attributes in the `dot` engine, so the Graphviz SVG and the [`DiagrammeR::grViz()`](https://rich-iannone.github.io/DiagrammeR/reference/grViz.html) widget carry hover text and clickable hyperlinks (*e.g.,* linking nodes to methods sections) | `PRISMA2020` |
| ggplot2 rendering engine | Optional `engine = "ggplot"` for compatibility with `patchwork`, `cowplot`, and journal theme systems | `ggconsort`, `flowchart` |

### Medium Priority

Features planned for subsequent releases:

| Feature | Description | Reference |
|:---|:---|:---|
| Inline text reporting | Extract counts at any pipeline stage for R Markdown inline reporting (*e.g.,* `cohort_n(flow, "Eligible")`) | `dtrackr` |
| Crossover trial support | Period-based layouts for crossover and N-of-1 designs | CONSORT extensions |
| Cluster-randomized layouts | Site-level and individual-level flow within the same diagram | CONSORT extensions |
| Custom node shapes | User-defined node geometry beyond rectangular boxes (*e.g.,* diamonds for decision points, rounded boxes for assessments) | `DiagrammeR` |
| Annotation layers | Free-text annotations, footnotes, and bracketed comments overlaid on the diagram | General |

### Lower Priority

Features under consideration for future releases:

| Feature | Description | Reference |
|:---|:---|:---|
| Mermaid output | `engine = "mermaid"` for native Quarto embedding without DiagrammeR | Quarto |

------------------------------------------------------------------------

## Contributing

Contributions to `selecta` are welcome. Prospective contributors
interested in implementing a roadmap feature or proposing a new one are
directed to the contributing guidelines in the [package
repository](https://codeberg.org/phmcc/selecta).

### Bug Reports

Bug reports and feature requests may be submitted via the issue tracker,
either on [Codeberg](https://codeberg.org/phmcc/selecta/issues) or
[GitHub](https://github.com/phmcc/selecta/issues).

A report concerning diagram layout—cropping, spacing, phase-label
wrapping, or figure dimensions—is most useful when accompanied by the
package’s layout trace. Setting `options(selecta.debug_layout = TRUE)`
before calling
[`flowchart()`](https://phmcc.codeberg.page/selecta/reference/flowchart.md),
[`flowsave()`](https://phmcc.codeberg.page/selecta/reference/flowsave.md),
or
[`recdims()`](https://phmcc.codeberg.page/selecta/reference/recdims.md)
causes the computation and rendering functions to print the resolved
node and edge tables, the computed positions, the recommended
dimensions, the per-phase band heights, and the generated DOT source.
The trace is emitted through
[`message()`](https://rdrr.io/r/base/message.html) and is disabled by
default; pasting it into the report, together with a minimal
reproducible pipeline, allows a layout issue to be diagnosed without a
round trip.

### Development Repository

- **Primary development**:
  [codeberg.org/phmcc/selecta](https://codeberg.org/phmcc/selecta)
- **GitHub mirror**:
  [github.com/phmcc/selecta](https://github.com/phmcc/selecta)

------------------------------------------------------------------------

## Version History

See the
[Changelog](https://phmcc.codeberg.page/selecta/articles/news/index.md)
for a detailed history of changes in each release.

------------------------------------------------------------------------

## Additional Resources

- [Feature
  Comparison](https://phmcc.codeberg.page/selecta/articles/feature_comparison.md):
  Comparison with related packages
- [Gallery](https://phmcc.codeberg.page/selecta/articles/gallery.md):
  Example diagrams across all supported guidelines
- [consort documentation](https://cran.r-project.org/package=consort)
- [flowchart
  documentation](https://cran.r-project.org/package=flowchart)
- [PRISMA2020
  documentation](https://cran.r-project.org/package=PRISMA2020)
