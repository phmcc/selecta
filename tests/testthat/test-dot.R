#' Test Suite for Graphviz DOT Output
#'
#' Tests covering export_dot() via flowchart(engine = "dot") for all
#' diagram types: CONSORT, STROBE, STARD, PRISMA, and MOOSE.
#'
#' @details Run with testthat::test_file("tests/testthat/test-dot.R")

library(testthat)
library(data.table)
library(selecta)


### * Setup: Reusable flows

data(selectaex2)

flow_0arm <- enroll(n = 500) |>
    phase("Enrollment") |>
    exclude("Ineligible", n = 65,
            reasons = c("Age < 18" = 30, "No consent" = 35)) |>
    phase("Analysis") |>
    endpoint("Final cohort")

flow_2arm <- enroll(n = 1200, label = "Assessed for eligibility") |>
    phase("Enrollment") |>
    exclude("Excluded", n = 300,
            reasons = c("Not meeting criteria" = 160,
                        "Declined" = 90, "Other" = 50)) |>
    phase("Allocation") |>
    allocate(labels = c("Drug A", "Placebo"), n = c(450, 450)) |>
    phase("Follow-up") |>
    exclude("Lost to follow-up", n = c(20, 20)) |>
    phase("Analysis") |>
    endpoint("Analyzed")

flow_dd2 <- enroll(selectaex2, id = "patient_id") |>
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

flow_strobe <- enroll(n = 3860, label = "Registry patients") |>
    phase("Enrollment") |>
    exclude("Excluded", n = 420,
            reasons = c("Missing exposure data" = 210,
                        "Prior treatment" = 130,
                        "Withdrew consent" = 80)) |>
    phase("Stratification") |>
    stratify(labels = c("Low exposure", "Medium exposure", "High exposure"),
             n = c(1200, 1300, 940),
             label = "Exposure level") |>
    phase("Follow-up") |>
    exclude("Lost to follow-up", n = c(60, 75, 45)) |>
    phase("Analysis") |>
    endpoint("Analysis cohort")

flow_stard <- enroll(n = 500, label = "Potentially eligible patients") |>
    phase("Enrollment") |>
    exclude("Excluded", n = 40,
            reasons = c("Refused" = 25, "Not meeting criteria" = 15)) |>
    phase("Index test") |>
    assess("Index test", not_received = 22,
           reasons = c("Refused" = 12, "Contraindicated" = 10)) |>
    phase("Reference standard") |>
    assess("Reference standard", not_received = 18,
           reasons = c("Lost to follow-up" = 10, "Inconclusive" = 8)) |>
    phase("Results") |>
    stratify(labels = c("Index test positive", "Index test negative"),
             n = c(180, 240),
             label = "Index test result") |>
    endpoint("Final diagnosis",
             breakdown = list(c("Target condition +" = 160,
                               "Target condition -" = 20),
                             c("Target condition +" = 15,
                               "Target condition -" = 225)))

flow_prisma <- sources(
    previous  = c("Previous review" = 12, "Previous reports" = 15),
    databases = c("PubMed" = 1234, "Embase" = 567, "CENTRAL" = 89),
    other     = c("Citation search" = 55, "Websites" = 34),
    headers   = c(previous  = "Previous studies",
                  databases = "Databases and registers",
                  other     = "Other methods")
) |>
    phase("Identification") |>
    combine("Records after deduplication") |>
    exclude("Duplicates removed", n = 340,
            included_label = "Records screened") |>
    phase("Screening") |>
    exclude("Records excluded", n = 800,
            reasons = c("Irrelevant" = 600, "No full text" = 200)) |>
    phase("Included") |>
    endpoint("Studies included in review")

flow_prisma1 <- sources(PubMed = 1234, Embase = 567, CENTRAL = 89) |>
    phase("Identification") |>
    combine("Records identified") |>
    exclude("Duplicates removed", n = 340,
            included_label = "Records screened") |>
    phase("Screening") |>
    exclude("Records excluded", n = 800) |>
    phase("Included") |>
    endpoint("Studies included")


### ** Helper: validate DOT string structure
expect_valid_dot <- function(dot_str) {
    expect_type(dot_str, "character")
    expect_length(dot_str, 1L)
    expect_match(dot_str, "^digraph selecta")
    expect_match(dot_str, "\\}\\s*$")
}


### * DOT String Generation

test_that("DOT output for 0-arm CONSORT is valid", {
    dot <- flowchart(flow_0arm, engine = "dot")
    expect_valid_dot(dot)
    ## Exclusions render as white side boxes joined to the trunk by an
    ## orthogonal tee (solid black), following the EQUATOR convention rather
    ## than a dashed edge style.
    expect_match(dot, "Ineligible \\(n = 65\\)")
    expect_match(dot, "splines=ortho")
})

test_that("DOT output for 2-arm manual CONSORT is valid", {
    dot <- flowchart(flow_2arm, engine = "dot")
    expect_valid_dot(dot)
    ## Should contain both arm labels
    expect_match(dot, "Drug A")
    expect_match(dot, "Placebo")
})

test_that("DOT output for 2-arm data-driven CONSORT is valid", {
    dot <- flowchart(flow_dd2, engine = "dot")
    expect_valid_dot(dot)
})

test_that("DOT output for STROBE is valid", {
    dot <- flowchart(flow_strobe, engine = "dot")
    expect_valid_dot(dot)
    expect_match(dot, "Low exposure")
    expect_match(dot, "Medium exposure")
    expect_match(dot, "High exposure")
})

test_that("DOT output for STARD is valid", {
    dot <- flowchart(flow_stard, engine = "dot")
    expect_valid_dot(dot)
    expect_match(dot, "Index test positive")
})

test_that("DOT output for PRISMA 3-column is valid", {
    dot <- flowchart(flow_prisma, engine = "dot")
    expect_valid_dot(dot)
    ## Source headers get a darker gray fill with bold black text, matching
    ## the grid engine
    expect_match(dot, "#D0D0D0")
    expect_match(dot, 'fontname="Helvetica-Bold"')
    ## Source boxes use the standard white fill
    expect_match(dot, 'fillcolor="#FFFFFF"')
})

test_that("DOT output for PRISMA 1-column is valid", {
    dot <- flowchart(flow_prisma1, engine = "dot")
    expect_valid_dot(dot)
    ## Flat sources produce a single consolidated white source node
    expect_match(dot, 'fillcolor="#FFFFFF"')
})


### * DOT Structure

test_that("DOT nodes have label and fillcolor attributes", {
    dot <- flowchart(flow_2arm, engine = "dot")
    node_lines <- regmatches(dot, gregexpr("n\\d+ \\[label=.*?\\];", dot))[[1L]]
    expect_true(length(node_lines) > 0)
    expect_true(all(grepl("fillcolor=", node_lines)))
})

test_that("DOT renders exclusions as solid orthogonal side branches", {
    dot <- flowchart(flow_2arm, engine = "dot")
    ## Top-level and per-arm exclusions both render as white side boxes; the
    ## edges are solid black, with structure carried by the orthogonal
    ## point-routing rather than a dashed line style.
    expect_match(dot, "Excluded \\(n = 300\\)")
    expect_match(dot, "Lost to follow-up \\(n = 20\\)")
    expect_match(dot, 'color="black"')
})

test_that("DOT exclude side boxes use white fill by default", {
    dot <- flowchart(flow_0arm, engine = "dot")
    ## Side (exclusion) boxes follow the EQUATOR convention of plain white
    ## boxes throughout, matching the grid engine.
    expect_match(dot, 'Ineligible.*fillcolor="#FFFFFF"')
})

test_that("side_fill parameter overrides the default white side box", {
    ## The white default is configurable; a gray such as #F0F0F0 sets exclusion
    ## boxes apart from the trunk when desired.
    dot <- flowchart(flow_0arm, engine = "dot", side_fill = "#F0F0F0")
    expect_match(dot, "#F0F0F0")
})

test_that("DOT source convergence feeds the combine node", {
    dot <- flowchart(flow_prisma, engine = "dot")
    ## Source columns converge into the combine node via orthogonal point
    ## routing (solid black), not a bold edge.
    expect_match(dot, "Records after deduplication")
    expect_match(dot, "PubMed")
    expect_match(dot, 'group="src_databases"')
})


### * plot.selecta Dispatch

test_that("plot.selecta dispatches to DOT engine", {
    dot_fc   <- flowchart(flow_0arm, engine = "dot")
    dot_plot <- plot(flow_0arm, engine = "dot")
    expect_identical(dot_fc, dot_plot)
})


### * Split-and-Recombine and Formatting

test_that("DOT output for split-and-recombine is valid", {

    flow <- enroll(n = 1000) |>
        stratify(labels = c("Low", "High"), n = c(500, 500),
                 label = "Risk") |>
        exclude("Discontinued", n = c(40, 60)) |>
        combine("Completers") |>
        endpoint("Analysis cohort")

    dot <- flowchart(flow, engine = "dot")
    expect_valid_dot(dot)
    ## Recombination merges the arms into a single node via orthogonal point
    ## routing (solid black); both arms and the merged node are present.
    expect_match(dot, 'group="arm_1"')
    expect_match(dot, 'group="arm_2"')
    expect_match(dot, "Completers")
})


test_that("DOT honors number_format for large counts", {

    flow <- enroll(n = 1284500, label = "Invited") |>
        exclude("Did not attend", n = 458200,
                included_label = "Attended") |>
        endpoint("Surveillance")

    ## US grouping uses commas; EU grouping uses periods
    dot_us <- flowchart(flow, engine = "dot", number_format = "us")
    expect_match(dot_us, "1,284,500")

    dot_eu <- flowchart(flow, engine = "dot", number_format = "eu")
    expect_match(dot_eu, "1\\.284\\.500")
})


test_that("DOT rich formatting renders without error", {

    dot <- flowchart(flow_2arm, engine = "dot", formatting = "rich")
    expect_valid_dot(dot)
})


### ** Sub-item rendering

test_that("DOT renders exclusion sub-reasons inside the box", {

    ## Sub-reasons are now emitted in the side-box label (plain mode); the
    ## reason names and their counts both appear.
    dot <- flowchart(flow_stard, engine = "dot")
    expect_match(dot, "Refused")
    expect_match(dot, "Contraindicated")
    expect_match(dot, "Not meeting criteria")
})

test_that("DOT renders endpoint breakdown sub-items", {

    ## STARD per-arm endpoints carry a target-condition breakdown, which now
    ## appears beneath each final-diagnosis count.
    dot <- flowchart(flow_stard, engine = "dot")
    expect_match(dot, "Target condition \\+")
})

test_that("DOT sub-reasons render in rich formatting too", {

    dot <- flowchart(flow_stard, engine = "dot", formatting = "rich")
    expect_valid_dot(dot)
    expect_match(dot, "Refused")
    ## Left-aligned breaks are used for the indented reason lines
    expect_match(dot, 'align="left"')
})

test_that("DOT source boxes list their individual sources", {

    ## Multi-source PRISMA boxes now show the per-source breakdown.
    dot <- flowchart(flow_prisma, engine = "dot")
    expect_match(dot, "PubMed")
    expect_match(dot, "Embase")
})


### ** Multi-source header positioning

test_that("three-source DOT aligns headers above their columns", {

    ## Three source groups: each header must be tied above its own source
    ## box (invisible edge) and all headers held on one rank, so Graphviz
    ## does not interleave the headers and boxes in a single row.
    dot <- flowchart(flow_prisma, engine = "dot")

    ## One invisible edge per group header (3 here)
    invis <- regmatches(dot, gregexpr("n\\d+ -> n\\d+ \\[style=invis[^]]*\\];", dot))[[1L]]
    expect_equal(length(invis), 3L)

    ## Headers share a single rank
    expect_match(dot, "rank=same")
})


test_that("two-source DOT also constrains header placement", {

    flow <- sources(
        databases = c("PubMed" = 100, "Embase" = 200),
        other     = c("Manual" = 50),
        headers   = c(databases = "Databases", other = "Other")) |>
        combine("Records identified") |>
        endpoint("Included")

    dot <- flowchart(flow, engine = "dot")
    invis <- regmatches(dot, gregexpr("n\\d+ -> n\\d+ \\[style=invis[^]]*\\];", dot))[[1L]]
    expect_equal(length(invis), 2L)
    expect_match(dot, "rank=same")
})


test_that("flat sources without headers emit no source-positioning constraints", {

    ## A flat (1-column) sources() call has no group headers, so no
    ## source-positioning invisible edges (n -> n) should appear. The
    ## phase-label column chain (PL -> PL) is a separate concern and may still
    ## be present, so the check targets the header edges specifically.
    dot <- flowchart(flow_prisma1, engine = "dot")
    src_invis <- regmatches(
        dot, gregexpr("n\\d+ -> n\\d+ \\[style=invis[^]]*\\];", dot))[[1L]]
    expect_equal(length(src_invis), 0L)
})
