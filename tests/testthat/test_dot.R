#' Test Suite for Graphviz DOT Output
#'
#' Tests covering to_dot() via flowchart(engine = "dot") for all
#' diagram types: CONSORT, STROBE, STARD, PRISMA, and MOOSE.
#'
#' @details Run with testthat::test_file("tests/testthat/test-dot.R")

library(testthat)
library(data.table)
library(selecta)


## ============================================================================
## Setup: Reusable flows
## ============================================================================

data(rctselect2)

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
    endpoint("Analysed")

flow_dd2 <- enroll(rctselect2, id = "patient_id") |>
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
             reasons = list(c("Target condition +" = 160,
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


## Helper: validate DOT string structure
expect_valid_dot <- function(dot_str) {
    expect_type(dot_str, "character")
    expect_length(dot_str, 1L)
    expect_match(dot_str, "^digraph selecta")
    expect_match(dot_str, "\\}\\s*$")
}


## ============================================================================
## SECTION 1: DOT String Generation
## ============================================================================

test_that("DOT output for 0-arm CONSORT is valid", {
    dot <- flowchart(flow_0arm, engine = "dot")
    expect_valid_dot(dot)
    expect_match(dot, "style=dashed")  # exclude edges
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
    ## Source nodes get green fill
    expect_match(dot, "#e8f4e8")
    ## Source headers get darker green
    expect_match(dot, "#d4e8d4")
})

test_that("DOT output for PRISMA 1-column is valid", {
    dot <- flowchart(flow_prisma1, engine = "dot")
    expect_valid_dot(dot)
    ## Flat sources produce a single consolidated source node
    expect_match(dot, "#e8f4e8")  # source node fill colour
})


## ============================================================================
## SECTION 2: DOT Structure
## ============================================================================

test_that("DOT nodes have label and fillcolor attributes", {
    dot <- flowchart(flow_2arm, engine = "dot")
    node_lines <- regmatches(dot, gregexpr("n\\d+ \\[label=.*?\\];", dot))[[1L]]
    expect_true(length(node_lines) > 0)
    expect_true(all(grepl("fillcolor=", node_lines)))
})

test_that("DOT edges use dashed style for exclusions", {
    dot <- flowchart(flow_2arm, engine = "dot")
    expect_match(dot, "style=dashed")
})

test_that("DOT exclude side boxes use light grey fill", {
    dot <- flowchart(flow_0arm, engine = "dot")
    expect_match(dot, "#f0f0f0")
})

test_that("DOT convergence edges use bold style", {
    dot <- flowchart(flow_prisma, engine = "dot")
    expect_match(dot, "style=bold")
})


## ============================================================================
## SECTION 3: plot.selecta Dispatch
## ============================================================================

test_that("plot.selecta dispatches to DOT engine", {
    dot_fc   <- flowchart(flow_0arm, engine = "dot")
    dot_plot <- plot(flow_0arm, engine = "dot")
    expect_identical(dot_fc, dot_plot)
})
