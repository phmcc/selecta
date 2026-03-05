#' Test Suite for Compute Engine
#'
#' Tests covering compute() output: node/edge structure, count correctness,
#' phase assignments, and data-driven vs manual mode consistency.
#'
#' @details Run with testthat::test_file("tests/testthat/test-compute.R")

library(testthat)
library(data.table)
library(selecta)

data(rctselect0)
data(rctselect2)
data(rctselect3)
data(rctselect6)


## ============================================================================
## Setup: Helper functions
## ============================================================================

## Helper: compute and return graph
run_compute <- function(flow) {
    selecta:::compute(flow)
}

## Helper: check graph structure
expect_graph <- function(graph) {
    expect_true(is.list(graph))
    expect_true("nodes" %in% names(graph))
    expect_true("edges" %in% names(graph))
    expect_true("phases" %in% names(graph))
    expect_s3_class(graph$nodes, "data.table")
    expect_s3_class(graph$edges, "data.table")
}


## ============================================================================
## SECTION 1: Basic Graph Structure
## ============================================================================

test_that("compute returns valid graph for minimal pipeline", {

    flow <- enroll(n = 100) |> endpoint("Final")
    graph <- run_compute(flow)

    expect_graph(graph)
    expect_true(nrow(graph$nodes) >= 2)  # at least enroll + endpoint
    expect_true(nrow(graph$edges) >= 1)
})


test_that("compute returns correct node roles", {

    flow <- enroll(n = 1000) |>
        exclude("Excluded", n = 200,
                included_label = "Eligible") |>
        allocate(labels = c("A", "B"), n = c(400, 400)) |>
        endpoint("Final")

    graph <- run_compute(flow)
    roles <- unique(graph$nodes$role)

    expect_true("main" %in% roles)
    expect_true("side" %in% roles)
    expect_true("alloc" %in% roles || "arm" %in% roles)
    expect_true("endpoint" %in% roles)
})


test_that("compute returns correct edge types", {

    flow <- enroll(n = 1000) |>
        exclude("Excluded", n = 200,
                included_label = "Eligible") |>
        endpoint("Final")

    graph <- run_compute(flow)
    edge_types <- unique(graph$edges$edge_type)

    expect_true("flow" %in% edge_types)
    expect_true("exclude" %in% edge_types)
})


## ============================================================================
## SECTION 2: Count Correctness — Manual Mode
## ============================================================================

test_that("compute tracks counts through single exclusion", {

    flow <- enroll(n = 500) |>
        exclude("Ineligible", n = 65, show_count = TRUE) |>
        endpoint("Final")

    graph <- run_compute(flow)
    endpoints <- graph$nodes[role == "endpoint"]

    expect_equal(endpoints$n, 435)
})


test_that("compute tracks counts through chained exclusions", {

    flow <- enroll(n = 1000) |>
        exclude("Step 1", n = 100, show_count = TRUE) |>
        exclude("Step 2", n = 50, show_count = TRUE) |>
        exclude("Step 3", n = 25, show_count = TRUE) |>
        endpoint("Final")

    graph <- run_compute(flow)
    endpoints <- graph$nodes[role == "endpoint"]

    expect_equal(endpoints$n, 825)
})


test_that("compute splits counts correctly for 2-arm allocation", {

    flow <- enroll(n = 900) |>
        allocate(labels = c("Drug A", "Placebo"), n = c(450, 450)) |>
        endpoint("Final")

    graph <- run_compute(flow)
    endpoints <- graph$nodes[role == "endpoint"]

    expect_equal(nrow(endpoints), 2)
    expect_equal(sort(endpoints$n), c(450, 450))
})


test_that("compute handles per-arm exclusions", {

    flow <- enroll(n = 900) |>
        allocate(labels = c("A", "B"), n = c(450, 450)) |>
        exclude("Lost", n = c(20, 30)) |>
        endpoint("Final")

    graph <- run_compute(flow)
    endpoints <- graph$nodes[role == "endpoint"]

    expect_equal(nrow(endpoints), 2)
    expect_equal(sort(endpoints$n), c(420, 430))
})


test_that("compute handles assess (STARD) with not_received", {

    flow <- enroll(n = 500) |>
        assess("Index test", not_received = 22) |>
        endpoint("Final")

    graph <- run_compute(flow)
    endpoints <- graph$nodes[role == "endpoint"]

    expect_equal(endpoints$n, 478)
})


test_that("compute handles chained assess steps", {

    flow <- enroll(n = 500) |>
        assess("Index test", not_received = 22) |>
        assess("Reference standard", not_received = 18) |>
        endpoint("Final")

    graph <- run_compute(flow)
    endpoints <- graph$nodes[role == "endpoint"]

    expect_equal(endpoints$n, 460)
})


## ============================================================================
## SECTION 3: Count Correctness — Data-Driven Mode
## ============================================================================

test_that("compute data-driven counts match manual expectations for 0-arm", {

    flow <- enroll(rctselect0, id = "patient_id") |>
        exclude("Duplicates", expr = is_duplicate == TRUE,
                included_label = "Unique") |>
        endpoint("Final")

    graph <- run_compute(flow)

    ## First node should have N = nrow(data)
    first_node <- graph$nodes[node_id == 1]
    expect_equal(first_node$n, nrow(rctselect0))

    ## Side box should count excluded
    side_nodes <- graph$nodes[role == "side"]
    expect_true(nrow(side_nodes) > 0)
    expect_true(all(side_nodes$n > 0))
})


test_that("compute data-driven 2-arm produces 2 endpoints", {

    flow <- enroll(rctselect2, id = "patient_id") |>
        allocate("treatment") |>
        endpoint("Final")

    graph <- run_compute(flow)
    endpoints <- graph$nodes[role == "endpoint"]

    expect_equal(nrow(endpoints), 2)
    expect_true(all(endpoints$n > 0))
    ## With no post-allocation exclusions, endpoint counts equal arm counts
    arm_n <- graph$nodes[role == "arm", sum(n)]
    expect_equal(sum(endpoints$n), arm_n)
})


test_that("compute data-driven 3-arm produces 3 endpoints", {

    flow <- enroll(rctselect3, id = "patient_id") |>
        allocate("treatment") |>
        endpoint("Final")

    graph <- run_compute(flow)
    endpoints <- graph$nodes[role == "endpoint"]

    expect_equal(nrow(endpoints), 3)
})


test_that("compute data-driven 6-arm produces 6 endpoints", {

    flow <- enroll(rctselect6, id = "patient_id") |>
        stratify("treatment") |>
        endpoint("Final")

    graph <- run_compute(flow)
    endpoints <- graph$nodes[role == "endpoint"]

    expect_equal(nrow(endpoints), 6)
})


## ============================================================================
## SECTION 4: show_count Behaviour
## ============================================================================

test_that("show_count = FALSE suppresses intermediate count node", {

    ## show_count = FALSE (default): count node suppressed after first exclude
    flow_no <- enroll(n = 500) |>
        exclude("First", n = 100) |>
        exclude("Second", n = 50) |>
        endpoint("Final")

    ## show_count = TRUE: count node retained after first exclude
    flow_yes <- enroll(n = 500) |>
        exclude("First", n = 100, show_count = TRUE) |>
        exclude("Second", n = 50) |>
        endpoint("Final")

    graph_no  <- run_compute(flow_no)
    graph_yes <- run_compute(flow_yes)

    ## show_count = TRUE should produce more main nodes
    main_no  <- graph_no$nodes[role == "main"]
    main_yes <- graph_yes$nodes[role == "main"]
    expect_true(nrow(main_yes) > nrow(main_no))
})


test_that("included_label overrides show_count = FALSE", {

    flow <- enroll(n = 500) |>
        exclude("Excluded", n = 100, included_label = "Eligible") |>
        exclude("Withdrew", n = 20) |>
        endpoint("Final")

    graph <- run_compute(flow)

    ## Should have count node with label "Eligible" despite default show_count = FALSE
    eligible_nodes <- graph$nodes[text == "Eligible"]
    expect_equal(nrow(eligible_nodes), 1)
    expect_equal(eligible_nodes$n, 400)
})


test_that("show_count auto-suppressed before allocate", {

    ## Even with show_count = TRUE, the node before allocate is suppressed
    flow <- enroll(n = 500) |>
        exclude("Excluded", n = 100, show_count = TRUE) |>
        allocate(labels = c("A", "B"), n = c(200, 200)) |>
        endpoint("Final")

    graph <- run_compute(flow)

    ## The intermediate count node should NOT appear between exclude and alloc
    ## because allocate provides its own count
    main_nodes <- graph$nodes[role == "main" & is.na(arm_id)]
    ## First node (enroll) only — no intermediate count before alloc
    expect_equal(nrow(main_nodes), 1)
})


## ============================================================================
## SECTION 5: Phase Tracking
## ============================================================================

test_that("phases are correctly assigned to nodes", {

    flow <- enroll(n = 500) |>
        phase("Screening") |>
        exclude("Excluded", n = 100) |>
        phase("Analysis") |>
        endpoint("Final")

    graph <- run_compute(flow)

    expect_true(nrow(graph$phases) >= 2)
    phase_labels <- graph$phases$label
    expect_true("Screening" %in% phase_labels)
    expect_true("Analysis" %in% phase_labels)
})


## ============================================================================
## SECTION 6: Endpoint with Reasons
## ============================================================================

test_that("compute stores endpoint reasons in node", {

    flow <- enroll(n = 500) |>
        endpoint("Final diagnosis",
                 reasons = c("Target +" = 300, "Target -" = 200))

    graph <- run_compute(flow)
    ep_node <- graph$nodes[role == "endpoint"]

    expect_equal(nrow(ep_node), 1)
    reas <- ep_node$reasons[[1]]
    expect_true(!is.null(reas))
    expect_equal(length(reas), 2)
})


test_that("compute stores per-arm endpoint reasons", {

    flow <- enroll(n = 500) |>
        stratify(labels = c("Pos", "Neg"), n = c(200, 300),
                 label = "Result") |>
        endpoint("Final diagnosis",
                 reasons = list(c("Target +" = 160, "Target -" = 40),
                                c("Target +" = 25, "Target -" = 275)))

    graph <- run_compute(flow)
    ep_nodes <- graph$nodes[role == "endpoint"]

    expect_equal(nrow(ep_nodes), 2)
    for (i in 1:2) {
        reas <- ep_nodes$reasons[[i]]
        expect_true(!is.null(reas))
        expect_equal(length(reas), 2)
    }
})


## ============================================================================
## SECTION 7: Source Convergence (PRISMA)
## ============================================================================

test_that("compute creates convergence edges from sources to combine", {

    flow <- sources(
        databases = c("PubMed" = 100, "Embase" = 200),
        other     = c("Manual" = 50),
        headers   = c(databases = "DBs", other = "Other")) |>
        combine("Records identified") |>
        endpoint("Included")

    graph <- run_compute(flow)

    converge_edges <- graph$edges[edge_type == "converge"]
    expect_true(nrow(converge_edges) >= 2)  # one per source group
})


test_that("compute creates source_header nodes for grouped sources", {

    flow <- sources(
        databases = c("PubMed" = 100, "Embase" = 200),
        other     = c("Manual" = 50),
        headers   = c(databases = "DBs", other = "Other")) |>
        combine("Records identified") |>
        endpoint("Included")

    graph <- run_compute(flow)

    header_nodes <- graph$nodes[role == "source_header"]
    expect_true(nrow(header_nodes) >= 2)
})
