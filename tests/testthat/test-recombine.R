#' Test Suite for Split-and-Recombine Topology
#'
#' Tests covering the split-and-recombine flow topology: a stratify()/
#' allocate() split followed by per-arm exclusions and a combine() that
#' reconverges the streams. Exercises both the structural result (converge
#' edges, single post-combine stream) and count correctness, including the
#' regression in which a data-driven combine reported the pre-exclusion
#' total instead of the post-exclusion total.
#'
#' @details Run with testthat::test_file("tests/testthat/test-recombine.R")

library(testthat)
library(data.table)
library(selecta)

data(selectaex2)

run_compute <- function(flow) selecta:::compute(flow)


### * Structure — stratify() -> combine()

test_that("stratify followed by combine produces a single post-combine stream", {

    flow <- enroll(n = 1000) |>
        stratify(labels = c("Low", "High"), n = c(500, 500),
                 label = "Risk") |>
        exclude("Discontinued", n = c(40, 60)) |>
        combine("Completers") |>
        endpoint("Analysis cohort")

    graph <- run_compute(flow)

    ## Exactly one endpoint after recombination (the streams have merged)
    endpoints <- graph$nodes[role == "endpoint"]
    expect_equal(nrow(endpoints), 1)

    ## The combine node exists as a single main node carrying the label
    combine_node <- graph$nodes[text == "Completers"]
    expect_equal(nrow(combine_node), 1)
})


test_that("combine emits one converge edge per incoming arm", {

    flow <- enroll(n = 900) |>
        stratify(labels = c("A", "B", "C"), n = c(300, 300, 300),
                 label = "Stratum") |>
        exclude("Dropped", n = c(10, 20, 30)) |>
        combine("Recombined") |>
        endpoint("Final")

    graph <- run_compute(flow)

    converge_edges <- graph$edges[edge_type == "converge"]
    expect_equal(nrow(converge_edges), 3)  # one per stratum
})


test_that("re-splitting after combine is permitted", {

    ## stratify -> combine -> stratify (adaptive design)
    flow <- enroll(n = 1000) |>
        stratify(labels = c("Low", "High"), n = c(500, 500),
                 label = "Baseline risk") |>
        combine("Eligible for randomization") |>
        allocate(labels = c("Treatment", "Control"), n = c(500, 500)) |>
        endpoint("Analysis")

    graph <- run_compute(flow)

    ## Two endpoints from the second split
    endpoints <- graph$nodes[role == "endpoint"]
    expect_equal(nrow(endpoints), 2)
})


### * Count Correctness — Manual Mode

test_that("manual combine total equals the sum of post-exclusion arms", {

    flow <- enroll(n = 1000) |>
        stratify(labels = c("Low", "High"), n = c(500, 500),
                 label = "Risk") |>
        exclude("Discontinued", n = c(40, 60)) |>
        combine("Completers") |>
        endpoint("Analysis cohort")

    graph <- run_compute(flow)

    ## (500 - 40) + (500 - 60) = 900
    combine_node <- graph$nodes[text == "Completers"]
    expect_equal(combine_node$n, 900)

    endpoints <- graph$nodes[role == "endpoint"]
    expect_equal(endpoints$n, 900)
})


test_that("manual combine without exclusions equals the split total", {

    flow <- enroll(n = 1000) |>
        stratify(labels = c("Low", "High"), n = c(500, 500),
                 label = "Risk") |>
        combine("Pooled") |>
        endpoint("Final")

    graph <- run_compute(flow)
    combine_node <- graph$nodes[text == "Pooled"]
    expect_equal(combine_node$n, 1000)
})


### * Count Correctness — Data-Driven Mode (regression)
## A data-driven combine() must subtract per-arm exclusions applied after the
## split. The combine node previously reported the pre-exclusion split total
## because the per-arm exclusion did not update the stored stream count in
## data mode, even though cohorts() (which recomputes from the data) was
## correct. These tests lock in agreement between the two.

test_that("data-driven combine subtracts per-arm exclusions", {

    flow <- enroll(selectaex2, id = "patient_id") |>
        exclude("Failed eligibility", criterion = eligible == FALSE,
                included_label = "Eligible cohort") |>
        stratify("treatment", label = "Treatment assignment") |>
        exclude("Discontinued", criterion = discontinued == TRUE) |>
        combine("Completers") |>
        endpoint("Analysis cohort")

    graph <- run_compute(flow)

    ## Locate the per-arm post-exclusion main nodes (arm streams) feeding the
    ## combine. Their counts must sum to the combine node's count.
    combine_node <- graph$nodes[text == "Completers"]
    expect_equal(nrow(combine_node), 1)

    ## The combine count equals the post-combine endpoint count
    endpoints <- graph$nodes[role == "endpoint"]
    expect_equal(endpoints$n, combine_node$n)

    ## And it is strictly smaller than the pre-exclusion split total
    ## (arm nodes record the counts at the moment of the split)
    split_total <- graph$nodes[role == "arm", sum(n)]
    expect_true(combine_node$n < split_total)
})


test_that("data-driven combine count matches cohorts() remaining", {

    flow <- enroll(selectaex2, id = "patient_id") |>
        exclude("Failed eligibility", criterion = eligible == FALSE,
                included_label = "Eligible cohort") |>
        stratify("treatment", label = "Treatment assignment") |>
        exclude("Discontinued", criterion = discontinued == TRUE) |>
        combine("Completers") |>
        endpoint("Analysis cohort")

    graph  <- run_compute(flow)
    stages <- cohorts(flow)

    combine_node <- graph$nodes[text == "Completers"]

    ## The figure shown in the diagram must equal the data actually retained
    expect_equal(combine_node$n, nrow(stages[["Completers"]]$included))

    ## cohort() (final, recombined) agrees as well
    expect_equal(nrow(cohort(flow)), combine_node$n)
})


test_that("cohorts() exposes per-arm snapshots at the stratified exclusion", {

    flow <- enroll(selectaex2, id = "patient_id") |>
        exclude("Failed eligibility", criterion = eligible == FALSE,
                included_label = "Eligible cohort") |>
        stratify("treatment", label = "Treatment assignment") |>
        exclude("Discontinued", criterion = discontinued == TRUE) |>
        combine("Completers") |>
        endpoint("Analysis cohort")

    stages <- cohorts(flow)
    disc   <- stages[["Discontinued"]]

    ## Per-arm region: remaining and excluded are per-arm named lists
    expect_true(is.list(disc$included))
    expect_true(is.list(disc$excluded))
    expect_equal(length(disc$included), 2)  # 2 treatment arms

    ## Per-arm remaining sums to the combined remaining
    per_arm_included <- sum(vapply(disc$included, nrow, integer(1L)))
    expect_equal(per_arm_included, nrow(stages[["Completers"]]$included))
})


### * Rendering

test_that("split-and-recombine flow renders to PDF without error", {

    flow <- enroll(n = 1000) |>
        phase("Baseline") |>
        stratify(labels = c("Low", "High"), n = c(500, 500),
                 label = "Risk stratum") |>
        phase("Follow-up") |>
        exclude("Discontinued", n = c(40, 60)) |>
        combine("Completers") |>
        phase("Analysis") |>
        endpoint("Analysis cohort")

    f <- tempfile(fileext = ".pdf")
    on.exit(unlink(f), add = TRUE)

    expect_no_error(flowsave(flow, f))
    expect_true(file.exists(f))
    expect_true(file.size(f) > 0)
})
