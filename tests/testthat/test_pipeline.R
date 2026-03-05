#' Test Suite for Core Pipeline Functions
#'
#' Tests covering enroll(), exclude(), allocate(), stratify(), endpoint(),
#' phase(), and assess() — both manual and data-driven modes.
#'
#' @details Run with testthat::test_file("tests/testthat/test-pipeline.R")

library(testthat)
library(data.table)
library(selecta)


## ============================================================================
## Setup: Test data and helper functions
## ============================================================================

data(rctselect0)
data(rctselect2)
data(rctselect3)
data(rctselect6)

## Helper: check valid selecta object
expect_selecta <- function(obj) {
    expect_s3_class(obj, "selecta")
    expect_true(is.list(obj))
    expect_true("steps" %in% names(obj))
    expect_true("mode" %in% names(obj))
}

## Helper: check step type at position
expect_step_type <- function(obj, position, type) {
    expect_true(length(obj$steps) >= position,
                info = paste("Expected at least", position, "steps, got",
                             length(obj$steps)))
    expect_equal(obj$steps[[position]]$type, type)
}


## ============================================================================
## SECTION 1: enroll() — Object Construction
## ============================================================================

test_that("enroll creates valid selecta object in manual mode", {

    flow <- enroll(n = 500)

    expect_selecta(flow)
    expect_equal(flow$mode, "manual")
    expect_equal(flow$n_start, 500)
    expect_null(flow$data)
    expect_null(flow$id)
})


test_that("enroll creates valid selecta object in data-driven mode", {

    flow <- enroll(rctselect2, id = "patient_id")

    expect_selecta(flow)
    expect_equal(flow$mode, "data")
    expect_equal(flow$n_start, nrow(rctselect2))
    expect_equal(flow$id, "patient_id")
    expect_true(!is.null(flow$data))
})


test_that("enroll accepts custom label", {

    flow <- enroll(n = 500, label = "Assessed for eligibility")

    expect_selecta(flow)
    expect_equal(flow$label, "Assessed for eligibility")
})


test_that("enroll defaults label to 'Study Population' when not given", {

    flow <- enroll(n = 500)

    expect_true(is.null(flow$label) || flow$label == "Study Population")
})


test_that("enroll errors without n or data", {

    expect_error(enroll(), regexp = "non-negative integer")
})


test_that("enroll data-driven mode defaults id to first column", {

    flow <- enroll(rctselect2)
    expect_equal(flow$id, names(rctselect2)[1L])
})


## ============================================================================
## SECTION 2: exclude() — Exclusion Logic
## ============================================================================

test_that("exclude appends exclusion step in manual mode", {

    flow <- enroll(n = 500) |>
        exclude("Ineligible", n = 65)

    expect_selecta(flow)
    expect_step_type(flow, 1, "exclude")
    expect_equal(flow$steps[[1]]$label, "Ineligible")
})


test_that("exclude appends exclusion step in data-driven mode", {

    flow <- enroll(rctselect2, id = "patient_id") |>
        exclude("Duplicates", expr = is_duplicate == TRUE)

    expect_selecta(flow)
    expect_step_type(flow, 1, "exclude")
})


test_that("exclude stores named numeric reasons", {

    flow <- enroll(n = 500) |>
        exclude("Excluded", n = 100,
                reasons = c("Age" = 40, "Comorbidity" = 35, "Declined" = 25))

    reas <- flow$steps[[1]]$reasons
    expect_true(!is.null(reas))
    expect_equal(length(reas), 3)
    expect_equal(names(reas), c("Age", "Comorbidity", "Declined"))
    expect_equal(sum(reas), 100)
})


test_that("exclude stores column-name reasons in data-driven mode", {

    flow <- enroll(rctselect2, id = "patient_id") |>
        exclude("Failed eligibility", expr = eligible == FALSE,
                reasons = "exclusion_reason")

    expect_equal(flow$steps[[1]]$reasons_var, "exclusion_reason")
})


test_that("exclude defaults to show_count = FALSE", {

    flow <- enroll(n = 500) |>
        exclude("Ineligible", n = 65)

    expect_false(isTRUE(flow$steps[[1]]$show_count))
})


test_that("exclude show_count = TRUE is stored", {

    flow <- enroll(n = 500) |>
        exclude("Ineligible", n = 65, show_count = TRUE)

    expect_true(flow$steps[[1]]$show_count)
})


test_that("exclude included_label is stored", {

    flow <- enroll(n = 500) |>
        exclude("Ineligible", n = 65, included_label = "Eligible")

    expect_equal(flow$steps[[1]]$included_label, "Eligible")
})


test_that("exclude errors without n in manual mode", {

    expect_error(
        enroll(n = 500) |> exclude("Ineligible"),
        regexp = "n"
    )
})


test_that("exclude errors without expr in data-driven mode", {

    expect_error(
        enroll(rctselect2, id = "patient_id") |> exclude("Ineligible"),
        regexp = "expr"
    )
})


test_that("exclude chains correctly with multiple exclusions", {

    flow <- enroll(n = 1000) |>
        exclude("Step 1", n = 100) |>
        exclude("Step 2", n = 50) |>
        exclude("Step 3", n = 25)

    expect_equal(length(flow$steps), 3)
    for (i in 1:3) expect_step_type(flow, i, "exclude")
})


test_that("exclude per-arm n vectors are stored correctly", {

    flow <- enroll(n = 1000) |>
        allocate(labels = c("A", "B"), n = c(500, 500)) |>
        exclude("Lost", n = c(20, 30))

    step <- flow$steps[[length(flow$steps)]]
    expect_equal(step$n, c(20, 30))
})


test_that("exclude per-arm labels are stored correctly", {

    flow <- enroll(n = 1000) |>
        stratify(labels = c("Exposed", "Unexposed"), n = c(500, 500),
                 label = "Exposure") |>
        exclude(c("Discontinued", "Started treatment"), n = c(20, 30))

    step <- flow$steps[[length(flow$steps)]]
    expect_equal(step$label, c("Discontinued", "Started treatment"))
})


## ============================================================================
## SECTION 3: allocate() and stratify()
## ============================================================================

test_that("allocate creates arm split in manual mode", {

    flow <- enroll(n = 900) |>
        allocate(labels = c("Drug A", "Placebo"), n = c(450, 450))

    expect_selecta(flow)
    expect_step_type(flow, 1, "stratify")  # allocate is alias for stratify
    expect_equal(flow$steps[[1]]$labels, c("Drug A", "Placebo"))
    expect_equal(flow$steps[[1]]$n, c(450, 450))
})


test_that("allocate creates arm split in data-driven mode", {

    flow <- enroll(rctselect2, id = "patient_id") |>
        allocate("treatment")

    expect_selecta(flow)
    expect_step_type(flow, 1, "stratify")
    expect_equal(flow$steps[[1]]$variable, "treatment")
})


test_that("stratify creates arm split in manual mode", {

    flow <- enroll(n = 1000) |>
        stratify(labels = c("Exposed", "Unexposed"), n = c(500, 500),
                 label = "Classified by exposure")

    expect_selecta(flow)
    expect_step_type(flow, 1, "stratify")
    expect_equal(flow$steps[[1]]$label, "Classified by exposure")
})


test_that("stratify supports 3+ arms", {

    flow <- enroll(n = 900) |>
        allocate(labels = c("Drug A", "Drug B", "Placebo"),
                 n = c(300, 300, 300))

    expect_equal(length(flow$steps[[1]]$labels), 3)
    expect_equal(length(flow$steps[[1]]$n), 3)
})


test_that("stratify supports 6 arms", {

    flow <- enroll(n = 1800) |>
        allocate(labels = paste("Arm", LETTERS[1:6]),
                 n = rep(300, 6))

    expect_equal(length(flow$steps[[1]]$labels), 6)
})


## ============================================================================
## SECTION 4: endpoint()
## ============================================================================

test_that("endpoint adds terminal step", {

    flow <- enroll(n = 500) |> endpoint("Final Analysis")

    expect_selecta(flow)
    expect_step_type(flow, 1, "endpoint")
    expect_equal(flow$steps[[1]]$label, "Final Analysis")
})


test_that("endpoint default label is 'Final Analysis'", {

    flow <- enroll(n = 500) |> endpoint()

    expect_equal(flow$steps[[1]]$label, "Final Analysis")
})


test_that("endpoint stores single-stream reasons", {

    flow <- enroll(n = 500) |>
        endpoint("Final diagnosis",
                 reasons = c("Target +" = 160, "Target -" = 340))

    reas <- flow$steps[[1]]$reasons
    expect_true(!is.null(reas))
    expect_equal(length(reas), 2)
    expect_equal(names(reas), c("Target +", "Target -"))
})


test_that("endpoint stores per-arm reasons as list", {

    flow <- enroll(n = 500) |>
        stratify(labels = c("Positive", "Negative"), n = c(200, 300),
                 label = "Index result") |>
        endpoint("Final diagnosis",
                 reasons = list(c("Target +" = 160, "Target -" = 40),
                                c("Target +" = 25, "Target -" = 275)))

    reas <- flow$steps[[2]]$reasons
    expect_true(is.list(reas))
    expect_equal(length(reas), 2)
    expect_equal(sum(reas[[1]]), 200)
    expect_equal(sum(reas[[2]]), 300)
})


## ============================================================================
## SECTION 5: phase()
## ============================================================================

test_that("phase adds phase label step", {

    flow <- enroll(n = 500) |>
        phase("Screening")

    expect_selecta(flow)
    expect_step_type(flow, 1, "phase")
    expect_equal(flow$steps[[1]]$label, "Screening")
})


test_that("multiple phases chain correctly", {

    flow <- enroll(n = 500) |>
        phase("Screening") |>
        exclude("Ineligible", n = 65) |>
        phase("Allocation") |>
        allocate(labels = c("A", "B"), n = c(200, 235)) |>
        phase("Analysis") |>
        endpoint("Final")

    phase_steps <- Filter(function(s) s$type == "phase", flow$steps)
    expect_equal(length(phase_steps), 3)
    expect_equal(
        vapply(phase_steps, function(s) s$label, character(1)),
        c("Screening", "Allocation", "Analysis")
    )
})


## ============================================================================
## SECTION 6: assess() — STARD diagnostic tests
## ============================================================================

test_that("assess adds assessment step in manual mode", {

    flow <- enroll(n = 500) |>
        assess("Index test", not_received = 22)

    expect_selecta(flow)
    expect_equal(length(flow$steps), 1)
    ## assess() stores as an exclude step with inverted label semantics
    expect_step_type(flow, 1, "exclude")
    expect_equal(flow$steps[[1]]$label, "Did not receive index test")
    expect_equal(flow$steps[[1]]$n, 22)
    expect_equal(flow$steps[[1]]$included_label, "Received index test")
})


test_that("assess stores reasons for not receiving test", {

    flow <- enroll(n = 500) |>
        assess("Index test", not_received = 22,
               reasons = c("Refused" = 12, "Contraindicated" = 10))

    reas <- flow$steps[[1]]$reasons
    expect_true(!is.null(reas))
    expect_equal(sum(reas), 22)
})


test_that("assess chains with multiple diagnostic steps", {

    flow <- enroll(n = 500) |>
        assess("Index test", not_received = 22) |>
        assess("Reference standard", not_received = 18)

    expect_equal(length(flow$steps), 2)
    expect_step_type(flow, 1, "exclude")
    expect_step_type(flow, 2, "exclude")
    expect_equal(flow$steps[[1]]$included_label, "Received index test")
    expect_equal(flow$steps[[2]]$included_label, "Received reference standard")
})


## ============================================================================
## SECTION 7: Full Pipeline Chains
## ============================================================================

test_that("complete CONSORT pipeline builds without error", {

    flow <- enroll(n = 1200, label = "Assessed for eligibility") |>
        phase("Enrollment") |>
        exclude("Excluded", n = 300,
                reasons = c("Not meeting criteria" = 160,
                            "Declined" = 90, "Other" = 50)) |>
        phase("Allocation") |>
        allocate(labels = c("Drug A", "Placebo"), n = c(450, 450)) |>
        phase("Follow-up") |>
        exclude("Lost to follow-up", n = c(22, 18), show_count = TRUE) |>
        exclude("Discontinued", n = c(8, 12)) |>
        phase("Analysis") |>
        endpoint("Analysed")

    expect_selecta(flow)
    expect_equal(length(flow$steps), 9)  # 4 phases + 3 excludes + 1 allocate + 1 endpoint
})


test_that("complete STARD pipeline builds without error", {

    flow <- enroll(n = 500, label = "Potentially eligible patients") |>
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
                 n = c(180, 240), label = "Index test result") |>
        endpoint("Final diagnosis",
                 reasons = list(c("Target condition +" = 160, "Target condition -" = 20),
                                c("Target condition +" = 15, "Target condition -" = 225)))

    expect_selecta(flow)
})


test_that("complete data-driven CONSORT pipeline builds without error", {

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

    expect_selecta(flow)
})


test_that("minimal pipeline (enroll + endpoint) builds without error", {

    flow <- enroll(n = 100) |> endpoint("Final")
    expect_selecta(flow)
    expect_equal(length(flow$steps), 1)
})
