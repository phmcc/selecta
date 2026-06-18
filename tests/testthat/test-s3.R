#' Test Suite for S3 Methods
#'
#' Tests covering the S3 methods print.selecta(), summary.selecta(), and
#' plot.selecta(), together with the cohort() and cohorts() data-extraction
#' accessors.
#'
#' @details Run with testthat::test_file("tests/testthat/test-s3.R")

library(testthat)
library(data.table)
library(selecta)

data(selectaex2)
data(selectaex3)


### * Setup: Reusable test flows

### ** Manual 2-arm
flow_m2 <- enroll(n = 1200, label = "Assessed for eligibility") |>
    phase("Enrollment") |>
    exclude("Excluded", n = 300,
            reasons = c("Not meeting criteria" = 160,
                        "Declined" = 90, "Other" = 50)) |>
    phase("Allocation") |>
    allocate(labels = c("Drug A", "Placebo"), n = c(450, 450)) |>
    phase("Follow-up") |>
    exclude("Lost to follow-up", n = c(22, 18), show_count = TRUE) |>
    phase("Analysis") |>
    endpoint("Analyzed")

### ** Data-driven 2-arm
flow_dd2 <- enroll(selectaex2, id = "patient_id") |>
    exclude("Duplicates", criterion = is_duplicate == TRUE,
            included_label = "Unique") |>
    exclude("Failed eligibility", criterion = eligible == FALSE,
            reasons = "exclusion_reason",
            included_label = "Eligible") |>
    allocate("treatment") |>
    exclude("Discontinued", criterion = discontinued == TRUE,
            reasons = "discontinuation_reason") |>
    endpoint("Analysis cohort")

### ** STARD flow with endpoint breakdown
flow_stard <- enroll(n = 500) |>
    assess("Index test", not_received = 22) |>
    stratify(labels = c("Positive", "Negative"), n = c(200, 260),
             label = "Result") |>
    endpoint("Final diagnosis",
             breakdown = list(c("Target +" = 160, "Target -" = 40),
                              c("Target +" = 15, "Target -" = 245)))

### ** Minimal
flow_min <- enroll(n = 100) |> endpoint("Final")

### ** PRISMA
flow_prisma <- sources(PubMed = 1234, Embase = 567) |>
    combine("Records identified") |>
    exclude("Duplicates", n = 340, included_label = "Screened") |>
    endpoint("Included")


### * print.selecta()

test_that("print returns object invisibly", {

    result <- print(flow_m2)
    expect_s3_class(result, "selecta")
})


test_that("print produces output without error", {

    expect_output(print(flow_m2))
})


test_that("print shows mode and starting N", {

    output <- capture.output(print(flow_m2))
    combined <- paste(output, collapse = "\n")

    expect_true(grepl("manual", combined, ignore.case = TRUE))
    expect_true(grepl("1,200|1200", combined))
})


test_that("print shows step types", {

    output <- capture.output(print(flow_m2))
    combined <- paste(output, collapse = "\n")

    expect_true(grepl("exclude", combined, ignore.case = TRUE))
    expect_true(grepl("endpoint", combined, ignore.case = TRUE))
})


test_that("print shows endpoint reasons when present", {

    output <- capture.output(print(flow_stard))
    combined <- paste(output, collapse = "\n")

    expect_true(grepl("Target", combined))
})


test_that("print works for all diagram types", {

    flows <- list(flow_min, flow_m2, flow_dd2, flow_stard, flow_prisma)

    for (i in seq_along(flows)) {
        expect_output(print(flows[[i]]),
                      info = paste("print failed for flow", i))
    }
})


### * summary.selecta()

test_that("summary returns a data.table", {

    result <- summary(flow_m2)

    expect_s3_class(result, "data.table")
})


test_that("summary has required columns", {

    result <- summary(flow_m2)

    expect_true("phase" %in% names(result))
    expect_true("role" %in% names(result))
    expect_true("arm" %in% names(result))
    expect_true("text" %in% names(result))
    expect_true("n" %in% names(result))
})


test_that("summary contains correct roles", {

    result <- summary(flow_m2)
    roles <- unique(result$role)

    expect_true("main" %in% roles)
    expect_true("side" %in% roles)
    expect_true("endpoint" %in% roles)
})


test_that("summary shows per-arm data for multi-arm flows", {

    result <- summary(flow_m2)
    endpoints <- result[role == "endpoint"]

    expect_equal(nrow(endpoints), 2)
    expect_true(all(!is.na(endpoints$arm)))
})


test_that("summary first node has N equal to n_start", {

    result <- summary(flow_m2)
    first_row <- result[1]

    expect_equal(first_row$n, 1200)
})


test_that("summary works for data-driven flows", {

    result <- summary(flow_dd2)

    expect_s3_class(result, "data.table")
    expect_true(result$n[1] == nrow(selectaex2))
})


test_that("summary works for PRISMA flows", {

    result <- summary(flow_prisma)

    expect_s3_class(result, "data.table")
    expect_true(nrow(result) > 0)
})


### * cohort() — Data Extraction

test_that("cohort returns a data.frame for data-driven flows", {

    result <- cohort(flow_dd2)

    expect_true(is.data.frame(result))
    expect_true(nrow(result) > 0)
})


test_that("cohort final dataset has fewer rows than initial", {

    result <- cohort(flow_dd2)

    expect_true(nrow(result) < nrow(selectaex2))
})


test_that("cohort split = TRUE returns list of per-arm data.frames", {

    result <- cohort(flow_dd2, split = TRUE)

    expect_true(is.list(result))
    expect_equal(length(result), 2)  # 2-arm
    for (arm_df in result) {
        expect_true(is.data.frame(arm_df))
        expect_true(nrow(arm_df) > 0)
    }
})


test_that("cohort split data sums to unsplit total", {

    unsplit <- cohort(flow_dd2)
    split   <- cohort(flow_dd2, split = TRUE)

    total_split <- sum(vapply(split, nrow, integer(1)))
    expect_equal(nrow(unsplit), total_split)
})


test_that("cohort arm parameter extracts single arm", {

    split <- cohort(flow_dd2, split = TRUE)
    arm_name <- names(split)[1]

    single <- cohort(flow_dd2, arm = arm_name)

    expect_true(is.data.frame(single))
    expect_equal(nrow(single), nrow(split[[1]]))
})


test_that("cohort preserves original columns", {

    result <- cohort(flow_dd2)
    original_cols <- names(selectaex2)

    for (col in original_cols) {
        expect_true(col %in% names(result),
                    info = paste("Missing column:", col))
    }
})


test_that("cohort works for 3-arm data-driven flow", {

    flow <- enroll(selectaex3, id = "patient_id") |>
        allocate("treatment") |>
        exclude("Discontinued", criterion = discontinued == TRUE) |>
        endpoint("Final")

    split <- cohort(flow, split = TRUE)
    expect_equal(length(split), 3)
})


test_that("cohort errors for manual-mode flows", {

    expect_error(cohort(flow_m2), regexp = "data")
})


### ** cohorts() — Per-Stage Extraction

test_that("cohorts returns a named list keyed by step label", {

    stages <- cohorts(flow_dd2)

    expect_true(is.list(stages))
    expect_true(length(stages) >= 1)
    expect_true("Failed eligibility" %in% names(stages))
})


test_that("cohorts snapshots carry remaining and excluded datasets", {

    stages <- cohorts(flow_dd2)
    snap   <- stages[["Failed eligibility"]]

    expect_true(is.data.frame(snap$included))
    expect_true(is.data.frame(snap$excluded))
    ## A single-stream exclusion records integer tallies too
    expect_equal(snap$n_included, nrow(snap$included))
    expect_equal(snap$n_excluded,  nrow(snap$excluded))
})


test_that("cohorts remaining count is monotonically non-increasing", {

    stages <- cohorts(flow_dd2)
    ## Single-stream snapshots only (per-arm snapshots store lists)
    counts <- vapply(stages, function(s) {
        if (is.data.frame(s$included)) nrow(s$included) else NA_integer_
    }, integer(1L))
    counts <- counts[!is.na(counts)]

    expect_true(all(diff(counts) <= 0))
})


test_that("cohorts final remaining matches cohort()", {

    stages <- cohorts(flow_dd2)
    final_snapshot <- stages[[length(stages)]]

    if (is.data.frame(final_snapshot$included)) {
        expect_equal(nrow(final_snapshot$included), nrow(cohort(flow_dd2)))
    } else {
        ## per-arm final: sum across arms equals split cohort total
        total <- sum(vapply(final_snapshot$included, nrow, integer(1L)))
        expect_equal(total, nrow(cohort(flow_dd2)))
    }
})


### * plot.selecta()

test_that("plot.selecta renders without error", {

    f <- tempfile(fileext = ".pdf")
    on.exit(unlink(f), add = TRUE)

    grDevices::pdf(f)
    expect_no_error(plot(flow_m2))
    grDevices::dev.off()

    expect_true(file.exists(f))
})
