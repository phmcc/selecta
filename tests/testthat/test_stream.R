#' Test Suite for Stream Functions
#'
#' Tests covering sources(), combine(), and classify() — the multi-source
#' entry model used for PRISMA, MOOSE, and similar systematic review diagrams.
#'
#' @details Run with testthat::test_file("tests/testthat/test-stream.R")

library(testthat)
library(data.table)
library(selecta)


## ============================================================================
## SECTION 1: sources() — Multi-Source Entry
## ============================================================================

test_that("sources creates valid selecta object with flat sources", {

    flow <- sources(PubMed = 1234, Embase = 567, CENTRAL = 89)

    expect_s3_class(flow, "selecta")
    expect_equal(flow$mode, "manual")
    expect_equal(flow$n_start, 1234 + 567 + 89)
    expect_equal(flow$steps[[1]]$type, "sources")
})


test_that("sources creates grouped sources with headers", {

    flow <- sources(
        databases = c("PubMed" = 1234, "Embase" = 567),
        other     = c("Citation search" = 55, "Websites" = 34),
        headers   = c(databases = "Databases and registers",
                      other     = "Other methods")
    )

    expect_s3_class(flow, "selecta")
    groups <- flow$steps[[1]]$groups
    expect_equal(length(groups), 2)
    expect_equal(groups[[1]]$header, "Databases and registers")
    expect_equal(groups[[2]]$header, "Other methods")
})


test_that("sources supports 3-column layout (previous + databases + other)", {

    flow <- sources(
        previous  = c("Previous review" = 12, "Previous reports" = 15),
        databases = c("PubMed" = 1234, "Embase" = 567, "CENTRAL" = 89),
        other     = c("Citation search" = 55, "Websites" = 34),
        headers   = c(previous  = "Previous studies",
                      databases = "Databases and registers",
                      other     = "Other methods")
    )

    groups <- flow$steps[[1]]$groups
    expect_equal(length(groups), 3)
    expect_equal(flow$n_start, 12 + 15 + 1234 + 567 + 89 + 55 + 34)
})


test_that("sources errors when flat arguments are unnamed", {

    expect_error(sources(1234, 567), regexp = "named")
})


test_that("sources computes correct total N", {

    flow <- sources(
        databases = c("PubMed" = 100, "Embase" = 200),
        other     = c("Manual" = 50),
        headers   = c(databases = "DBs", other = "Other")
    )

    expect_equal(flow$n_start, 350)
})


## ============================================================================
## SECTION 2: combine() — Stream Convergence
## ============================================================================

test_that("combine adds merge step after sources", {

    flow <- sources(PubMed = 1234, Embase = 567) |>
        combine("Records identified")

    expect_s3_class(flow, "selecta")

    combine_step <- flow$steps[[2]]
    expect_equal(combine_step$type, "combine")
    expect_equal(combine_step$label, "Records identified")
})


test_that("combine accepts optional manual n", {

    flow <- sources(PubMed = 1234, Embase = 567) |>
        combine("Records identified", n = 1800)

    expect_equal(flow$steps[[2]]$n, 1800)
})


test_that("combine without n defaults to NULL (auto-computed)", {

    flow <- sources(PubMed = 1234, Embase = 567) |>
        combine("Records identified")

    expect_null(flow$steps[[2]]$n)
})


## ============================================================================
## SECTION 3: classify() — Cross-Classification Grids
## ============================================================================

test_that("classify creates grid step", {

    flow <- enroll(n = 500) |>
        stratify(labels = c("Positive", "Negative"), n = c(200, 300),
                 label = "Result") |>
        classify(rows = c("Target +", "Target -"),
                 cols = c("Positive", "Negative"),
                 n = matrix(c(160, 40, 25, 275), nrow = 2, byrow = TRUE),
                 label = "Classification")

    step <- flow$steps[[length(flow$steps)]]
    expect_equal(step$type, "classify")
    expect_equal(step$rows, c("Target +", "Target -"))
    expect_equal(step$cols, c("Positive", "Negative"))
    expect_true(is.matrix(step$n))
})


## ============================================================================
## SECTION 4: Full PRISMA Pipeline Chains
## ============================================================================

test_that("complete 3-column PRISMA pipeline builds without error", {

    flow <- sources(
        previous  = c("Previous review" = 12, "Previous reports" = 15),
        databases = c("PubMed" = 1234, "Embase" = 567, "CENTRAL" = 89),
        other     = c("Citation search" = 55, "Websites" = 34),
        headers   = c(previous  = "Previous studies",
                      databases = "Databases and registers",
                      other     = "Other methods")) |>
        phase("Identification") |>
        combine("Records identified", n = 2006) |>
        exclude("Records removed before screening", n = 352,
                reasons = c("Duplicates" = 340, "Automated" = 12),
                included_label = "Records screened") |>
        phase("Screening") |>
        exclude("Records excluded", n = 1100) |>
        exclude("Reports not retrieved", n = 15) |>
        exclude("Full-text excluded", n = 45,
                reasons = c("Wrong population" = 20,
                            "Wrong outcome" = 15,
                            "Wrong design" = 10)) |>
        phase("Included") |>
        endpoint("Studies included in review")

    expect_s3_class(flow, "selecta")
})


test_that("2-column PRISMA pipeline builds without error", {

    flow <- sources(
        databases = c("PubMed" = 1234, "Embase" = 567),
        other     = c("Citation search" = 55),
        headers   = c(databases = "Databases", other = "Other")) |>
        combine("Records identified") |>
        exclude("Duplicates", n = 340, included_label = "Records screened") |>
        exclude("Excluded", n = 800) |>
        endpoint("Studies included")

    expect_s3_class(flow, "selecta")
})


test_that("flat PRISMA pipeline (1-column) builds without error", {

    flow <- sources(PubMed = 1234, Embase = 567, CENTRAL = 89) |>
        combine("Records identified") |>
        exclude("Duplicates", n = 340, included_label = "Screened") |>
        exclude("Excluded", n = 800) |>
        endpoint("Included")

    expect_s3_class(flow, "selecta")
})


test_that("MOOSE gray-literature pipeline builds without error", {

    flow <- sources(
        databases = c("MEDLINE" = 2500, "Embase" = 1800),
        gray      = c("Dissertations" = 45, "Conference" = 120),
        headers   = c(databases = "Electronic databases",
                      gray      = "Gray literature")) |>
        combine("Total records") |>
        exclude("Duplicates", n = 1200, included_label = "Unique records") |>
        exclude("Excluded on title/abstract", n = 2800) |>
        exclude("Excluded on full text", n = 150,
                reasons = c("No control" = 60, "Insufficient data" = 50,
                            "Wrong population" = 40)) |>
        endpoint("Studies in meta-analysis")

    expect_s3_class(flow, "selecta")
})
