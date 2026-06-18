#' Test Suite for Two-Level Exclusion Reasons
#'
#' Tests covering the reason -> sub-reason breakdown of exclude():
#'   * manual nested reasons (a named vector whose entries may themselves be
#'     named vectors of sub-reasons);
#'   * two-column data-mode reasons (reasons = c("reason", "subreason")) that
#'     are cross-tabulated automatically into the same nested model;
#'   * collapse_singletons, which collapses a parent with a single sub-reason
#'     to a plain leaf.
#' Exercises construction, the compute() graph (the nested structure stored on
#' side nodes, count correctness, and the cross-arm parent ordering), DOT
#' output (bulleted parents, en-dashed sub-reasons), and grid rendering.
#'
#' @details Run with testthat::test_file("tests/testthat/test-subreasons.R")

library(testthat)
library(data.table)
library(selecta)


### * Setup: fixtures and helpers

run_compute <- function(flow) selecta:::compute(flow)

expect_valid_dot <- function(dot_str) {
    expect_type(dot_str, "character")
    expect_length(dot_str, 1L)
    expect_match(dot_str, "^digraph selecta")
    expect_match(dot_str, "\\}\\s*$")
}

temp_path <- function(ext) tempfile(fileext = paste0(".", ext))

### ** Data-mode fixture: a reason column and a sub-reason column.
### "R1" splits into two sub-reasons (a, b); "R2" has a single sub-reason (c),
### which exercises both the nested path and collapse_singletons.
make_subreason_data <- function() {
    dd <- data.frame(
        id        = seq_len(200),
        drop      = c(rep(TRUE, 80), rep(FALSE, 120)),
        reason    = NA_character_,
        subreason = NA_character_,
        stringsAsFactors = FALSE
    )
    dd$reason[1:80]     <- c(rep("R1", 50), rep("R2", 30))
    dd$subreason[1:30]  <- "a"   # R1 -> a (30)
    dd$subreason[31:50] <- "b"   # R1 -> b (20)
    dd$subreason[51:80] <- "c"   # R2 -> c (30), single sub-reason
    dd
}


### * Construction

test_that("exclude stores a manual nested reasons list", {

    flow <- enroll(n = 1000) |>
        exclude("Excluded", n = 100,
                reasons = list("Ineligible" = c("Age" = 40, "Comorbidity" = 35),
                               "Declined"   = 25))

    reas <- flow$steps[[1]]$reasons
    expect_true(is.list(reas))
    expect_equal(names(reas), c("Ineligible", "Declined"))
    expect_equal(reas$Ineligible, c("Age" = 40, "Comorbidity" = 35))
    expect_equal(unname(reas$Declined), 25)
})


test_that("exclude stores two column names for data-mode reasons", {

    dd <- make_subreason_data()
    flow <- enroll(dd, id = "id") |>
        exclude("Excluded", criterion = drop == TRUE,
                reasons = c("reason", "subreason"))

    expect_equal(flow$steps[[1]]$reasons_var, c("reason", "subreason"))
})


test_that("two-column reasons are rejected in manual mode", {

    expect_error(
        enroll(n = 1000) |>
            exclude("Excluded", n = 100, reasons = c("reason", "subreason")),
        regexp = "data mode"
    )
})


test_that("more than two reason columns are rejected", {

    dd <- make_subreason_data()
    expect_error(
        enroll(dd, id = "id") |>
        exclude("Excluded", criterion = drop == TRUE,
                reasons = c("reason", "subreason", "extra")),
        regexp = "at most two columns"
    )
})


test_that("exclude stores the collapse_singletons flag", {

    flow_default <- enroll(n = 100) |> exclude("X", n = 10)
    flow_set     <- enroll(n = 100) |>
        exclude("X", n = 10, collapse_singletons = TRUE)

    expect_false(isTRUE(flow_default$steps[[1]]$collapse_singletons))
    expect_true(flow_set$steps[[1]]$collapse_singletons)
})


### * Compute — manual nested reasons

test_that("compute preserves a manual nested reasons structure", {

    flow <- enroll(n = 1000) |>
        exclude("Excluded", n = 100,
                reasons = list(
                    "Ineligible"     = c("Age" = 40, "Comorbidity" = 35),
                    "Declined"       = 15,
                    "Administrative" = c("Lost to contact" = 10))) |>
        endpoint("Final")

    graph <- run_compute(flow)
    reas  <- graph$nodes[role == "side"]$reasons[[1]]

    expect_true(is.list(reas))
    expect_equal(names(reas), c("Ineligible", "Declined", "Administrative"))

    ## Parent with two sub-reasons: a named vector of length 2 summing to 75
    expect_equal(length(reas$Ineligible), 2L)
    expect_false(is.null(names(reas$Ineligible)))
    expect_equal(sum(reas$Ineligible), 75)

    ## Leaf: a bare scalar with no names
    expect_true(is.null(names(reas$Declined)))
    expect_equal(unname(reas$Declined), 15)

    ## Single-child parent: still a named length-1 vector by default
    expect_false(is.null(names(reas$Administrative)))
    expect_equal(length(reas$Administrative), 1L)
})


### * Compute — two-column data-mode cross-tabulation

test_that("data-mode two-column reasons cross-tabulate into the nested model", {

    dd   <- make_subreason_data()
    flow <- enroll(dd, id = "id", label = "Records") |>
        exclude("Excluded", criterion = drop == TRUE,
                reasons = c("reason", "subreason")) |>
        endpoint("Included")

    graph <- run_compute(flow)
    side  <- graph$nodes[role == "side"]
    reas  <- side$reasons[[1]]

    ## Two parents, ordered by descending total: R1 (50) before R2 (30)
    expect_true(is.list(reas))
    expect_equal(names(reas), c("R1", "R2"))

    ## R1 -> a (30), b (20), ordered by descending count
    expect_equal(reas$R1, c("a" = 30, "b" = 20))

    ## R2 -> c (30), a single sub-reason
    expect_equal(reas$R2, c("c" = 30))

    ## Parents reconcile to the exclusion total for free (data-derived)
    expect_equal(sum(vapply(reas, sum, numeric(1))), 80)
    expect_equal(side$n, 80)
})


test_that("single-column data-mode reasons remain flat", {

    dd   <- make_subreason_data()
    flow <- enroll(dd, id = "id") |>
        exclude("Excluded", criterion = drop == TRUE, reasons = "reason") |>
        endpoint("Included")

    graph <- run_compute(flow)
    reas  <- graph$nodes[role == "side"]$reasons[[1]]

    ## A flat named vector, not a list
    expect_false(is.list(reas))
    expect_equal(sort(names(reas)), c("R1", "R2"))
    expect_equal(sum(reas), 80)
})


### * Compute — collapse_singletons

test_that("collapse_singletons collapses single-child parents to leaves", {

    dd   <- make_subreason_data()
    flow <- enroll(dd, id = "id") |>
        exclude("Excluded", criterion = drop == TRUE,
                reasons = c("reason", "subreason"),
                collapse_singletons = TRUE) |>
        endpoint("Included")

    graph <- run_compute(flow)
    reas  <- graph$nodes[role == "side"]$reasons[[1]]

    ## R2 had one sub-reason -> now a bare leaf carrying R2's count
    expect_true(is.null(names(reas$R2)))
    expect_equal(unname(reas$R2), 30)

    ## R1 had two sub-reasons -> untouched
    expect_false(is.null(names(reas$R1)))
    expect_equal(length(reas$R1), 2L)
})


test_that("collapse_singletons works on a manual nested list too", {

    flow <- enroll(n = 1000) |>
        exclude("Excluded", n = 100,
                reasons = list("Big"  = c("x" = 40, "y" = 30),
                               "Solo" = c("z" = 30)),
                collapse_singletons = TRUE) |>
        endpoint("Final")

    graph <- run_compute(flow)
    reas  <- graph$nodes[role == "side"]$reasons[[1]]

    expect_true(is.null(names(reas$Solo)))      # collapsed
    expect_equal(unname(reas$Solo), 30)
    expect_equal(length(reas$Big), 2L)          # untouched
})


test_that("collapse_singletons defaults to keeping parents expanded", {

    flow <- enroll(n = 1000) |>
        exclude("Excluded", n = 100,
                reasons = list("Big"  = c("x" = 40, "y" = 30),
                               "Solo" = c("z" = 30))) |>
        endpoint("Final")

    graph <- run_compute(flow)
    reas  <- graph$nodes[role == "side"]$reasons[[1]]

    ## Default FALSE: the single-child parent stays nested
    expect_false(is.null(names(reas$Solo)))
    expect_equal(reas$Solo, c("z" = 30))
})


### * Compute — per-arm two-column reasons

test_that("per-arm two-column reasons cross-tabulate per arm with a shared order", {

    ## Each arm drops 40: Adverse event (25: Serious 15, Mild 10),
    ## Withdrew (15: Personal 15). Adverse event outranks Withdrew in both arms.
    da <- data.frame(
        id   = seq_len(240),
        arm  = rep(c("Treatment", "Control"), each = 120),
        drop = rep(c(rep(TRUE, 40), rep(FALSE, 80)), times = 2),
        reason    = NA_character_,
        subreason = NA_character_,
        stringsAsFactors = FALSE
    )
    drop_rows <- which(da$drop)
    da$reason[drop_rows]    <- rep(c("Adverse event", "Withdrew"), c(25, 15))
    da$subreason[drop_rows] <- rep(c("Serious", "Mild", "Personal"),
                                   c(15, 10, 15))

    flow <- enroll(da, id = "id") |>
        allocate(variable = "arm") |>
        exclude("Discontinued", criterion = drop == TRUE,
                reasons = c("reason", "subreason")) |>
        endpoint("Analyzed")

    graph <- run_compute(flow)
    side  <- graph$nodes[role == "side"]
    expect_equal(nrow(side), 2L)

    ## Both arms nested, sharing the same parent order (Adverse event first)
    for (i in seq_len(nrow(side))) {
        reas <- side$reasons[[i]]
        expect_true(is.list(reas))
        expect_equal(names(reas)[1L], "Adverse event")
        expect_equal(length(reas[["Adverse event"]]), 2L)
    }
})


### * DOT output

test_that("DOT renders two-level reasons with bulleted parents and en-dashed subs", {

    flow <- enroll(n = 1000) |>
        exclude("Excluded", n = 100,
                reasons = list(
                    "Did not meet criteria" = c("Age outside range" = 40,
                                                "Comorbidity"       = 35),
                    "Declined"              = 15,
                    "Administrative"        = c("Lost to contact" = 10))) |>
        endpoint("Final")

    dot <- flowchart(flow, engine = "dot")
    expect_valid_dot(dot)

    ## Parent and sub-reason labels both appear
    expect_match(dot, "Did not meet criteria")
    expect_match(dot, "Age outside range")

    ## Plain DOT marks parents with a bullet and sub-reasons with an en-dash
    expect_match(dot, "\u2022")   # bullet (parent)
    expect_match(dot, "\u2013")   # en-dash (sub-reason)
})


test_that("DOT renders a data-mode two-column breakdown", {

    dd   <- make_subreason_data()
    flow <- enroll(dd, id = "id", label = "Records") |>
        exclude("Excluded", criterion = drop == TRUE,
                reasons = c("reason", "subreason")) |>
        endpoint("Included")

    dot <- flowchart(flow, engine = "dot")
    expect_valid_dot(dot)
    expect_match(dot, "R1")
    expect_match(dot, "\u2013")   # sub-reasons present
})


### * Grid rendering

test_that("two-level reasons render to file (manual and data mode)", {

    flow_manual <- enroll(n = 1000) |>
        exclude("Excluded", n = 100,
                reasons = list("Ineligible" = c("Age" = 40, "Comorbidity" = 35),
                               "Declined"   = 25)) |>
        endpoint("Final")

    dd <- make_subreason_data()
    flow_data <- enroll(dd, id = "id") |>
        exclude("Excluded", criterion = drop == TRUE,
                reasons = c("reason", "subreason")) |>
        endpoint("Included")

    for (fl in list(flow_manual, flow_data)) {
        f <- temp_path("pdf")
        on.exit(unlink(f), add = TRUE)
        expect_no_error(flowsave(fl, f))
        expect_true(file.exists(f))
    }
})


test_that("collapse_singletons renders to file", {

    dd   <- make_subreason_data()
    flow <- enroll(dd, id = "id") |>
        exclude("Excluded", criterion = drop == TRUE,
                reasons = c("reason", "subreason"),
                collapse_singletons = TRUE) |>
        endpoint("Included")

    f <- temp_path("pdf")
    on.exit(unlink(f), add = TRUE)
    expect_no_error(flowsave(flow, f))
    expect_true(file.exists(f))
})
