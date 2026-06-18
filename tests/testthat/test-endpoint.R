#' Test Suite for Split Endpoints
#'
#' Tests covering endpoint(groups = , n = ) (manual) and endpoint(variable = )
#' (data mode): the terminal split in which the endpoint fans from a shared
#' distributor into one separate box per study group. Exercises construction
#' (input validation and the construction-time guard against splitting after an
#' uncombined arm split), the compute() graph (distributor + group boxes +
#' split edges), DOT output, and grid rendering. The within-box endpoint
#' breakdown (endpoint(breakdown = )) is a distinct, mutually exclusive feature
#' covered in test-pipeline.R and test-subreasons.R.
#'
#' @details Run with testthat::test_file("tests/testthat/test-endpoint-groups.R")

library(testthat)
library(data.table)
library(selecta)


### * Setup: fixtures and helpers

### ** Data-mode fixture: a balanced three-level grouping column
df_grp <- data.frame(
    id  = seq_len(300),
    grp = rep(c("Group A", "Group B", "Group C"), each = 100),
    stringsAsFactors = FALSE
)

run_compute <- function(flow) selecta:::compute(flow)

expect_valid_dot <- function(dot_str) {
    expect_type(dot_str, "character")
    expect_length(dot_str, 1L)
    expect_match(dot_str, "^digraph selecta")
    expect_match(dot_str, "\\}\\s*$")
}

temp_path <- function(ext) tempfile(fileext = paste0(".", ext))


### * Construction — manual mode (groups + n)

test_that("endpoint stores groups and n for a manual split", {

    flow <- enroll(n = 300, label = "Eligible cohort") |>
        endpoint("Allocated to study group",
                 groups = c("Group A", "Group B", "Group C"),
                 n = c(100, 100, 100))

    step <- flow$steps[[length(flow$steps)]]
    expect_equal(step$type, "endpoint")
    expect_equal(step$groups, c("Group A", "Group B", "Group C"))
    expect_equal(step$n, c(100, 100, 100))
    expect_null(step$reasons)   # breakdown not used
})


test_that("endpoint stores variable for a data-mode split", {

    flow <- enroll(df_grp, id = "id", label = "Eligible cohort") |>
        endpoint("Allocated to study group", variable = "grp")

    step <- flow$steps[[length(flow$steps)]]
    expect_equal(step$type, "endpoint")
    expect_equal(step$variable, "grp")
})


test_that("groups and breakdown are mutually exclusive", {

    expect_error(
        enroll(n = 300) |>
            endpoint("X", groups = c("A", "B"), n = c(150, 150),
                     breakdown = c("p" = 100, "q" = 200)),
        regexp = "not both"
    )
})


test_that("manual split requires n, parallel to groups", {

    ## Missing n
    expect_error(
        enroll(n = 300) |> endpoint("X", groups = c("A", "B")),
        regexp = "n"
    )
    ## Length mismatch
    expect_error(
        enroll(n = 300) |> endpoint("X", groups = c("A", "B"), n = 300),
        regexp = "one count per group"
    )
})


test_that("data-mode split requires variable, not groups/n", {

    expect_error(
        enroll(df_grp, id = "id") |>
            endpoint("X", groups = c("A", "B"), n = c(150, 150)),
        regexp = "variable"
    )
})


### * Construction — guard against splitting after an uncombined split

test_that("split endpoint after an uncombined allocate errors at construction", {

    ## The error must be raised while building the pipeline (not deferred to
    ## rendering), because a per-arm group split is a nested split and out of
    ## scope.
    expect_error(
        enroll(n = 200) |>
            allocate(labels = c("A", "B"), n = c(100, 100)) |>
            endpoint("Outcome", groups = c("X", "Y"), n = c(50, 50)),
        regexp = "single incoming stream"
    )
})


test_that("split endpoint after a recombined split is allowed", {

    ## Once combine() has recombined the arms there is a single stream again,
    ## so a terminal split is valid.
    expect_no_error(
        enroll(n = 200) |>
            allocate(labels = c("A", "B"), n = c(100, 100)) |>
            combine("Pooled") |>
            endpoint("Outcome", groups = c("X", "Y"), n = c(120, 80))
    )
})


### * Compute graph — manual split

test_that("manual split endpoint builds distributor + group boxes + split edges", {

    flow <- enroll(n = 300, label = "Eligible cohort") |>
        endpoint("Allocated", groups = c("A", "B", "C"), n = c(100, 100, 100))

    graph <- run_compute(flow)

    ## A single centered distributor (alloc role) carrying the endpoint label
    distrib <- graph$nodes[role == "alloc"]
    expect_equal(nrow(distrib), 1L)
    expect_equal(distrib$text, "Allocated")
    expect_equal(distrib$n, 300)
    expect_true(is.na(distrib$arm_id))

    ## One terminal endpoint box per group, columned by arm_id
    grp <- graph$nodes[role == "endpoint"]
    expect_equal(nrow(grp), 3L)
    expect_equal(sort(grp$arm_id), c(1L, 2L, 3L))
    expect_equal(sort(grp$n), c(100, 100, 100))

    ## The distributor fans into the groups via split edges
    splits <- graph$edges[edge_type == "split"]
    expect_equal(nrow(splits), 3L)
    expect_true(all(splits$from == distrib$node_id))
})


test_that("manual split group counts and total are preserved", {

    flow <- enroll(n = 240, label = "Randomized") |>
        endpoint("Study arm",
                 groups = c("Intervention", "Usual care"),
                 n = c(130, 110))

    graph   <- run_compute(flow)
    distrib <- graph$nodes[role == "alloc"]
    grp     <- graph$nodes[role == "endpoint"]

    expect_equal(distrib$n, 240)
    expect_equal(sum(grp$n), 240)
    expect_equal(sort(grp$n), c(110, 130))
})


### * Compute graph — data-mode split

test_that("data-mode split endpoint tabulates group counts from the column", {

    flow <- enroll(df_grp, id = "id", label = "Eligible cohort") |>
        endpoint("Allocated", variable = "grp")

    graph <- run_compute(flow)

    grp <- graph$nodes[role == "endpoint"]
    expect_equal(nrow(grp), 3L)
    expect_equal(sort(grp$n), c(100, 100, 100))
    expect_setequal(grp$text, c("Group A", "Group B", "Group C"))

    ## Distributor count equals the cohort reaching it
    distrib <- graph$nodes[role == "alloc"]
    expect_equal(distrib$n, 300)

    splits <- graph$edges[edge_type == "split"]
    expect_equal(nrow(splits), 3L)
})


test_that("data-mode split reflects upstream exclusions", {

    ## Exclude half before splitting; the split must partition the survivors.
    dd <- data.frame(id = 1:300,
                     drop = rep(c(TRUE, FALSE), c(60, 240)),
                     grp  = rep(c("A", "B", "C"), times = 100),
                     stringsAsFactors = FALSE)

    flow <- enroll(dd, id = "id") |>
        exclude("Excluded", criterion = drop == TRUE) |>
        endpoint("Allocated", variable = "grp")

    graph <- run_compute(flow)
    grp   <- graph$nodes[role == "endpoint"]

    expect_equal(sum(grp$n), 240)   # 300 - 60
})


### * DOT output

test_that("manual split endpoint produces valid DOT with all group labels", {

    flow <- enroll(n = 300, label = "Eligible cohort") |>
        endpoint("Allocated to study group",
                 groups = c("Group A", "Group B", "Group C"),
                 n = c(100, 100, 100))

    dot <- flowchart(flow, engine = "dot")
    expect_valid_dot(dot)
    expect_match(dot, "Allocated to study group")
    expect_match(dot, "Group A")
    expect_match(dot, "Group B")
    expect_match(dot, "Group C")
})


test_that("even (two-group) split produces valid DOT", {

    flow <- enroll(n = 240, label = "Randomized cohort") |>
        endpoint("Study arm",
                 groups = c("Intervention", "Usual care"),
                 n = c(120, 120))

    dot <- flowchart(flow, engine = "dot")
    expect_valid_dot(dot)
    expect_match(dot, "Intervention")
    expect_match(dot, "Usual care")
})


test_that("data-mode split produces valid DOT", {

    flow <- enroll(df_grp, id = "id", label = "Eligible cohort") |>
        endpoint("Allocated", variable = "grp")

    dot <- flowchart(flow, engine = "dot")
    expect_valid_dot(dot)
    expect_match(dot, "Group A")
})


### * Grid rendering

test_that("manual split endpoints render to file (odd and even)", {

    flow_odd <- enroll(n = 300, label = "Eligible cohort") |>
        endpoint("Allocated", groups = c("A", "B", "C"), n = c(100, 100, 100))
    flow_even <- enroll(n = 240, label = "Randomized") |>
        endpoint("Study arm", groups = c("Intervention", "Usual care"),
                 n = c(120, 120))

    for (fl in list(flow_odd, flow_even)) {
        f <- temp_path("pdf")
        on.exit(unlink(f), add = TRUE)
        expect_no_error(flowsave(fl, f))
        expect_true(file.exists(f))
    }
})


test_that("split endpoint within a phased flow renders to file", {

    flow <- enroll(n = 400, label = "Patients assessed") |>
        phase("Screening") |>
        exclude("Excluded", n = 100) |>
        phase("Group assignment") |>
        endpoint("Assigned to study group",
                 groups = c("Group A", "Group B"), n = c(150, 150))

    f <- temp_path("pdf")
    on.exit(unlink(f), add = TRUE)
    expect_no_error(flowsave(flow, f))
    expect_true(file.exists(f))
})


test_that("data-mode split endpoint renders to file", {

    flow <- enroll(df_grp, id = "id", label = "Eligible cohort") |>
        endpoint("Allocated", variable = "grp")

    f <- temp_path("pdf")
    on.exit(unlink(f), add = TRUE)
    expect_no_error(flowsave(flow, f))
    expect_true(file.exists(f))
})
