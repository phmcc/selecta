#' Test Suite for Factorial (Two-Level) Stratification
#'
#' Tests covering factorial stratify()/allocate() splits used for trials with
#' double randomization: a first split into arms, then a second split of EACH
#' arm into sub-arms (branching directly, with no second allocation box). The
#' suite exercises both count sources -- manual (labels + n) and data-driven
#' (counts derived from the data) -- across the builder depth guard, the
#' compute() graph (stable globally-unique arm ids, arm_parent / arm_level
#' fields, per-parent split edges, parent-major counts), per-leaf exclusions,
#' endpoints, and assess(), the factorial combine() that peels one level, a
#' second combine() that pools to a single cohort, cohort()/cohorts()
#' extraction of factorial cells, edge cases (shared / mis-sized n vectors,
#' absent cells), the DOT emission (per-parent sub-distributors plus the
#' cross-parent bar-link that fixes sub-arm ordering), and the grid-engine
#' factorial layout. Here "factorial" denotes the double-split topology; the
#' separate "nested reasons" feature is covered in test-subreasons.R.
#'
#' @details Run with testthat::test_file("tests/testthat/test-factorial.R")

library(testthat)
library(data.table)
library(selecta)

run_compute <- function(flow) selecta:::compute(flow)
run_layout  <- function(flow) selecta:::layout_nodes(selecta:::compute(flow))

## A canonical 2 x 2 factorial flow (mirrors the consort reference figure).
factorial_flow <- function() {
    enroll(n = 169, label = "Randomized patient") |>
        allocate(labels = c("Conc", "Seq"), n = c(86, 83),
                 label = "Randomized") |>
        allocate(labels = c("Trt A", "Trt B"), n = c(42, 44, 44, 39)) |>
        exclude("Not dosed", n = c(2, 1, 7, 2)) |>
        endpoint("Final miTT Analysis")
}

## A small data-mode 2 x 2 fixture with an exactly known cross-tabulation, so
## every derived count is hand-checkable. arm {A, B} x sub {x, y}; a per-row
## drop flag feeds a per-leaf exclusion.
##   A-x = 5 (1 dropped), A-y = 3 (0), B-x = 4 (2 dropped), B-y = 6 (1 dropped)
factorial_data <- function() {
    data.table(
        id   = seq_len(18L),
        arm  = rep(c("A", "B"), c(8L, 10L)),
        sub  = c(rep("x", 5L), rep("y", 3L), rep("x", 4L), rep("y", 6L)),
        drop = c(TRUE, rep(FALSE, 4L),       # A-x: rows 1-5
                 rep(FALSE, 3L),             # A-y: rows 6-8
                 TRUE, TRUE, rep(FALSE, 2L), # B-x: rows 9-12
                 TRUE, rep(FALSE, 5L)))      # B-y: rows 13-18
}


### * Builder depth guard

test_that("two factorial split levels are allowed", {
    expect_silent(
        enroll(n = 100) |>
            allocate(labels = c("A", "B"), n = c(50, 50)) |>
            allocate(labels = c("x", "y"), n = c(25, 25, 25, 25))
    )
})

test_that("a third factorial split level is refused", {
    expect_error(
        enroll(n = 100) |>
            allocate(labels = c("A", "B"), n = c(50, 50)) |>
            allocate(labels = c("x", "y"), n = c(25, 25, 25, 25)) |>
            allocate(labels = c("p", "q"), n = rep(12.5, 8)),
        "two factorial"
    )
})

test_that("combine() peels a level, re-permitting a split", {
    ## split -> split -> combine (back to level 1) -> split again is allowed.
    expect_silent(
        enroll(n = 100) |>
            allocate(labels = c("A", "B"), n = c(50, 50)) |>
            allocate(labels = c("x", "y"), n = c(25, 25, 25, 25)) |>
            combine("Merged") |>
            allocate(labels = c("p", "q"), n = c(25, 25, 25, 25))
    )
})

test_that("a factorial split accepts a single shared sub-arm n (recycled per parent)", {
    ## n of length n_sub is shared across every parent.
    flow <- enroll(n = 100) |>
        allocate(labels = c("A", "B"), n = c(50, 50)) |>
        allocate(labels = c("x", "y"), n = c(25, 25)) |>
        endpoint("Analyzed")
    leaves <- run_compute(flow)$nodes[role == "arm" & arm_level == 2L][order(arm_id)]
    expect_equal(leaves$n, c(25, 25, 25, 25))
})

test_that("a factorial split rejects an n that is not a multiple of the sub-arm count", {
    expect_error(
        enroll(n = 100) |>
            allocate(labels = c("A", "B"), n = c(50, 50)) |>
            allocate(labels = c("x", "y", "z"), n = c(10, 10, 10, 10, 10)),
        "multiple"
    )
})


### * Compute structure (manual)

test_that("factorial split assigns unique arm ids with parent/level fields", {
    graph <- run_compute(factorial_flow())
    nodes <- graph$nodes

    ## First-level arms: level 1, no parent.
    lvl1 <- nodes[role == "arm" & arm_level == 1L][order(arm_id)]
    expect_equal(lvl1$text, c("Conc", "Seq"))
    expect_true(all(is.na(lvl1$arm_parent)))

    ## Leaf arms: level 2, each pointing at a first-level parent.
    leaves <- nodes[role == "arm" & arm_level == 2L][order(arm_id)]
    expect_equal(nrow(leaves), 4L)
    expect_equal(leaves$text, c("Trt A", "Trt B", "Trt A", "Trt B"))

    ## arm ids are globally unique across both levels.
    arm_nodes <- nodes[role == "arm"]
    expect_equal(length(unique(arm_nodes$arm_id)), nrow(arm_nodes))

    ## Leaves are grouped parent-major: the first two share one parent, the
    ## last two share the other, and the parents are the two level-1 arms.
    expect_equal(leaves$arm_parent[1], leaves$arm_parent[2])
    expect_equal(leaves$arm_parent[3], leaves$arm_parent[4])
    expect_false(leaves$arm_parent[1] == leaves$arm_parent[3])
    expect_setequal(unique(leaves$arm_parent), lvl1$arm_id)
})

test_that("factorial split adds no second allocation box", {
    graph <- run_compute(factorial_flow())
    ## Only the first split contributes an alloc box.
    expect_equal(nrow(graph$nodes[role == "alloc"]), 1L)
    expect_equal(graph$nodes[role == "alloc"]$text, "Randomized")
})

test_that("each parent arm fans split edges to its own sub-arms", {
    graph <- run_compute(factorial_flow())
    nodes <- graph$nodes
    edges <- graph$edges

    alloc_id <- nodes[role == "alloc"]$node_id
    lvl1_ids <- nodes[role == "arm" & arm_level == 1L]$node_id

    splits <- edges[edge_type == "split"]
    ## Two split edges from the alloc box (to the first-level arms) ...
    expect_equal(sum(splits$from == alloc_id), 2L)
    ## ... and two from each first-level arm (to its leaf sub-arms).
    for (pid in lvl1_ids)
        expect_equal(sum(splits$from == pid), 2L)
})

test_that("factorial counts are parent-major", {
    graph <- run_compute(factorial_flow())
    leaves <- graph$nodes[role == "arm" & arm_level == 2L][order(arm_id)]
    ## Conc's pair: 42, 44; Seq's pair: 44, 39.
    expect_equal(leaves$n, c(42, 44, 44, 39))
})


### * Per-leaf exclusions and endpoints (manual)

test_that("exclusions and endpoints attach per leaf arm", {
    graph <- run_compute(factorial_flow())
    nodes <- graph$nodes

    ## One "Not dosed" side box per leaf, each tagged with a leaf arm id.
    side <- nodes[role == "side" & text %like% "Not dosed"]
    expect_equal(nrow(side), 4L)
    expect_true(all(side$arm_level == 2L))

    ## One endpoint per leaf, post-exclusion counts.
    ends <- nodes[role == "endpoint"][order(arm_id)]
    expect_equal(nrow(ends), 4L)
    expect_equal(ends$n, c(40, 43, 37, 37))
    expect_true(all(ends$arm_level == 2L))
})


### * Factorial combine (peel one level) -- manual

test_that("factorial combine merges sub-arms per parent and keeps one level", {
    flow <- enroll(n = 169, label = "Randomized") |>
        allocate(labels = c("Conc", "Seq"), n = c(86, 83)) |>
        allocate(labels = c("Trt A", "Trt B"), n = c(42, 44, 44, 39)) |>
        combine("Per-stratum cohort") |>
        endpoint("Analysis")

    graph <- run_compute(flow)
    nodes <- graph$nodes
    edges <- graph$edges

    ## One merge box per parent (two), each carrying the combine label.
    merges <- nodes[text == "Per-stratum cohort"]
    expect_equal(nrow(merges), 2L)

    ## Four converge edges (two leaves into each of the two parent merges).
    expect_equal(nrow(edges[edge_type == "converge"]), 4L)

    ## After peeling one level the merged streams are first-level again, so a
    ## single endpoint sits on each (one per restored arm).
    expect_equal(nrow(nodes[role == "endpoint"]), 2L)
})


### * Data-mode factorial (counts derived from the data)

test_that("data-mode factorial derives parent-major leaf counts from the data", {
    flow <- enroll(factorial_data(), id = "id", label = "Enrolled") |>
        allocate(variable = "arm", label = "Arm assignment") |>
        allocate(variable = "sub") |>
        endpoint("Analyzed")
    nodes <- run_compute(flow)$nodes

    lvl1 <- nodes[role == "arm" & arm_level == 1L][order(arm_id)]
    expect_equal(lvl1$text, c("A", "B"))

    leaves <- nodes[role == "arm" & arm_level == 2L][order(arm_id)]
    expect_equal(nrow(leaves), 4L)
    ## Sub-arm labels are the second variable's levels (sorted), shared by both
    ## parents; counts are the data cross-tabulation, parent-major.
    expect_equal(leaves$text, c("x", "y", "x", "y"))
    expect_equal(leaves$n, c(5, 3, 4, 6))
    expect_setequal(unique(leaves$arm_parent), lvl1$arm_id)
})

test_that("data-mode factorial matches manual structure (ids, parent/level, no 2nd alloc box)", {
    nodes <- run_compute(
        enroll(factorial_data(), id = "id") |>
            allocate(variable = "arm") |>
            allocate(variable = "sub") |>
            endpoint("Analyzed"))$nodes
    arm_nodes <- nodes[role == "arm"]
    expect_equal(length(unique(arm_nodes$arm_id)), nrow(arm_nodes))
    expect_equal(nrow(nodes[role == "alloc"]), 1L)
})

test_that("data-mode per-leaf exclusion filters each cell's own rows", {
    flow <- enroll(factorial_data(), id = "id", label = "Enrolled") |>
        allocate(variable = "arm", label = "Arm assignment") |>
        allocate(variable = "sub") |>
        exclude("Dropped", criterion = drop == TRUE) |>
        endpoint("Analyzed")
    nodes <- run_compute(flow)$nodes

    side <- nodes[role == "side" & text %like% "Dropped"]
    expect_equal(nrow(side), 4L)

    ## Post-exclusion endpoints: A-x 5-1=4, A-y 3-0=3, B-x 4-2=2, B-y 6-1=5.
    ends <- nodes[role == "endpoint"][order(arm_id)]
    expect_equal(ends$n, c(4, 3, 2, 5))
    expect_true(all(ends$arm_level == 2L))
})

test_that("data-mode factorial relabels sub-arms through 'labels'", {
    flow <- enroll(factorial_data(), id = "id") |>
        allocate(variable = "arm") |>
        allocate(variable = "sub", labels = c(x = "Sub X", y = "Sub Y")) |>
        endpoint("Analyzed")
    leaves <- run_compute(flow)$nodes[role == "arm" & arm_level == 2L][order(arm_id)]
    expect_equal(leaves$text, c("Sub X", "Sub Y", "Sub X", "Sub Y"))
    ## Relabeling changes only the text, not the derived counts.
    expect_equal(leaves$n, c(5, 3, 4, 6))
})

test_that("data-mode factorial fills an absent cell as an empty (n = 0) sub-arm", {
    ## arm A has only sub 'x'; arm B has both. The shared level set keeps the
    ## grid rectangular, so A's missing 'y' appears as an n = 0 sub-arm.
    dt <- data.table(
        id  = seq_len(9L),
        arm = c("A", "A", "A", "B", "B", "B", "B", "B", "B"),
        sub = c("x", "x", "x", "x", "x", "y", "y", "y", "y"))
    flow <- enroll(dt, id = "id") |>
        allocate(variable = "arm") |>
        allocate(variable = "sub") |>
        endpoint("Analyzed")
    leaves <- run_compute(flow)$nodes[role == "arm" & arm_level == 2L][order(arm_id)]
    expect_equal(nrow(leaves), 4L)
    expect_equal(leaves$text, c("x", "y", "x", "y"))
    expect_equal(leaves$n, c(3, 0, 2, 4))   # A-y is the empty cell
})

test_that("manual and data-mode factorial produce the same arm tree", {
    g_data <- run_compute(
        enroll(factorial_data(), id = "id", label = "Enrolled") |>
            allocate(variable = "arm", label = "Arm assignment") |>
            allocate(variable = "sub") |>
            endpoint("Analyzed"))
    g_manual <- run_compute(
        enroll(n = 18, label = "Enrolled") |>
            allocate(labels = c("A", "B"), n = c(8, 10), label = "Arm assignment") |>
            allocate(labels = c("x", "y"), n = c(5, 3, 4, 6)) |>
            endpoint("Analyzed"))
    sel <- function(g)
        g$nodes[role == "arm"][order(arm_id),
                               .(text, n = as.numeric(n), arm_level, arm_parent)]
    expect_equal(sel(g_data), sel(g_manual))
})


### * assess() on a factorial

test_that("assess() attaches an inverted-label box per factorial leaf", {
    ## assess() builds an exclude step with "Did not receive" / "Received"
    ## labels, so it must route through the same per-leaf path as exclude().
    flow <- enroll(n = 100, label = "Randomized") |>
        allocate(labels = c("A", "B"), n = c(50, 50)) |>
        allocate(labels = c("x", "y"), n = c(25, 25, 25, 25)) |>
        assess("study drug", not_received = c(3, 2, 4, 1)) |>
        endpoint("Analyzed")
    nodes <- run_compute(flow)$nodes

    side <- nodes[role == "side" & text %like% "Did not receive"]
    expect_equal(nrow(side), 4L)
    ## Endpoints carry the "Received" counts: 25 minus not_received per leaf.
    ends <- nodes[role == "endpoint"][order(arm_id)]
    expect_equal(ends$n, c(22, 23, 21, 24))
})


### * Data-mode factorial combine and double recombine

test_that("data-mode factorial combine recombines each parent's cells exactly", {
    flow <- enroll(factorial_data(), id = "id", label = "Enrolled") |>
        allocate(variable = "arm", label = "Arm assignment") |>
        allocate(variable = "sub") |>
        combine("Per-arm cohort") |>
        endpoint("Analyzed")
    nodes <- run_compute(flow)$nodes

    ## One merge box per parent, each carrying the combine label.
    merges <- nodes[text == "Per-arm cohort"]
    expect_equal(nrow(merges), 2L)
    ## Merged counts equal each parent's row total: A = 8, B = 10.
    expect_equal(sort(merges$n), c(8, 10))
    ## Peeled back to one level: one endpoint per restored arm.
    expect_equal(nrow(nodes[role == "endpoint"]), 2L)
})

test_that("data-mode factorial then full combine pools to a single cohort", {
    flow <- enroll(factorial_data(), id = "id", label = "Enrolled") |>
        allocate(variable = "arm", label = "Arm assignment") |>
        allocate(variable = "sub") |>
        combine("Per-arm cohort") |>      # peel inner level -> two arms
        combine("Pooled cohort") |>       # full recombine -> single trunk
        endpoint("Analyzed")
    nodes <- run_compute(flow)$nodes

    ## The second combine collapses to a single trunk box and one endpoint.
    expect_equal(nrow(nodes[text == "Pooled cohort"]), 1L)
    expect_equal(nrow(nodes[role == "endpoint"]), 1L)
    expect_equal(nodes[text == "Pooled cohort"]$n, 18)
})


### * Factorial cohort extraction (cohorts / cohort)

test_that("cohorts() keys factorial cells as 'parent: sub'", {
    stages <- cohorts(
        enroll(factorial_data(), id = "id", label = "Enrolled") |>
            allocate(variable = "arm", label = "Arm assignment") |>
            allocate(variable = "sub") |>
        exclude("Dropped", criterion = drop == TRUE) |>
        endpoint("Analyzed"))

    ## Level-1 split snapshot keyed by arm label.
    expect_setequal(names(stages[["_arm"]]$n_included), c("A", "B"))
    ## Level-2 (factorial) snapshot keyed "parent: sub".
    expect_setequal(names(stages[["_arm2"]]$n_included),
                    c("A: x", "A: y", "B: x", "B: y"))
    expect_equal(stages[["_arm2"]]$n_included[["A: x"]], 5L)
    expect_equal(stages[["_arm2"]]$n_included[["B: y"]], 6L)
})

test_that("cohort() returns per-cell factorial data, splittable and poolable", {
    flow <- enroll(factorial_data(), id = "id", label = "Enrolled") |>
        allocate(variable = "arm", label = "Arm assignment") |>
        allocate(variable = "sub") |>
        exclude("Dropped", criterion = drop == TRUE) |>
        endpoint("Analyzed")

    ## A single cell, post-exclusion.
    ax <- cohort(flow, arm = "A: x")
    expect_s3_class(ax, "data.table")
    expect_equal(nrow(ax), 4L)                 # 5 enrolled minus 1 dropped
    expect_true(all(ax$arm == "A" & ax$sub == "x"))

    ## split = TRUE returns every cell; the default pools them.
    cells <- cohort(flow, split = TRUE)
    expect_setequal(names(cells), c("A: x", "A: y", "B: x", "B: y"))
    expect_equal(nrow(cohort(flow)), 14L)      # 18 enrolled minus 4 dropped
})

test_that("cohorts() restores parent keys after a factorial combine peel", {
    stages <- cohorts(
        enroll(factorial_data(), id = "id", label = "Enrolled") |>
            allocate(variable = "arm", label = "Arm assignment") |>
            allocate(variable = "sub") |>
            combine("Per-arm cohort") |>
            endpoint("Analyzed"))
    ## The combine snapshot is keyed by the restored parent labels.
    expect_setequal(names(stages[["Per-arm cohort"]]$n_included), c("A", "B"))
    expect_equal(stages[["Per-arm cohort"]]$n_included[["A"]], 8L)
    expect_equal(stages[["Per-arm cohort"]]$n_included[["B"]], 10L)
})


### * DOT emission

test_that("DOT renders factorial layout with per-parent sub-distributors", {
    dot <- selecta:::export_dot(run_layout(factorial_flow()))
    expect_type(dot, "character")
    ## Second-level sub-distributor bars are linked into one ordered chain by an
    ## invisible weight-20 edge (replacing the old leaf-ordering chain), which
    ## keeps sibling subtrees from swapping under ortho routing.
    expect_match(dot, "style=invis, weight=20", fixed = TRUE, all = FALSE)
    ## The second-level split adds no allocation box, so "Randomized" appears
    ## exactly twice -- in "Randomized patient" (enrolment) and the single
    ## "Randomized" allocation box. A leaked second box would make it three.
    expect_equal(lengths(regmatches(dot, gregexpr("Randomized", dot)))[1], 2L)
})

test_that("DOT emits a 3-arm (3 x 3) factorial without error", {
    ## Geometrically cramped (nine leaf columns) but must still emit; this is
    ## the structural counterpart to the grid render check below.
    flow <- enroll(n = 900, label = "Randomized") |>
        allocate(labels = c("Low", "Mid", "High"), n = c(300, 300, 300),
                 label = "Dose tier") |>
        allocate(labels = c("Schedule A", "Schedule B", "Schedule C"),
                 n = rep(100L, 9L)) |>
        endpoint("Analyzed")
    dot <- selecta:::export_dot(run_layout(flow))
    expect_type(dot, "character")
    ## Nine leaf sub-arms: each shared sub-label appears three times (once per
    ## parent), so the three labels together name nine leaves.
    for (lbl in c("Schedule A", "Schedule B", "Schedule C"))
        expect_equal(lengths(regmatches(dot, gregexpr(lbl, dot)))[1], 3L)
})


### * Grid-engine factorial layout

## The grid engine lays out factorial splits directly: the level-2 sub-arms
## become the leaf columns, each level-1 parent is centered over its own
## sub-arms, and the trunk is centered over all leaves. flowsave() opens the
## device, so a layout fault surfaces as an error here rather than a silently
## malformed page.

test_that("grid engine renders a terminal factorial layout", {
    f <- tempfile(fileext = ".pdf")
    on.exit(unlink(f), add = TRUE)
    expect_no_error(flowsave(factorial_flow(), f))
    expect_true(file.exists(f))
})

test_that("grid engine renders a factorial layout closed by a combine", {
    flow <- enroll(n = 169, label = "Randomized") |>
        allocate(labels = c("Conc", "Seq"), n = c(86, 83)) |>
        allocate(labels = c("Trt A", "Trt B"), n = c(42, 44, 44, 39)) |>
        combine("Per-stratum cohort") |>
        endpoint("Analysis")
    f <- tempfile(fileext = ".pdf")
    on.exit(unlink(f), add = TRUE)
    expect_no_error(flowsave(flow, f))
    expect_true(file.exists(f))
})

test_that("grid engine renders a 3-arm (3 x 3) factorial layout", {
    flow <- enroll(n = 900, label = "Randomized") |>
        allocate(labels = c("Low", "Mid", "High"), n = c(300, 300, 300),
                 label = "Dose tier") |>
        allocate(labels = c("Schedule A", "Schedule B", "Schedule C"),
                 n = rep(100L, 9L)) |>
        exclude("Withdrew", n = rep(5L, 9L)) |>
        endpoint("Analyzed")
    f <- tempfile(fileext = ".pdf")
    on.exit(unlink(f), add = TRUE)
    expect_no_error(flowsave(flow, f))
    expect_true(file.exists(f))
})

test_that("grid engine renders a data-mode factorial layout", {
    flow <- enroll(factorial_data(), id = "id", label = "Enrolled") |>
        allocate(variable = "arm", label = "Arm assignment") |>
        allocate(variable = "sub") |>
        exclude("Dropped", criterion = drop == TRUE) |>
        endpoint("Analyzed")
    f <- tempfile(fileext = ".pdf")
    on.exit(unlink(f), add = TRUE)
    expect_no_error(flowsave(flow, f))
    expect_true(file.exists(f))
})

test_that("grid engine renders a factorial double recombine to a single cohort", {
    flow <- enroll(n = 600, label = "Randomized") |>
        allocate(labels = c("Surgery", "Medical therapy"), n = c(300, 300),
                 label = "Treatment strategy") |>
        allocate(labels = c("High dose", "Low dose"), n = c(150, 150, 150, 150)) |>
        exclude("Did not start treatment", n = c(5, 7, 6, 4)) |>
        combine("Treated per strategy") |>     # peel inner level -> two arms
        combine("Pooled cohort") |>            # full recombine -> single trunk
        endpoint("Analyzed")
    f <- tempfile(fileext = ".pdf")
    on.exit(unlink(f), add = TRUE)
    expect_no_error(flowsave(flow, f))
    expect_true(file.exists(f))
})
