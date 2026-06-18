#' Test Suite for Options and Arithmetic Safeguards
#'
#' Tests covering the package's session options and the manual-mode count
#' checks:
#'   * selecta.check_arithmetic and the warn_arithmetic() advisories
#'   * selecta.number_format and the number-format presets
#'   * selecta.debug_layout and the layout trace
#'   * phase-label wrapping (phase_multiline / phase_max_lines)
#'   * the .measure_dev measurement hook of recdims()
#'
#' @details Run with testthat::test_file("tests/testthat/test-options.R")

library(testthat)
library(data.table)
library(selecta)

data(selectaex2)

run_compute <- function(flow) selecta:::compute(flow)


### * Arithmetic Safeguards (manual mode)
## The checks are advisory: they emit a warning but never alter the counts.
## They fire at compute() time, which underlies flowchart(), flowsave(),
## recdims(), print(), and summary().

test_that("split whose arms do not sum to the incoming count warns", {

    flow <- enroll(n = 1000) |>
        exclude("Excluded", n = 100, included_label = "Eligible") |>
        allocate(labels = c("A", "B"), n = c(450, 400)) |>  # 850 != 900
        endpoint("Final")

    expect_warning(run_compute(flow), regexp = "arm counts sum")
})


test_that("split whose arms sum correctly does not warn", {

    flow <- enroll(n = 1000) |>
        exclude("Excluded", n = 100, included_label = "Eligible") |>
        allocate(labels = c("A", "B"), n = c(450, 450)) |>  # 900 == 900
        endpoint("Final")

    expect_no_warning(run_compute(flow))
})


test_that("exclusion larger than the available count warns", {

    flow <- enroll(n = 100) |>
        exclude("Excluded", n = 150) |>  # cannot remove 150 of 100
        endpoint("Final")

    expect_warning(run_compute(flow), regexp = "are available")
})


test_that("exclusion sub-reasons that do not sum to the total warn", {

    flow <- enroll(n = 1000) |>
        exclude("Excluded", n = 100,
                reasons = c("A" = 60, "B" = 30)) |>  # 90 != 100
        endpoint("Final")

    expect_warning(run_compute(flow), regexp = "sub-reasons sum")
})


test_that("per-arm sub-reasons that do not sum to the arm total warn", {

    ## resolve_exclusion() runs once per arm, so the scalar sub-reasons check
    ## covers each arm; arm 1 here is mismatched (30 != 40).
    flow <- enroll(n = 1000) |>
        allocate(labels = c("A", "B"), n = c(500, 500)) |>
        exclude("Discontinued", n = c(40, 60),
                reasons = list(c("AE" = 18, "WC" = 12),     # 30 != 40
                               c("AE" = 30, "LF" = 30))) |>  # 60 == 60
        endpoint("Final")

    expect_warning(run_compute(flow), regexp = "sub-reasons sum")
})


test_that("matching sub-reasons do not warn", {

    flow <- enroll(n = 1000) |>
        exclude("Excluded", n = 100,
                reasons = c("A" = 60, "B" = 40)) |>  # 100 == 100
        endpoint("Final")

    expect_no_warning(run_compute(flow))
})


test_that("two-level reasons that do not sum to the total warn", {

    ## Each entry contributes its own count (a leaf) or the sum of its
    ## sub-reasons (a named vector): 80 + 10 = 90, not 100.
    flow <- enroll(n = 1000) |>
        exclude("Excluded", n = 100,
                reasons = list("Ineligible" = c("Age" = 60, "Other" = 20),
                               "Declined"   = 10)) |>  # 90 != 100
        endpoint("Final")

    expect_warning(run_compute(flow), regexp = "sub-reasons sum")
})


test_that("two-level reasons that reconcile do not warn", {

    flow <- enroll(n = 1000) |>
        exclude("Excluded", n = 100,
                reasons = list("Ineligible" = c("Age" = 60, "Other" = 25),
                               "Declined"   = 15)) |>  # 100 == 100
        endpoint("Final")

    expect_no_warning(run_compute(flow))
})


test_that("data-mode two-column reasons never warn (counts are data-derived)", {

    ## Cross-tabulated counts always reconcile to the exclusion total, so no
    ## arithmetic advisory should fire.
    dd <- data.frame(
        id        = seq_len(200),
        drop      = c(rep(TRUE, 80), rep(FALSE, 120)),
        reason    = NA_character_,
        subreason = NA_character_,
        stringsAsFactors = FALSE
    )
    dd$reason[1:80]     <- c(rep("R1", 50), rep("R2", 30))
    dd$subreason[1:30]  <- "a"
    dd$subreason[31:50] <- "b"
    dd$subreason[51:80] <- "c"

    flow <- enroll(dd, id = "id") |>
        exclude("Excluded", criterion = drop == TRUE,
                reasons = c("reason", "subreason")) |>
        endpoint("Included")

    expect_no_warning(run_compute(flow))
})


test_that("split endpoint whose groups do not sum to the incoming count warns", {

    flow <- enroll(n = 300) |>
        endpoint("Allocated", groups = c("A", "B"), n = c(100, 150))  # 250 != 300

    expect_warning(run_compute(flow), regexp = "group counts sum")
})


test_that("split endpoint whose groups sum correctly does not warn", {

    flow <- enroll(n = 300) |>
        endpoint("Allocated", groups = c("A", "B", "C"),
                 n = c(100, 100, 100))  # 300 == 300

    expect_no_warning(run_compute(flow))
})


test_that("manual combine total disagreeing with its streams warns", {

    flow <- enroll(n = 1000) |>
        stratify(labels = c("A", "B"), n = c(500, 500), label = "Stratum") |>
        combine("Pooled", n = 1100) |>  # streams sum to 1000
        endpoint("Final")

    expect_warning(run_compute(flow), regexp = "incoming streams sum")
})


test_that("selecta.check_arithmetic = FALSE silences the warnings", {

    flow <- enroll(n = 100) |>
        exclude("Excluded", n = 150) |>
        endpoint("Final")

    withr_local <- options(selecta.check_arithmetic = FALSE)
    on.exit(options(withr_local), add = TRUE)

    expect_no_warning(run_compute(flow))
})


test_that("warnings do not alter the displayed counts", {

    ## Even when an exclusion over-draws, the user's numbers are preserved.
    flow <- enroll(n = 100) |>
        exclude("Excluded", n = 150, included_label = "Remaining") |>
        endpoint("Final")

    suppressWarnings({
        graph <- run_compute(flow)
    })

    side <- graph$nodes[role == "side"]
    expect_equal(side$n, 150)  # not clamped
})


### * number_format presets and selecta.number_format

test_that("resolve_number_marks returns correct marks for each preset", {

    expect_equal(selecta:::resolve_number_marks("us"),
                 list(big.mark = ",", decimal.mark = "."))
    expect_equal(selecta:::resolve_number_marks("eu"),
                 list(big.mark = ".", decimal.mark = ","))
    expect_equal(selecta:::resolve_number_marks("none"),
                 list(big.mark = "", decimal.mark = "."))
    expect_equal(selecta:::resolve_number_marks("space")$decimal.mark, ".")
})


test_that("resolve_number_marks accepts a custom c(big, decimal) vector", {

    marks <- selecta:::resolve_number_marks(c("'", "."))
    expect_equal(marks$big.mark, "'")
    expect_equal(marks$decimal.mark, ".")
})


test_that("resolve_number_marks rejects an unknown preset", {

    expect_error(selecta:::resolve_number_marks("xx"), regexp = "preset")
})


test_that("fmt_n applies the thousands separator above 1000 only", {

    expect_equal(selecta:::fmt_n(1234, list(big.mark = ",", decimal.mark = ".")),
                 "1,234")
    expect_equal(selecta:::fmt_n(1234, list(big.mark = ".", decimal.mark = ",")),
                 "1.234")
    expect_equal(selecta:::fmt_n(1284500, list(big.mark = ".", decimal.mark = ",")),
                 "1.284.500")
    ## Below 1000: no separator
    expect_equal(selecta:::fmt_n(999, list(big.mark = ",", decimal.mark = ".")),
                 "999")
})


test_that("fmt_n is vectorized and maps NA to empty string", {

    out <- selecta:::fmt_n(c(1234, NA, 50),
                           list(big.mark = ",", decimal.mark = "."))
    expect_equal(out, c("1,234", "", "50"))
})


test_that("selecta.number_format option is honored when no marks supplied", {

    old <- options(selecta.number_format = "eu")
    on.exit(options(old), add = TRUE)

    expect_equal(selecta:::resolve_number_marks(),
                 list(big.mark = ".", decimal.mark = ","))
    expect_equal(selecta:::fmt_n(1234), "1.234")
})


test_that("validate_number_format passes valid and rejects invalid specs", {

    expect_true(selecta:::validate_number_format("eu"))
    expect_true(selecta:::validate_number_format(NULL))
    expect_true(selecta:::validate_number_format(c(".", ",")))
    expect_error(selecta:::validate_number_format("bogus"), regexp = "preset")
    expect_error(selecta:::validate_number_format(c(".", ".")),
                 regexp = "same")
})


test_that("number_format renders without error through flowsave", {

    flow <- enroll(n = 1284500, label = "Invited") |>
        exclude("Did not attend", n = 458200,
                included_label = "Attended") |>
        endpoint("Entered surveillance")

    f <- tempfile(fileext = ".pdf")
    on.exit(unlink(f), add = TRUE)

    expect_no_error(flowsave(flow, f, number_format = "eu"))
    expect_true(file.exists(f))
})


### * Phase-label wrapping (phase_multiline / phase_max_lines)

test_that("recdims accepts phase_multiline and phase_max_lines", {

    flow <- enroll(n = 1000) |>
        phase("Enrollment and baseline assessment") |>
        exclude("Excluded", n = 100) |>
        phase("Randomized allocation to study arms") |>
        allocate(labels = c("A", "B"), n = c(450, 450)) |>
        endpoint("Analyzed")

    sz_wrap   <- recdims(flow, phase_multiline = TRUE,  phase_max_lines = 3L)
    sz_nowrap <- recdims(flow, phase_multiline = FALSE)

    expect_true(is.numeric(sz_wrap) && sz_wrap["height"] > 0)
    expect_true(is.numeric(sz_nowrap) && sz_nowrap["height"] > 0)
})


test_that("wrapping long phase labels reduces required height versus no-wrap", {

    ## Long descriptive labels: with wrapping they consume strip width rather
    ## than stretching the diagram vertically, so the wrapped canvas should be
    ## no taller (and generally shorter) than the unwrapped one.
    flow <- enroll(n = 1000) |>
        phase("Enrollment and baseline assessment of participants") |>
        exclude("Excluded", n = 100, included_label = "Eligible") |>
        phase("Randomized allocation to the study arms") |>
        allocate(labels = c("Drug A", "Placebo"), n = c(450, 450)) |>
        phase("Post randomization follow up period") |>
        exclude("Lost to follow-up", n = c(20, 20)) |>
        phase("Intention to treat analysis set") |>
        endpoint("Analyzed")

    sz_wrap   <- recdims(flow, phase_multiline = TRUE)
    sz_nowrap <- recdims(flow, phase_multiline = FALSE)

    expect_true(sz_wrap["height"] <= sz_nowrap["height"])
})


test_that("phase_multiline defaults to TRUE in recdims", {

    flow <- enroll(n = 1000) |>
        phase("A reasonably long phase label that may wrap across lines") |>
        exclude("Excluded", n = 100) |>
        endpoint("Final")

    sz_default <- recdims(flow)
    sz_true    <- recdims(flow, phase_multiline = TRUE)

    ## Default behavior matches an explicit TRUE
    expect_equal(unname(sz_default["height"]), unname(sz_true["height"]))
    expect_equal(unname(sz_default["width"]),  unname(sz_true["width"]))
})


test_that("explicit newline in a phase label renders without error", {

    flow <- enroll(n = 1000) |>
        phase("Enrollment and\nbaseline assessment") |>
        exclude("Excluded", n = 100) |>
        phase("Analysis") |>
        endpoint("Final")

    f <- tempfile(fileext = ".pdf")
    on.exit(unlink(f), add = TRUE)

    expect_no_error(flowsave(flow, f))
    expect_true(file.exists(f))
})


### * selecta.debug_layout trace

test_that("selecta.debug_layout = TRUE emits a layout trace via message()", {

    flow <- enroll(n = 500) |>
        exclude("Excluded", n = 100) |>
        endpoint("Final")

    old <- options(selecta.debug_layout = TRUE)
    on.exit(options(old), add = TRUE)

    ## The trace is written to the message stream, not the return value
    expect_message(run_compute(flow), regexp = "selecta debug")
})


test_that("debug trace is silent by default", {

    flow <- enroll(n = 500) |>
        exclude("Excluded", n = 100) |>
        endpoint("Final")

    old <- options(selecta.debug_layout = NULL)
    on.exit(options(old), add = TRUE)

    expect_no_message(run_compute(flow))
})


test_that("DOT engine emits its source under selecta.debug_layout", {

    flow <- enroll(n = 500) |>
        exclude("Excluded", n = 100) |>
        endpoint("Final")

    old <- options(selecta.debug_layout = TRUE)
    on.exit(options(old), add = TRUE)

    expect_message(flowchart(flow, engine = "dot"), regexp = "export_dot")
})


### * recdims() measurement hook (.measure_dev)

test_that("recdims accepts a custom .measure_dev device opener", {

    flow <- enroll(n = 1000, label = "Assessed") |>
        exclude("Excluded", n = 200, included_label = "Eligible") |>
        allocate(labels = c("A", "B"), n = c(400, 400)) |>
        endpoint("Analyzed")

    ## A measurer that opens a standard pdf device on a temp file
    measurer <- function() {
        tf <- tempfile(fileext = ".pdf")
        grDevices::pdf(tf, width = 10, height = 10)
        tf
    }

    sz <- recdims(flow, .measure_dev = measurer)
    expect_true(is.numeric(sz))
    expect_true(sz["width"] > 0 && sz["height"] > 0)
})


test_that("recdims default measurement matches an explicit pdf measurer", {

    flow <- enroll(n = 1000, label = "Assessed") |>
        exclude("Excluded", n = 200, included_label = "Eligible") |>
        endpoint("Analyzed")

    sz_default <- recdims(flow)
    sz_pdf <- recdims(flow, .measure_dev = function() {
        tf <- tempfile(fileext = ".pdf")
        grDevices::pdf(tf, width = 10, height = 10)
        tf
    })

    ## Both measure on a pdf device, so dimensions should coincide
    expect_equal(unname(sz_default["width"]),  unname(sz_pdf["width"]))
    expect_equal(unname(sz_default["height"]), unname(sz_pdf["height"]))
})
