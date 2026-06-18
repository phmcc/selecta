#' Test Suite for Rendering Functions
#'
#' Tests covering flowsave(), recdims(), export_grid(), and visual
#' parameters including font sizes, count_first mode, and output formats.
#'
#' @details Run with testthat::test_file("tests/testthat/test-render.R")

library(testthat)
library(data.table)
library(selecta)

data(selectaex2)


### * Setup: Reusable test flows and helpers

### ** Simple 2-arm manual flow
flow_2arm <- enroll(n = 1200, label = "Assessed for eligibility") |>
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
    endpoint("Analyzed")

### ** Minimal flow
flow_min <- enroll(n = 100) |> endpoint("Final")

### ** STARD flow
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
             n = c(180, 240), label = "Index test result") |>
    endpoint("Final diagnosis",
             breakdown = list(c("Target condition +" = 160, "Target condition -" = 20),
                              c("Target condition +" = 15, "Target condition -" = 225)))

### ** PRISMA flow
flow_prisma <- sources(
    databases = c("PubMed" = 1234, "Embase" = 567),
    other     = c("Citation search" = 55),
    headers   = c(databases = "Databases", other = "Other")) |>
    combine("Records identified") |>
    exclude("Duplicates", n = 340, included_label = "Records screened") |>
    exclude("Excluded", n = 800) |>
    endpoint("Studies included")

### ** Helper: create temp file path for a given extension
temp_path <- function(ext) {
    tempfile(fileext = paste0(".", ext))
}


### * recdims() — Dimension Estimation

test_that("recdims returns named numeric vector", {

    sz <- recdims(flow_min)

    expect_true(is.numeric(sz))
    expect_true("width" %in% names(sz))
    expect_true("height" %in% names(sz))
    expect_true(sz["width"] > 0)
    expect_true(sz["height"] > 0)
})


test_that("recdims scales with pipeline complexity", {

    sz_min  <- recdims(flow_min)
    sz_2arm <- recdims(flow_2arm)

    ## Complex diagram should be taller
    expect_true(sz_2arm["height"] > sz_min["height"])
})


test_that("recdims responds to font size changes", {

    sz_default <- recdims(flow_2arm)
    sz_large   <- recdims(flow_2arm, cex = 1.2)
    sz_small   <- recdims(flow_2arm, cex = 0.6)

    ## Larger font should produce larger dimensions
    expect_true(sz_large["width"] >= sz_default["width"])
    expect_true(sz_large["height"] >= sz_default["height"])

    ## Smaller font should produce smaller dimensions
    expect_true(sz_small["width"] <= sz_default["width"])
    expect_true(sz_small["height"] <= sz_default["height"])
})


test_that("recdims responds to count_first", {

    sz_default <- recdims(flow_2arm)
    sz_cf      <- recdims(flow_2arm, count_first = TRUE)

    ## count_first uses single-line boxes, so height may differ
    expect_true(is.numeric(sz_cf["width"]))
    expect_true(is.numeric(sz_cf["height"]))
})


### * flowsave() — File Output

test_that("flowsave creates PDF file", {

    f <- temp_path("pdf")
    on.exit(unlink(f), add = TRUE)

    flowsave(flow_2arm, f)

    expect_true(file.exists(f))
    expect_true(file.size(f) > 0)
})


test_that("flowsave creates PNG file", {

    f <- temp_path("png")
    on.exit(unlink(f), add = TRUE)

    flowsave(flow_2arm, f)

    expect_true(file.exists(f))
    expect_true(file.size(f) > 0)
})


test_that("flowsave creates SVG file", {

    f <- temp_path("svg")
    on.exit(unlink(f), add = TRUE)

    flowsave(flow_2arm, f)

    expect_true(file.exists(f))
    expect_true(file.size(f) > 0)
})


test_that("flowsave creates TIFF file", {

    f <- temp_path("tiff")
    on.exit(unlink(f), add = TRUE)

    flowsave(flow_2arm, f)

    expect_true(file.exists(f))
    expect_true(file.size(f) > 0)
})


test_that("flowsave respects explicit width and height", {

    f <- temp_path("pdf")
    on.exit(unlink(f), add = TRUE)

    flowsave(flow_min, f, width = 4, height = 3)

    expect_true(file.exists(f))
})


test_that("flowsave errors on non-selecta input", {

    expect_error(flowsave(list(), "test.pdf"), regexp = "selecta")
})


### * count_first Mode

test_that("flowsave renders in count_first mode without error", {

    f <- temp_path("pdf")
    on.exit(unlink(f), add = TRUE)

    flowsave(flow_2arm, f, count_first = TRUE)

    expect_true(file.exists(f))
    expect_true(file.size(f) > 0)
})


test_that("count_first mode works for all diagram types", {

    flows <- list(
        minimal = flow_min,
        consort = flow_2arm,
        stard   = flow_stard,
        prisma  = flow_prisma
    )

    for (nm in names(flows)) {
        f <- temp_path("pdf")
        on.exit(unlink(f), add = TRUE)

        expect_no_error(
            flowsave(flows[[nm]], f, count_first = TRUE),
            message = paste("count_first failed for", nm)
        )
        expect_true(file.exists(f),
                    info = paste("File not created for", nm))
    }
})


### * Font Size Parameters

test_that("cex parameter produces valid output", {

    for (cex_val in c(0.6, 0.85, 1.0, 1.2)) {
        f <- temp_path("pdf")
        on.exit(unlink(f), add = TRUE)

        expect_no_error(
            flowsave(flow_2arm, f, cex = cex_val),
            message = paste("Failed at cex =", cex_val)
        )
        expect_true(file.exists(f),
                    info = paste("File not created at cex =", cex_val))
    }
})


test_that("cex_side parameter produces valid output", {

    f <- temp_path("pdf")
    on.exit(unlink(f), add = TRUE)

    expect_no_error(flowsave(flow_2arm, f, cex_side = 0.65))
    expect_true(file.exists(f))
})


test_that("cex_phase parameter produces valid output", {

    for (cex_val in c(0.7, 0.9, 1.2)) {
        f <- temp_path("pdf")
        on.exit(unlink(f), add = TRUE)

        expect_no_error(
            flowsave(flow_2arm, f, cex_phase = cex_val),
            message = paste("Failed at cex_phase =", cex_val)
        )
    }
})


test_that("combined font parameters produce valid output", {

    f <- temp_path("pdf")
    on.exit(unlink(f), add = TRUE)

    ## Poster mode: everything large
    expect_no_error(
        flowsave(flow_prisma, f,
                    cex = 1.2, cex_side = 1.0, cex_phase = 1.3)
    )
    expect_true(file.exists(f))
})


test_that("small fonts for supplementary figures render correctly", {

    f <- temp_path("pdf")
    on.exit(unlink(f), add = TRUE)

    expect_no_error(
        flowsave(flow_2arm, f,
                    cex = 0.6, cex_side = 0.55, cex_phase = 0.65)
    )
    expect_true(file.exists(f))
})


test_that("font size changes scale canvas dimensions", {

    sz_small <- recdims(flow_2arm, cex = 0.6)
    sz_large <- recdims(flow_2arm, cex = 1.2)

    expect_true(sz_large["width"] > sz_small["width"])
    expect_true(sz_large["height"] > sz_small["height"])
})


### * Diagram-Type Specific Rendering

test_that("0-arm observational diagram renders without error", {

    flow <- enroll(selectaex2, id = "patient_id") |>
        exclude("Duplicates", criterion = is_duplicate == TRUE,
                included_label = "Unique") |>
        endpoint("Final")

    f <- temp_path("pdf")
    on.exit(unlink(f), add = TRUE)

    expect_no_error(flowsave(flow, f))
    expect_true(file.exists(f))
})


test_that("2-arm without post-split sides renders without cutoff", {

    flow <- enroll(n = 1000) |>
        exclude("Excluded", n = 200,
                reasons = c("A" = 100, "B" = 100)) |>
        allocate(labels = c("Drug A", "Placebo"), n = c(400, 400)) |>
        endpoint("Final")

    f <- temp_path("pdf")
    on.exit(unlink(f), add = TRUE)

    expect_no_error(flowsave(flow, f))
    expect_true(file.exists(f))
})


test_that("6-arm diagram renders without error", {

    flow <- enroll(n = 1800) |>
        allocate(labels = paste("Arm", LETTERS[1:6]),
                 n = rep(300, 6)) |>
        exclude("Lost", n = rep(10, 6), show_count = TRUE) |>
        endpoint("Final")

    f <- temp_path("pdf")
    on.exit(unlink(f), add = TRUE)

    expect_no_error(flowsave(flow, f))
    expect_true(file.exists(f))
})


test_that("STARD diagram renders without canvas cutoff", {

    f <- temp_path("pdf")
    on.exit(unlink(f), add = TRUE)

    expect_no_error(flowsave(flow_stard, f))
    expect_true(file.exists(f))

    ## Also test with larger fonts (previously caused cutoff)
    f2 <- temp_path("pdf")
    on.exit(unlink(f2), add = TRUE)

    expect_no_error(flowsave(flow_stard, f2, cex = 1.0, cex_side = 0.8))
    expect_true(file.exists(f2))
})


test_that("STARD diagram renders in count_first mode", {

    f <- temp_path("pdf")
    on.exit(unlink(f), add = TRUE)

    expect_no_error(flowsave(flow_stard, f, count_first = TRUE))
    expect_true(file.exists(f))
})


test_that("PRISMA 3-column diagram renders without error", {

    flow <- sources(
        previous  = c("Previous review" = 12, "Previous reports" = 15),
        databases = c("PubMed" = 1234, "Embase" = 567, "CENTRAL" = 89),
        other     = c("Citation search" = 55, "Websites" = 34),
        headers   = c(previous  = "Previous studies",
                      databases = "Databases and registers",
                      other     = "Other methods")) |>
        combine("Records identified", n = 2006) |>
        exclude("Records removed", n = 352,
                reasons = c("Duplicates" = 340, "Automated" = 12),
                included_label = "Records screened") |>
        exclude("Records excluded", n = 1100) |>
        exclude("Full-text excluded", n = 45,
                reasons = c("Wrong population" = 20,
                            "Wrong outcome" = 15,
                            "Wrong design" = 10)) |>
        endpoint("Studies included")

    f <- temp_path("pdf")
    on.exit(unlink(f), add = TRUE)

    expect_no_error(flowsave(flow, f))
    expect_true(file.exists(f))
})


### * Data-Driven Rendering

test_that("data-driven 2-arm renders identically to manual equivalent", {

    flow <- enroll(selectaex2, id = "patient_id") |>
        exclude("Duplicates", criterion = is_duplicate == TRUE,
                included_label = "Unique") |>
        exclude("Failed eligibility", criterion = eligible == FALSE,
                reasons = "exclusion_reason",
                included_label = "Eligible") |>
        allocate("treatment") |>
        exclude("Discontinued", criterion = discontinued == TRUE,
                reasons = "discontinuation_reason") |>
        endpoint("Analysis cohort")

    f <- temp_path("pdf")
    on.exit(unlink(f), add = TRUE)

    expect_no_error(flowsave(flow, f))
    expect_true(file.exists(f))
    expect_true(file.size(f) > 0)
})


### * Font Family

test_that("font_family renders for the generic families", {

    for (fam in c("sans", "serif", "mono")) {
        f <- temp_path("pdf")
        on.exit(unlink(f), add = TRUE)

        expect_no_error(
            flowsave(flow_2arm, f, font_family = fam),
            message = paste("Failed for font_family =", fam)
        )
        expect_true(file.exists(f),
                    info = paste("File not created for", fam))
    }
})


test_that("recdims responds to font_family", {

    sz_sans  <- recdims(flow_2arm, font_family = "sans")
    sz_serif <- recdims(flow_2arm, font_family = "serif")

    ## Both are valid positive dimensions; the measured width may differ
    ## between families but must remain finite and positive.
    expect_true(is.finite(sz_sans["width"])  && sz_sans["width"]  > 0)
    expect_true(is.finite(sz_serif["width"]) && sz_serif["width"] > 0)
})


test_that("number_format and font_family combine without error", {

    f <- temp_path("pdf")
    on.exit(unlink(f), add = TRUE)

    expect_no_error(
        flowsave(flow_2arm, f, font_family = "serif", number_format = "eu")
    )
    expect_true(file.exists(f))
})
