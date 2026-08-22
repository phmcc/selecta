## Guards the single source of truth for layout defaults. export_grid()'s
## formals are authoritative; recdims() and flowsave() forward a named subset
## of them and must never restate a default value.

test_that("every measurement parameter is an export_grid() formal", {
    grid_formals <- names(formals(export_grid))
    expect_true(all(.measure_params %in% grid_formals),
                info = paste("Not formals of export_grid():",
                             paste(setdiff(.measure_params, grid_formals),
                                   collapse = ", ")))
})


test_that("recdims() restates no layout default", {
    ## Every shared parameter must default to NULL, so an unsupplied value is
    ## forwarded unset and resolved by export_grid() alone.
    rec_formals <- formals(recdims)
    for (p in .measure_params) {
        expect_true(p %in% names(rec_formals),
                    info = paste0("recdims() has no '", p, "' parameter"))
        expect_null(rec_formals[[p]],
                    info = paste0("recdims() restates a default for '", p, "'"))
    }
})


test_that("flowchart() restates no drawing-routine default", {
    ## flowchart() names count_first explicitly rather than leaving it to the
    ## dots, so it must default to NULL for the same reason recdims() does.
    expect_null(formals(flowchart)$count_first)

    ## Any other named formal of flowchart() that export_grid() or export_dot()
    ## also declares must either be absent or default to NULL.
    shared <- intersect(names(formals(flowchart)),
                        union(names(formals(export_grid)),
                              names(formals(export_dot))))
    for (p in shared)
        expect_null(formals(flowchart)[[p]],
                    info = paste0("flowchart() restates a default for '", p, "'"))
})


test_that("flowsave() forwards the shared parameter set", {
    ## The forwarding loop must reference the constant rather than a literal
    ## vector, so a parameter added in one place cannot be missed in another.
    body_txt <- paste(deparse(body(flowsave)), collapse = " ")
    expect_true(grepl(".measure_params", body_txt, fixed = TRUE))
})


test_that("measurement matches rendering under a non-default cex", {
    skip_on_cran()

    flow <- enroll(n = 500) |>
        exclude("Ineligible", n = 65) |>
        endpoint("Analyzed")

    ## A parameter supplied to recdims() must reach export_grid(); a change in
    ## typography must therefore change the recommendation.
    small <- recdims(flow, cex = 0.6)
    large <- recdims(flow, cex = 1.4)

    expect_gt(unname(large["width"]),  unname(small["width"]))
    expect_gt(unname(large["height"]), unname(small["height"]))

    ## An unsupplied cex_side must track cex, as export_grid() resolves it.
    paired <- recdims(flow, cex = 1.4, cex_side = 1.4)
    expect_equal(unname(large["width"]),  unname(paired["width"]))
    expect_equal(unname(large["height"]), unname(paired["height"]))
})


test_that("recdims() units convert without clipping", {
    skip_on_cran()

    flow <- enroll(n = 500) |>
        exclude("Ineligible", n = 65) |>
        endpoint("Analyzed")

    inches <- recdims(flow)
    mm     <- recdims(flow, units = "mm")

    expect_identical(attr(inches, "units"), "in")
    expect_identical(attr(mm, "units"), "mm")

    ## Rounding up in millimeters cannot yield a canvas smaller than the
    ## measurement, so the metric result is never below the inch result less
    ## one rounding step.
    expect_gt(unname(mm["width"]) / 25.4, unname(inches["width"]) - 0.1)
    expect_gt(unname(mm["height"]) / 25.4, unname(inches["height"]) - 0.1)
})
