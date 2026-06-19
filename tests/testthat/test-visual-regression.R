#' Visual Regression Script
#'
#' Renders every diagram type the package can produce, from the simplest
#' enroll-to-endpoint flow to multi-source reviews and adaptive
#' split-and-recombine designs, through BOTH rendering engines (the grid
#' engine via flowsave() and the Graphviz DOT engine via flowchart(engine =
#' "dot")), so the two outputs can be compared diagram for diagram. Coverage
#' includes the EQUATOR families (CONSORT, STROBE, PRISMA, MOOSE, STARD),
#' single-arm and 2/3/6-arm designs in manual and data-driven modes, split
#' endpoints (terminal group splits), two-level reason -> sub-reason
#' breakdowns and collapse_singletons, split-and-recombine topologies, edge
#' cases, and the grid- and DOT-specific styling controls (font sizes, color
#' palettes, font families, multi-line phase labels, count-first boxes, rich
#' vs plain DOT formatting, and orthogonal routing). DOT output is rasterized
#' when the system `dot` binary is available, otherwise written as .dot files,
#' so the script never fails for lack of Graphviz. It also exercises the
#' print() and cohort() paths.
#'
#' @details Run with testthat::test_file("tests/testthat/test-visual-regression.R")

library(testthat)
library(selecta)

## Render only off-CRAN. testthat::skip_on_cran() skips when !interactive()
## and the NOT_CRAN environment variable is unset; mirror that exactly so
## rendering and the assertions below stay in lockstep. On CRAN the suite
## still builds the (cheap) flow objects but writes nothing and costs
## effectively no time; run interactively or on CI (where devtools/rcmdcheck
## set NOT_CRAN=true) it renders the full gallery for visual inspection.
.render <- interactive() ||
    identical(tolower(Sys.getenv("NOT_CRAN")), "true")

## Output directory. Defaults to the session temp directory: CRAN-safe, and
## inspectable for the life of the session (the path is printed in the summary
## below). Set SELECTA_TEST_OUTPUT to a stable absolute path to retain renders
## across sessions for run-to-run visual comparison.
outdir <- Sys.getenv("SELECTA_TEST_OUTPUT",
                     unset = file.path(tempdir(), "test_output"))
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

## On CRAN, shadow flowsave() with a no-op so the scattered grid render calls
## below write nothing and cost nothing. flowsave() is called by bare name
## throughout, so a global binding masks the package export for the rest of the
## script; the DOT path is short-circuited inside render_dot() likewise.
if (!.render) {
    flowsave <- function(...) invisible(NULL)
}

### * CONSORT / STROBE — Data-driven (package datasets)

### ** 0-arm (observational, no stratification)

data(selectaex0)

flow0 <- enroll(selectaex0, id = "patient_id") |>
    phase("Screening") |>
    exclude("Duplicate records", criterion = is_duplicate == TRUE,
            included_label = "Unique records") |>
    exclude("Failed eligibility",
            criterion = eligible == FALSE & is_duplicate == FALSE,
            reasons = "exclusion_reason",
            included_label = "Eligible cohort") |>
    phase("Follow-up") |>
    exclude("Lost to follow-up",
            criterion = lost_to_followup == TRUE,
            reasons = "followup_loss_reason") |>
    phase("Study") |>
    endpoint("Analysis cohort")

flowsave(flow0, file.path(outdir, "grid_dd_0arm.pdf"))
flowsave(flow0, file.path(outdir, "grid_dd_0arm_cf.pdf"), count_first = TRUE)

### ** 2-arm (CONSORT RCT)

data(selectaex2)

flow2 <- enroll(selectaex2, id = "patient_id") |>
    phase("Screening") |>
    exclude("Duplicate records", criterion = is_duplicate == TRUE,
            included_label = "Unique records") |>
    exclude("Failed eligibility", criterion = eligible == FALSE,
            reasons = "exclusion_reason",
            included_label = "Eligible cohort") |>
    phase("Allocation") |>
    allocate("treatment") |>
    phase("Follow-up") |>
    exclude("Discontinued", criterion = discontinued == TRUE,
            reasons = "discontinuation_reason") |>
    phase("Study") |>
    endpoint("Analysis cohort")

flowsave(flow2, file.path(outdir, "grid_dd_2arm.pdf"))
flowsave(flow2, file.path(outdir, "grid_dd_2arm_cf.pdf"), count_first = TRUE)

### ** 3-arm (CONSORT RCT)

data(selectaex3)

flow3 <- enroll(selectaex3, id = "patient_id") |>
    phase("Screening") |>
    exclude("Duplicate records", criterion = is_duplicate == TRUE,
            included_label = "Unique records") |>
    exclude("Failed eligibility", criterion = eligible == FALSE,
            reasons = "exclusion_reason",
            included_label = "Eligible cohort") |>
    phase("Allocation") |>
    allocate("treatment") |>
    phase("Follow-up") |>
    exclude("Discontinued", criterion = discontinued == TRUE,
            reasons = "discontinuation_reason") |>
    phase("Study") |>
    endpoint("Analysis cohort")

flowsave(flow3, file.path(outdir, "grid_dd_3arm.pdf"))
flowsave(flow3, file.path(outdir, "grid_dd_3arm_cf.pdf"), count_first = TRUE)

### ** 6-arm (dose-finding, uses stratify)

data(selectaex6)

flow6 <- enroll(selectaex6, id = "patient_id") |>
    phase("Screening") |>
    exclude("Duplicate records", criterion = is_duplicate == TRUE,
            included_label = "Unique records") |>
    exclude("Failed eligibility", criterion = eligible == FALSE,
            reasons = "exclusion_reason",
            included_label = "Eligible cohort") |>
    phase("Allocation") |>
    stratify("treatment") |>
    phase("Follow-up") |>
    exclude("Discontinued", criterion = discontinued == TRUE,
            reasons = "discontinuation_reason") |>
    phase("Study") |>
    endpoint("Analysis cohort")

flowsave(flow6, file.path(outdir, "grid_dd_6arm.pdf"))
flowsave(flow6, file.path(outdir, "grid_dd_6arm_cf.pdf"), count_first = TRUE)

### ** cohort() extraction

final  <- cohort(flow2)
by_arm <- cohort(flow2, split = TRUE)
arm1   <- cohort(flow2, arm = names(by_arm)[1])
stopifnot(is.data.frame(final))
stopifnot(is.list(by_arm), length(by_arm) == 2)
stopifnot(nrow(final) == sum(vapply(by_arm, nrow, integer(1))))
cat("cohort() extraction: OK\n")

### * CONSORT / STROBE — Manual

### ** 2-arm manual

flow_m2 <- enroll(n = 1200, label = "Assessed for eligibility") |>
    phase("Enrollment") |>
    exclude("Excluded", n = 300,
            reasons = c("Not meeting inclusion criteria" = 160,
                        "Declined to participate" = 90,
                        "Other reasons" = 50)) |>
    phase("Allocation") |>
    allocate(labels = c("Drug A", "Placebo"), n = c(450, 450)) |>
    phase("Follow-up") |>
    exclude("Lost to follow-up", n = c(22, 18), show_count = TRUE) |>
    exclude("Discontinued intervention", n = c(8, 12)) |>
    phase("Analysis") |>
    endpoint("Analyzed")

flowsave(flow_m2, file.path(outdir, "grid_man_2arm.pdf"))
flowsave(flow_m2, file.path(outdir, "grid_man_2arm_cf.pdf"), count_first = TRUE)

### ** 3-arm manual

flow_m3 <- enroll(n = 900, label = "Screened") |>
    phase("Enrollment") |>
    exclude("Excluded", n = 150) |>
    phase("Allocation") |>
    allocate(labels = c("Drug A", "Drug B", "Placebo"),
             n = c(250, 250, 250)) |>
    phase("Follow-up") |>
    exclude("Discontinued", n = c(10, 15, 8), show_count = TRUE) |>
    phase("Analysis") |>
    endpoint("Analyzed")

flowsave(flow_m3, file.path(outdir, "grid_man_3arm.pdf"))
flowsave(flow_m3, file.path(outdir, "grid_man_3arm_cf.pdf"), count_first = TRUE)

### ** 6-arm manual (stress test)

flow_m6 <- enroll(n = 1800, label = "Screened") |>
    phase("Enrollment") |>
    exclude("Excluded", n = 300) |>
    phase("Allocation") |>
    allocate(labels = paste("Arm", LETTERS[1:6]),
             n = rep(250, 6)) |>
    phase("Follow-up") |>
    exclude("Lost", n = c(10, 12, 8, 15, 11, 9), show_count = TRUE) |>
    phase("Analysis") |>
    endpoint("Analyzed")

flowsave(flow_m6, file.path(outdir, "grid_man_6arm.pdf"))

### ** STROBE per-arm labels

flow_strobe <- enroll(n = 5000, label = "Source population") |>
    phase("Eligibility") |>
    exclude("Did not meet inclusion criteria", n = 800,
            reasons = c("Age outside range" = 340,
                        "Prior treatment" = 280,
                        "Comorbidity" = 180),
            included_label = "Met inclusion criteria") |>
    exclude("Declined to participate", n = 340) |>
    phase("Baseline") |>
    stratify(labels = c("Exposed", "Unexposed"), n = c(1900, 1960),
             label = "Classified by exposure") |>
    phase("Follow-up") |>
    exclude(c("Treatment discontinued", "Initiated treatment"),
            n = c(45, 52),
            included_label = c("Continued treatment", "Remained unexposed")) |>
    exclude("Withdrew consent", n = c(12, 18)) |>
    phase("Analysis") |>
    endpoint("Included in analysis")

flowsave(flow_strobe, file.path(outdir, "grid_man_strobe.pdf"))
flowsave(flow_strobe, file.path(outdir, "grid_man_strobe_cf.pdf"),
            count_first = TRUE)

### * PRISMA 2020

### ** 3-column (previous + databases + other)

flow_prisma3 <- sources(
    previous  = c("Previous review" = 12, "Previous reports" = 15),
    databases = c("PubMed" = 1234, "Embase" = 567, "CENTRAL" = 89),
    other     = c("Citation search" = 55, "Websites" = 34),
    headers   = c(previous  = "Previous studies",
                  databases = "Databases and registers",
                  other     = "Other methods")) |>
    phase("Identification") |>
    combine("Records identified", n = 2006) |>
    exclude("Records removed before screening", n = 352,
            reasons = c("Duplicates" = 340,
                        "Marked ineligible by automation" = 12),
            included_label = "Records screened") |>
    phase("Screening") |>
    exclude("Records excluded after title/abstract screening",
            n = 1100, show_count = FALSE) |>
    exclude("Reports not retrieved", n = 15, show_count = FALSE) |>
    exclude("Reports excluded after full-text review", n = 45,
            reasons = c("Wrong population" = 20,
                        "Wrong outcome" = 15,
                        "Wrong study design" = 10)) |>
    phase("Included") |>
    endpoint("Studies included in review")

flowsave(flow_prisma3, file.path(outdir, "grid_prisma_3col.pdf"))
flowsave(flow_prisma3, file.path(outdir, "grid_prisma_3col_cf.pdf"),
            count_first = TRUE)

### ** 2-column (databases + other)

flow_prisma2 <- sources(
    databases = c("PubMed" = 1234, "Embase" = 567, "CENTRAL" = 89),
    other     = c("Citation search" = 55, "Websites" = 34),
    headers   = c(databases = "Databases and registers",
                  other     = "Other methods")) |>
    phase("Identification") |>
    combine("Records identified") |>
    exclude("Duplicates removed", n = 340,
            included_label = "Records screened") |>
    phase("Screening") |>
    exclude("Records excluded", n = 800, show_count = FALSE) |>
    exclude("Full-text excluded", n = 45,
            reasons = c("Wrong population" = 20,
                        "Wrong outcome" = 15,
                        "Wrong design" = 10)) |>
    phase("Included") |>
    endpoint("Studies included")

flowsave(flow_prisma2, file.path(outdir, "grid_prisma_2col.pdf"))
flowsave(flow_prisma2, file.path(outdir, "grid_prisma_2col_cf.pdf"),
            count_first = TRUE)

### ** 1-column (flat sources, no grouping)

flow_prisma1 <- sources(PubMed = 1234, Embase = 567, CENTRAL = 89) |>
    phase("Identification") |>
    combine("Records identified") |>
    exclude("Duplicates removed", n = 340,
            included_label = "Records screened") |>
    phase("Screening") |>
    exclude("Records excluded", n = 800) |>
    phase("Included") |>
    endpoint("Studies included")

flowsave(flow_prisma1, file.path(outdir, "grid_prisma_1col.pdf"))
flowsave(flow_prisma1, file.path(outdir, "grid_prisma_1col_cf.pdf"),
            count_first = TRUE)

### ** MOOSE (gray literature variant)

flow_moose <- sources(
    databases = c("MEDLINE" = 2500, "Embase" = 1800, "PsycINFO" = 400),
    gray      = c("Dissertations" = 45, "Conference proceedings" = 120),
    headers   = c(databases = "Electronic databases",
                  gray      = "Gray literature")) |>
    phase("Identification") |>
    combine("Total records") |>
    exclude("Duplicates removed", n = 1200,
            included_label = "Unique records") |>
    phase("Screening") |>
    exclude("Excluded on title/abstract", n = 2800, show_count = FALSE) |>
    exclude("Excluded on full text", n = 150,
            reasons = c("No control group" = 60,
                        "Insufficient data" = 50,
                        "Wrong population" = 40)) |>
    phase("Included") |>
    endpoint("Studies in meta-analysis")

flowsave(flow_moose, file.path(outdir, "grid_moose.pdf"))
flowsave(flow_moose, file.path(outdir, "grid_moose_cf.pdf"), count_first = TRUE)

### * STARD 2015

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
             n = c(180, 240),
             label = "Index test result") |>
    endpoint("Final diagnosis",
             breakdown = list(c("Target condition +" = 160, "Target condition -" = 20),
                              c("Target condition +" = 15, "Target condition -" = 225)))

flowsave(flow_stard, file.path(outdir, "grid_stard.pdf"))
flowsave(flow_stard, file.path(outdir, "grid_stard_cf.pdf"), count_first = TRUE)

### * Edge cases

### ** Minimal (enroll + endpoint only)

flow_min <- enroll(n = 100) |> endpoint("Final")
flowsave(flow_min, file.path(outdir, "grid_edge_minimal.pdf"))

### ** Zero-count exclusion

flow_zero <- enroll(n = 500, label = "Screened") |>
    exclude("Protocol violation", n = 0, show_zero = TRUE, show_count = TRUE) |>
    exclude("Adverse event", n = 10, show_count = TRUE) |>
    endpoint("Analyzed")
flowsave(flow_zero, file.path(outdir, "grid_edge_zero_count.pdf"))

### ** show_count=FALSE stacking (4 consecutive suppressed)

flow_nocount <- enroll(n = 2000, label = "Records identified") |>
    phase("Screening") |>
    exclude("Duplicates", n = 300, show_count = FALSE) |>
    exclude("Not in English", n = 50, show_count = FALSE) |>
    exclude("Conference abstracts only", n = 80, show_count = FALSE) |>
    exclude("No full text available", n = 20) |>
    phase("Included") |>
    endpoint("Studies included")

flowsave(flow_nocount, file.path(outdir, "grid_edge_no_count.pdf"))
flowsave(flow_nocount, file.path(outdir, "grid_edge_no_count_cf.pdf"),
            count_first = TRUE)

### ** included_label on every step

flow_inclbl <- enroll(n = 1000, label = "Assessed for eligibility") |>
    phase("Enrollment") |>
    exclude("Did not meet criteria", n = 200,
            included_label = "Eligible participants") |>
    exclude("Declined to participate", n = 100,
            included_label = "Enrolled") |>
    phase("Analysis") |>
    endpoint("Completed study")
flowsave(flow_inclbl, file.path(outdir, "grid_edge_included_labels.pdf"))

### ** Multi-site combine -> allocate

flow_multi <- sources(
    site_a = c("Site A" = 300),
    site_b = c("Site B" = 250),
    site_c = c("Site C" = 200),
    headers = c(site_a = "Site A", site_b = "Site B", site_c = "Site C")) |>
    phase("Enrollment") |>
    combine("Pooled cohort") |>
    exclude("Screening failures", n = 150,
            reasons = c("Lab values" = 80, "ECG abnormal" = 40,
                        "Withdrew consent" = 30)) |>
    phase("Allocation") |>
    allocate(labels = c("Treatment", "Control"), n = c(300, 300)) |>
    phase("Follow-up") |>
    exclude("Discontinued", n = c(15, 12)) |>
    phase("Analysis") |>
    endpoint("Primary analysis")

flowsave(flow_multi, file.path(outdir, "grid_edge_multisite.pdf"))
flowsave(flow_multi, file.path(outdir, "grid_edge_multisite_cf.pdf"),
            count_first = TRUE)

### * Font size variations

### ** Small text (cex = 0.65) — compact diagrams

flowsave(flow_m2, file.path(outdir, "grid_font_2arm_small.pdf"),
            cex = 0.65)
flowsave(flow_m2, file.path(outdir, "grid_font_2arm_small_cf.pdf"),
            cex = 0.65, count_first = TRUE)

### ** Large text (cex = 1.1) — presentation-ready

flowsave(flow_m2, file.path(outdir, "grid_font_2arm_large.pdf"),
            cex = 1.1)

### ** Independent side box font (cex_side = 0.65, main stays default)

flowsave(flow_strobe, file.path(outdir, "grid_font_strobe_smallside.pdf"),
            cex_side = 0.65)

### ** Large phase labels (cex_phase = 1.2)

flowsave(flow_m2, file.path(outdir, "grid_font_2arm_bigphase.pdf"),
            cex_phase = 1.2)

### ** Small phase labels (cex_phase = 0.7)

flowsave(flow_m2, file.path(outdir, "grid_font_2arm_smallphase.pdf"),
            cex_phase = 0.7)

### ** Everything large — poster mode

flowsave(flow_prisma3, file.path(outdir, "grid_font_prisma_poster.pdf"),
            cex = 1.2, cex_side = 1.0, cex_phase = 1.3)

### ** Everything small — supplementary figure

flowsave(flow6, file.path(outdir, "grid_font_6arm_supplement.pdf"),
            cex = 0.6, cex_side = 0.55, cex_phase = 0.65)

### ** STARD with adjusted fonts

flowsave(flow_stard, file.path(outdir, "grid_font_stard_large.pdf"),
            cex = 1.0, cex_side = 0.8)

### * Split-and-Recombine

## A stratify() split closed by combine(), drawn from the documented
## screening-validation, exposure-classification, data-driven, and
## adaptive (split / recombine / re-split) examples.

### ** Screening validation (sublabel on combine)

flow_screening <- enroll(n = 160, label = "High-risk participants") |>
    phase("Enrollment") |>
    exclude("Concurrent enrollment in another study", n = 2,
            included_label = "Total cohort") |>
    phase("Screening Status") |>
    stratify(labels = c("Unscreened", "Screened"),
             n = c(82, 76),
             label = "Annual screening status") |>
    exclude("Without confirmed outcome", n = c(44, 66)) |>
    combine("Outcome cohort",
            sublabel = "Participants with confirmed outcome") |>
    phase("Outcome Verification") |>
    exclude("Without available adjudication", n = 7) |>
    exclude("Without available imaging", n = 23) |>
    endpoint("Participants with available imaging")

flowsave(flow_screening, file.path(outdir, "grid_recombine_screening.pdf"))
flowsave(flow_screening, file.path(outdir, "grid_recombine_screening_cf.pdf"),
            count_first = TRUE)

### ** Per-stratum exclusion reasons (reason list across strata)

flow_exposure <- enroll(n = 5000, label = "Patients in registry") |>
    phase("Enrollment") |>
    exclude("Ineligible", n = 800,
            reasons = c("Age < 18" = 200,
                        "Prior diagnosis" = 350,
                        "Missing baseline data" = 250),
            included_label = "Eligible cohort") |>
    phase("Exposure Classification") |>
    stratify(labels = c("Statin users", "Non-users"),
             n = c(1800, 2400),
             label = "Classified by statin exposure") |>
    exclude("Lost to follow-up", n = c(120, 180),
            reasons = list(
                c("Moved" = 50, "Withdrew consent" = 40, "Deceased" = 30),
                c("Moved" = 80, "Withdrew consent" = 60, "Deceased" = 40)
            )) |>
    combine("Analysis cohort",
            sublabel = "Patients with complete follow-up") |>
    phase("Analysis") |>
    endpoint("Included in primary analysis")

flowsave(flow_exposure, file.path(outdir, "grid_recombine_exposure.pdf"))

### ** Data-driven split-and-recombine

flow_recombine_dd <- enroll(selectaex2, id = "patient_id") |>
    phase("Screening") |>
    exclude("Duplicate records", criterion = is_duplicate == TRUE,
            included_label = "Unique records") |>
    exclude("Failed eligibility", criterion = eligible == FALSE,
            reasons = "exclusion_reason",
            included_label = "Eligible cohort") |>
    phase("Treatment") |>
    stratify("treatment", label = "Treatment assignment") |>
    exclude("Discontinued", criterion = discontinued == TRUE,
            reasons = "discontinuation_reason") |>
    combine("Completers") |>
    phase("Analysis") |>
    endpoint("Analysis cohort")

flowsave(flow_recombine_dd, file.path(outdir, "grid_recombine_datadriven.pdf"))

### ** Adaptive design (split, recombine, then re-split)

flow_resplit <- enroll(n = 2000, label = "Screened") |>
    phase("Screening") |>
    exclude("Ineligible", n = 400,
            reasons = c("No consent" = 180, "Prior treatment" = 120,
                        "ECOG >= 3" = 100)) |>
    phase("Risk Stratification") |>
    stratify(labels = c("High risk", "Low risk"),
             n = c(700, 900),
             label = "Risk classification") |>
    exclude("Declined participation", n = c(50, 80)) |>
    combine("Eligible cohort") |>
    phase("Allocation") |>
    allocate(labels = c("Intervention", "Control"),
             n = c(735, 735)) |>
    phase("Follow-up") |>
    exclude("Lost to follow-up", n = c(30, 35),
            reasons = list(
                c("Withdrew consent" = 18, "Relocated" = 12),
                c("Withdrew consent" = 20, "Relocated" = 15)
            )) |>
    phase("Analysis") |>
    endpoint("Analyzed")

flowsave(flow_resplit, file.path(outdir, "grid_recombine_resplit.pdf"))

### ** Recombination after a second split (manual)

## Two stratify / recombine cycles in series on a single trunk, each split
## closed by combine(). Exercises arm-id reuse across the second-degree split:
## the second split must reuse the first split's central columns rather than
## splaying further sideways. Grid only: under DOT's ortho router this
## particular wide-label combination drifts the middle trunk segment a fraction
## of an inch sideways (the merge box has no anchor below it, so its converge
## center follows the floating segment rather than the arm midpoint). The
## topology is sound -- the narrower data-driven case below renders straight on
## both engines -- so the aberrant DOT render is left out rather than forcing
## the trunk and distorting the fan.

flow_double_recombine <-
    enroll(n = 1800, label = "Assessed for eligibility") |>
    phase("Screening") |>
    exclude("Did not meet criteria", n = 300) |>
    phase("Risk Stratification") |>
    stratify(labels = c("Biomarker positive", "Biomarker negative"),
             n = c(620, 880),
             label = "Biomarker status") |>
    exclude("Withdrew before allocation", n = c(20, 30)) |>
    combine("Allocated cohort") |>
    phase("Dose Allocation") |>
    stratify(labels = c("Standard dose", "High dose"),
             n = c(725, 725),
             label = "Dose assignment") |>
    exclude("Did not receive assigned dose", n = c(15, 18)) |>
    combine("Treated cohort") |>
    phase("Analysis") |>
    endpoint("Analyzed")

flowsave(flow_double_recombine, file.path(outdir, "grid_recombine_double.pdf"))
flowsave(flow_double_recombine, file.path(outdir, "grid_recombine_double_cf.pdf"),
            count_first = TRUE)

### ** Recombination after a second split (data-driven)

## The data-driven counterpart: two categorical variables stratified in
## series, each split closed by combine(), with all arm counts derived from
## the data. Confirms arm-id reuse and column reuse hold in data mode.

df_resplit <- data.frame(
    id           = seq_len(1000),
    ineligible   = rep(c(TRUE, FALSE), c(160, 840)),
    risk_group   = NA_character_,
    treatment    = NA_character_,
    withdrew     = FALSE,
    discontinued = FALSE,
    stringsAsFactors = FALSE)
eligible_rows <- 161:1000
df_resplit$risk_group[eligible_rows] <- rep(c("High", "Low"), c(380, 460))
df_resplit$treatment[eligible_rows]  <- rep(c("Active", "Placebo"), c(420, 420))
df_resplit$withdrew[c(161:185, 541:575)]     <- TRUE
df_resplit$discontinued[c(186:204, 576:598)] <- TRUE

flow_resplit_dd <- enroll(df_resplit, id = "id", label = "Records screened") |>
    phase("Eligibility") |>
    exclude("Ineligible", criterion = ineligible == TRUE,
            included_label = "Eligible cohort") |>
    phase("Risk Stratification") |>
    stratify("risk_group", label = "Baseline risk") |>
    exclude("Withdrew consent", criterion = withdrew == TRUE) |>
    combine("Randomization set") |>
    phase("Treatment Allocation") |>
    stratify("treatment", label = "Treatment assignment") |>
    exclude("Discontinued treatment", criterion = discontinued == TRUE) |>
    combine("Per-protocol set") |>
    phase("Analysis") |>
    endpoint("Analyzed")

flowsave(flow_resplit_dd, file.path(outdir, "grid_recombine_resplit_dd.pdf"))

### * Factorial (two-level) splits

## Two-level (factorial) randomization: a first split into parent arms, each
## then split again into sub-arms, with per-sub-arm exclusions and endpoints.
## Exercises the grid engine's factorial layout (level-2 sub-arms as the leaf
## columns, each parent centered over its own children, the trunk centered over
## all leaves) alongside the DOT engine's factorial rendering. Sub-arm counts
## are parent-major: the n vector runs [parent-1 sub-1, parent-1 sub-2,
## parent-2 sub-1, parent-2 sub-2].

flow_factorial <- enroll(n = 480, label = "Randomized") |>
    allocate(labels = c("Drug A", "Drug B"), n = c(240, 240),
             label = "Antiviral assignment") |>
    allocate(labels = c("Vaccine", "Placebo"),
             n = c(120, 120, 120, 120)) |>
    exclude("Discontinued before dosing", n = c(8, 6, 7, 9)) |>
    endpoint("Primary analysis")

flowsave(flow_factorial, file.path(outdir, "grid_factorial.pdf"))
flowsave(flow_factorial, file.path(outdir, "grid_factorial_cf.pdf"),
            count_first = TRUE)

## Factorial split closed by a combine that peels the inner level: each
## parent's sub-arms converge back into one box per parent, then a per-parent
## endpoint. Exercises the factorial converge -- one merge box per parent, each
## centered over its own sub-arms -- on both engines.

flow_factorial_combine <- enroll(n = 600, label = "Randomized") |>
    allocate(labels = c("Surgery", "Medical therapy"), n = c(300, 300),
             label = "Treatment strategy") |>
    allocate(labels = c("High dose", "Low dose"),
             n = c(150, 150, 150, 150)) |>
    exclude("Did not start treatment", n = c(5, 7, 6, 4)) |>
    combine("Treated per strategy") |>
    endpoint("Analyzed")

flowsave(flow_factorial_combine,
            file.path(outdir, "grid_factorial_combine.pdf"))

## ---- Data-mode factorial (counts derived from two crossed variables) ----

## A balanced 2 x 2: antiviral {Drug A, Drug B} crossed with adjunct {Vaccine,
## Placebo}, 200 per cell, with a scattered discontinuation flag feeding a
## per-leaf exclusion. Grid layout and DOT emission must match the manual 2 x 2.
df_factorial <- data.frame(
    id           = seq_len(800),
    antiviral    = rep(c("Drug A", "Drug B"), each = 400),
    adjunct      = rep(c("Vaccine", "Placebo"), times = 400),
    discontinued = FALSE,
    stringsAsFactors = FALSE)
df_factorial$discontinued[seq(1, 800, by = 25)] <- TRUE

flow_factorial_dd <- enroll(df_factorial, id = "id", label = "Randomized") |>
    allocate("antiviral", label = "Antiviral assignment") |>
    allocate("adjunct") |>
    exclude("Discontinued before dosing", criterion = discontinued == TRUE) |>
    endpoint("Primary analysis")

flowsave(flow_factorial_dd, file.path(outdir, "grid_factorial_dd.pdf"))

## ---- Second-degree 3-arm and asymmetric splits ----

## Three-by-three plus the two asymmetric shapes. These are geometrically
## limited -- nine (or six) leaf columns leave little room -- so the goal is
## only to confirm they lay out and render at all, on both engines.

flow_factorial_3x3 <- enroll(n = 900, label = "Randomized") |>
    allocate(labels = c("Low", "Mid", "High"), n = c(300, 300, 300),
             label = "Dose tier") |>
    allocate(labels = c("Schedule A", "Schedule B", "Schedule C"),
             n = rep(100L, 9L)) |>
    endpoint("Analyzed")

flowsave(flow_factorial_3x3, file.path(outdir, "grid_factorial_3x3.pdf"))

flow_factorial_3x3_excl <- enroll(n = 900, label = "Randomized") |>
    allocate(labels = c("Low", "Mid", "High"), n = c(300, 300, 300),
             label = "Dose tier") |>
    allocate(labels = c("Schedule A", "Schedule B", "Schedule C"),
             n = rep(100L, 9L)) |>
    exclude("Withdrew before dosing", n = rep(5L, 9L)) |>
    endpoint("Analyzed")

flowsave(flow_factorial_3x3_excl,
            file.path(outdir, "grid_factorial_3x3_excl.pdf"))

flow_factorial_2x3 <- enroll(n = 600, label = "Randomized") |>
    allocate(labels = c("Active", "Control"), n = c(300, 300),
             label = "Treatment") |>
    allocate(labels = c("Dose 1", "Dose 2", "Dose 3"),
             n = rep(100L, 6L)) |>
    endpoint("Analyzed")

flowsave(flow_factorial_2x3, file.path(outdir, "grid_factorial_2x3.pdf"))

flow_factorial_3x2 <- enroll(n = 600, label = "Randomized") |>
    allocate(labels = c("Site 1", "Site 2", "Site 3"), n = c(200, 200, 200),
             label = "Center") |>
    allocate(labels = c("Drug", "Placebo"),
             n = rep(100L, 6L)) |>
    endpoint("Analyzed")

flowsave(flow_factorial_3x2, file.path(outdir, "grid_factorial_3x2.pdf"))

## ---- Double recombination down to a single analysis cohort ----

## A factorial split, a combine that peels the inner level back to the parent
## arms, then a second combine that pools the arms into one trunk cohort.
## Manual and data-driven, each closed by a single endpoint.

flow_factorial_double <- enroll(n = 600, label = "Randomized") |>
    allocate(labels = c("Surgery", "Medical therapy"), n = c(300, 300),
             label = "Treatment strategy") |>
    allocate(labels = c("High dose", "Low dose"),
             n = c(150, 150, 150, 150)) |>
    exclude("Did not start treatment", n = c(5, 7, 6, 4)) |>
    combine("Treated per strategy") |>
    combine("Pooled analysis cohort") |>
    endpoint("Analyzed")

flowsave(flow_factorial_double, file.path(outdir, "grid_factorial_double.pdf"))

flow_factorial_double_dd <-
    enroll(df_factorial, id = "id", label = "Randomized") |>
    allocate("antiviral", label = "Antiviral assignment") |>
    allocate("adjunct") |>
    exclude("Discontinued before dosing", criterion = discontinued == TRUE) |>
    combine("Per-antiviral cohort") |>
    combine("Pooled analysis cohort") |>
    endpoint("Analyzed")

flowsave(flow_factorial_double_dd,
            file.path(outdir, "grid_factorial_double_dd.pdf"))

### * Split Endpoints (terminal group split)

## A terminal endpoint that fans from a shared distributor into one separate
## box per study group. Manual (groups + n) and data-driven (variable =)
## modes, with two and three groups, plain and phased.

flow_split2 <- enroll(n = 480, label = "Randomized participants") |>
    phase("Enrollment") |>
    exclude("Did not complete baseline", n = 30,
            included_label = "Baseline complete") |>
    phase("Analysis populations") |>
    endpoint("Assigned to analysis set",
             groups = c("Per-protocol", "Intention-to-treat"),
             n = c(210, 240))

flowsave(flow_split2, file.path(outdir, "grid_split_2group.pdf"))
flowsave(flow_split2, file.path(outdir, "grid_split_2group_cf.pdf"),
            count_first = TRUE)

flow_split3 <- enroll(n = 600, label = "Enrolled participants") |>
    phase("Allocation") |>
    endpoint("Allocated to study group",
             groups = c("Low dose", "High dose", "Placebo"),
             n = c(200, 200, 200))

flowsave(flow_split3, file.path(outdir, "grid_split_3group.pdf"))

df_split <- data.frame(
    id          = seq_len(300),
    final_group = rep(c("Per protocol", "Modified ITT", "Safety only"),
                      c(120, 110, 70)),
    stringsAsFactors = FALSE)

flow_split_dd <- enroll(df_split, id = "id", label = "Randomized cohort") |>
    phase("Analysis populations") |>
    endpoint("Analysis populations", variable = "final_group")

flowsave(flow_split_dd, file.path(outdir, "grid_split_datadriven.pdf"))

### * Two-Level Exclusion Reasons and collapse_singletons

## Nested reason -> sub-reason breakdowns inside an exclusion box, as a manual
## nested list and as a data-driven two-column cross-tabulation, plus
## collapse_singletons which folds a single-child parent back to a plain leaf.

flow_nested <- enroll(n = 2000, label = "Patients screened") |>
    phase("Eligibility") |>
    exclude("Excluded", n = 520,
            reasons = list(
                "Did not meet inclusion criteria" = c(
                    "Age outside range"             = 180,
                    "Insufficient disease severity" = 140,
                    "Comorbid condition"            = 90),
                "Declined to participate" = 70,
                "Other" = c("Logistical barriers"     = 25,
                            "Investigator discretion" = 15)),
            included_label = "Eligible cohort") |>
    phase("Allocation") |>
    allocate(labels = c("Intervention", "Control"), n = c(740, 740)) |>
    phase("Analysis") |>
    endpoint("Analyzed")

flowsave(flow_nested, file.path(outdir, "grid_reasons_nested.pdf"))
flowsave(flow_nested, file.path(outdir, "grid_reasons_nested_cf.pdf"),
            count_first = TRUE)

## Data-driven: a reason column cross-tabulated against a sub-reason column.
df_nested <- data.frame(
    id        = seq_len(800),
    excluded  = c(rep(TRUE, 240), rep(FALSE, 560)),
    reason    = NA_character_,
    subreason = NA_character_,
    stringsAsFactors = FALSE)
df_nested$reason[1:240]      <- rep(c("Ineligible", "Declined", "Other"),
                                    c(140, 60, 40))
df_nested$subreason[1:80]    <- "Age outside range"
df_nested$subreason[81:140]  <- "Disease severity"
df_nested$subreason[141:200] <- "Time commitment"
df_nested$subreason[201:225] <- "Logistical barrier"
df_nested$subreason[226:240] <- "Investigator discretion"

flow_nested_dd <- enroll(df_nested, id = "id", label = "Records assessed") |>
    phase("Eligibility") |>
    exclude("Excluded", criterion = excluded == TRUE,
            reasons = c("reason", "subreason"),
            included_label = "Eligible records") |>
    phase("Analysis") |>
    endpoint("Analytic cohort")

flowsave(flow_nested_dd, file.path(outdir, "grid_reasons_nested_dd.pdf"))

## collapse_singletons: single-child parents (Declined, Administrative) fold
## to plain leaves while the multi-child parent stays expanded.
flow_collapse <- enroll(n = 2000, label = "Patients screened") |>
    phase("Eligibility") |>
    exclude("Excluded", n = 520,
            reasons = list(
                "Did not meet inclusion criteria" = c(
                    "Age outside range"             = 180,
                    "Insufficient disease severity" = 140,
                    "Comorbid condition"            = 90),
                "Declined to participate" = c("Time commitment"    = 70),
                "Administrative"          = c("Records incomplete" = 40)),
            collapse_singletons = TRUE,
            included_label = "Eligible cohort") |>
    phase("Analysis") |>
    endpoint("Analyzed")

flowsave(flow_collapse, file.path(outdir, "grid_reasons_collapse.pdf"))

### * Color Schemes

## The same manual CONSORT flow rendered under three distinct palettes via
## the six color parameters (box_fill, side_fill, border_col, arrow_col,
## phase_fill, phase_text_col).

flow_palette <- enroll(n = 1200, label = "Assessed for eligibility") |>
    phase("Enrollment") |>
    exclude("Excluded", n = 300,
            reasons = c("Not meeting criteria" = 160,
                        "Declined to participate" = 90,
                        "Other reasons" = 50),
            included_label = "Randomized") |>
    phase("Allocation") |>
    allocate(labels = c("Intervention", "Control"), n = c(450, 450)) |>
    phase("Follow-up") |>
    exclude("Lost to follow-up", n = c(18, 22),
            reasons = list(
                c("Withdrew consent" = 10, "Adverse event" = 8),
                c("Withdrew consent" = 14, "Relocated" = 8)
            )) |>
    phase("Analysis") |>
    endpoint("Analyzed")

### ** Blue palette

flowsave(flow_palette, file.path(outdir, "grid_color_blue.pdf"),
            box_fill       = "#f0f5ff",
            side_fill      = "#e8eef9",
            border_col     = "#1a365d",
            arrow_col      = "#2c5282",
            phase_fill     = "#2c5282",
            phase_text_col = "#ffffff")

### ** Warm (terracotta) palette

flowsave(flow_palette, file.path(outdir, "grid_color_warm.pdf"),
            box_fill       = "#fff7f0",
            side_fill      = "#fde8d8",
            border_col     = "#7c2d12",
            arrow_col      = "#c2410c",
            phase_fill     = "#9a3412",
            phase_text_col = "#ffffff")

### ** Green palette

flowsave(flow_palette, file.path(outdir, "grid_color_green.pdf"),
            box_fill       = "#f0fdf4",
            side_fill      = "#dcfce7",
            border_col     = "#14532d",
            arrow_col      = "#16a34a",
            phase_fill     = "#166534",
            phase_text_col = "#ffffff")

### * Font Families

## A single flow rendered in the three generic families plus a named
## serif face. Box dimensions are recomputed from each font's metrics.

flow_fonts <- enroll(n = 8400, label = "Biobank participants") |>
    phase("Eligibility") |>
    exclude("Ineligible for analysis", n = 1560,
            reasons = c("Insufficient sample volume" = 720,
                        "Missing consent records" = 540,
                        "Genotyping failure" = 300),
            included_label = "Analyzable samples") |>
    phase("Quality control") |>
    exclude("Failed quality control", n = 340,
            reasons = c("Call rate below threshold" = 190,
                        "Sex mismatch" = 90,
                        "Cryptic relatedness" = 60),
            included_label = "Passed quality control") |>
    phase("Analysis") |>
    endpoint("Analytic cohort")

flowsave(flow_fonts, file.path(outdir, "grid_font_sans.pdf"),  font_family = "sans")
flowsave(flow_fonts, file.path(outdir, "grid_font_serif.pdf"), font_family = "serif")
flowsave(flow_fonts, file.path(outdir, "grid_font_mono.pdf"),  font_family = "mono")
flowsave(flow_fonts, file.path(outdir, "grid_font_times.pdf"), font_family = "Times")

## Serif paired with European number formatting and a large count
flow_fonts_eu <- enroll(n = 1284500, label = "Invited to screening") |>
    phase("Participation") |>
    exclude("Did not attend", n = 458200,
            included_label = "Attended screening") |>
    exclude("Inadequate sample", n = 32600,
            reasons = c("Sample insufficient" = 21400,
                        "Technical failure" = 11200),
            included_label = "Valid result") |>
    phase("Outcome") |>
    endpoint("Entered surveillance")

flowsave(flow_fonts_eu, file.path(outdir, "grid_font_serif_eu.pdf"),
            font_family = "serif", number_format = "eu")

### * Multi-Line Phase Labels

## Long descriptive phase labels wrap to fit their band (the default), and
## an explicit "\n" forces a break wherever placed.

flow_multiline <- enroll(n = 5200, label = "Cohort members") |>
    phase("Baseline enrollment and exposure assessment") |>
    exclude("Excluded at baseline", n = 700,
            reasons = c("Incomplete exposure history" = 360,
                        "Prevalent disease" = 220,
                        "Missing covariates" = 120),
            included_label = "Eligible for follow-up") |>
    phase("Stratification by exposure tertile") |>
    stratify(labels = c("Low exposure", "Moderate exposure",
                        "High exposure"),
             n = c(1500, 1500, 1500),
             label = "Exposure tertile") |>
    phase("Longitudinal outcome ascertainment") |>
    exclude("Lost to follow-up", n = c(120, 140, 160)) |>
    phase("Analysis") |>
    endpoint("Analytic sample")

flowsave(flow_multiline, file.path(outdir, "grid_multiline_wrapped.pdf"))
flowsave(flow_multiline, file.path(outdir, "grid_multiline_nowrap.pdf"),
            phase_multiline = FALSE)

flow_multiline_explicit <- enroll(n = 1200,
                                  label = "Assessed for eligibility") |>
    phase("Enrollment and\nbaseline assessment") |>
    exclude("Excluded", n = 300, included_label = "Eligible cohort") |>
    phase("Allocation") |>
    allocate(labels = c("Drug A", "Placebo"), n = c(450, 450)) |>
    phase("Analysis") |>
    endpoint("Analyzed")

flowsave(flow_multiline_explicit,
            file.path(outdir, "grid_multiline_explicit.pdf"))

### * DOT / Graphviz Output

## The DOT engine returns a Graphviz DOT string. Rendering it to an image
## requires the system `dot` binary; when it is unavailable the DOT strings
## are written to .dot files instead, so this section never fails on a
## machine without Graphviz installed. Every flow rendered above through the
## grid engine is rendered here through the DOT engine too, so the two
## outputs can be compared diagram for diagram.

.dot_available <- nzchar(Sys.which("dot"))

render_dot_file <- function(dot_str, stem, fmt = "pdf", dpi = 150) {
    dot_in <- file.path(outdir, paste0(stem, ".dot"))
    writeLines(dot_str, dot_in)
    if (.dot_available) {
        out  <- file.path(outdir, paste0(stem, ".", fmt))
        args <- c(paste0("-T", fmt))
        if (fmt == "png") args <- c(args, paste0("-Gdpi=", dpi))
        args <- c(args, shQuote(dot_in), "-o", shQuote(out))
        system2("dot", args, stdout = NULL, stderr = NULL)
        return(out)
    }
    dot_in
}

## Defensive wrapper: a DOT-generation error on one flow is logged and
## skipped rather than aborting the whole regression, so every other diagram
## still renders and the gap is visible (and caught by the assertions below).
render_dot <- function(flow, stem, ...) {
    if (!.render) return(invisible(NULL))
    dot <- tryCatch(flowchart(flow, engine = "dot", ...),
                    error = function(e) {
                        cat(sprintf("  [DOT FAILED] %s: %s\n",
                                    stem, conditionMessage(e)))
                        NULL
                    })
    if (is.null(dot)) return(invisible(NULL))
    render_dot_file(dot, stem)
}

## ---- Core gallery: every single-stream diagram type through DOT ----

dot_core <- list(
    ## CONSORT / STROBE, data-driven and manual, 0/2/3/6 arms
    dd_0arm           = flow0,
    dd_2arm           = flow2,
    dd_3arm           = flow3,
    dd_6arm           = flow6,
    man_2arm          = flow_m2,
    man_3arm          = flow_m3,
    man_6arm          = flow_m6,
    strobe            = flow_strobe,
    ## PRISMA / MOOSE (source convergence)
    prisma_3col       = flow_prisma3,
    prisma_2col       = flow_prisma2,
    prisma_1col       = flow_prisma1,
    moose             = flow_moose,
    ## STARD (diagnostic, per-arm endpoint breakdown)
    stard             = flow_stard,
    ## Split endpoints (terminal group split)
    split_2group      = flow_split2,
    split_3group      = flow_split3,
    split_datadriven  = flow_split_dd,
    ## Two-level reasons and collapse_singletons
    reasons_nested    = flow_nested,
    reasons_nested_dd = flow_nested_dd,
    reasons_collapse  = flow_collapse,
    ## Edge cases
    edge_minimal      = flow_min,
    edge_zero         = flow_zero,
    edge_nocount      = flow_nocount,
    edge_included     = flow_inclbl,
    edge_multisite    = flow_multi)

for (nm in names(dot_core))
    render_dot(dot_core[[nm]], paste0("dot_", nm))

## ---- Split-and-recombine through DOT (the most demanding topology) ----

dot_recombine <- list(
    recombine_screening  = flow_screening,
    recombine_exposure   = flow_exposure,
    recombine_datadriven = flow_recombine_dd,
    recombine_resplit    = flow_resplit,
    recombine_resplit_dd = flow_resplit_dd)

for (nm in names(dot_recombine))
    render_dot(dot_recombine[[nm]], paste0("dot_", nm))

## ---- Factorial (two-level) DOT renders ----

dot_factorial <- list(
    factorial           = flow_factorial,
    factorial_combine   = flow_factorial_combine,
    factorial_dd        = flow_factorial_dd,
    factorial_3x3       = flow_factorial_3x3,
    factorial_3x3_excl  = flow_factorial_3x3_excl,
    factorial_2x3       = flow_factorial_2x3,
    factorial_3x2       = flow_factorial_3x2,
    factorial_double    = flow_factorial_double,
    factorial_double_dd = flow_factorial_double_dd)

for (nm in names(dot_factorial))
    render_dot(dot_factorial[[nm]], paste0("dot_", nm))

## ---- DOT-specific styling variants ----

## Plain vs rich (HTML-like) labels, orthogonal vs spline routing, count-first
## boxes, a named Graphviz font face, and European number grouping.
render_dot(flow_m2,        "dot_style_plain")
render_dot(flow_m2,        "dot_style_rich",         formatting    = "rich")
render_dot(flow_m2,        "dot_style_ortho",        ortho         = TRUE)
render_dot(flow_m2,        "dot_style_countfirst",   count_first   = TRUE)
render_dot(flow_m2,        "dot_style_times",        font_family   = "Times-Roman")
render_dot(flow_prisma3,   "dot_style_prisma_rich",  formatting    = "rich")
render_dot(flow_prisma3,   "dot_style_prisma_ortho", ortho         = TRUE)
render_dot(flow_stard,     "dot_style_stard_rich",   formatting    = "rich")
render_dot(flow_fonts_eu,  "dot_style_eu",           number_format = "eu")

if (.dot_available) {
    cat("\nGraphviz 'dot' found: DOT diagrams rendered to PDF.\n")
} else {
    cat("\nGraphviz 'dot' not found: DOT strings written as .dot files;",
        "install Graphviz to rasterize them.\n")
}

### * print() verification


cat("\n== print() output ==\n\n")
cat("--- 0-arm data-driven ---\n");  print(flow0)
cat("\n--- 2-arm data-driven ---\n"); print(flow2)
cat("\n--- 3-arm data-driven ---\n"); print(flow3)
cat("\n--- 6-arm data-driven ---\n"); print(flow6)
cat("\n--- STROBE per-arm ---\n");    print(flow_strobe)
cat("\n--- PRISMA 3-col ---\n");      print(flow_prisma3)
cat("\n--- STARD ---\n");             print(flow_stard)
cat("\n--- split endpoint ---\n");    print(flow_split3)
cat("\n--- nested reasons ---\n");    print(flow_nested)
cat("\n--- show_count=FALSE ---\n");  print(flow_nocount)

### * Summary

files <- list.files(outdir, pattern = "\\.pdf$")
dots  <- list.files(outdir, pattern = "\\.dot$")
cat(sprintf("\n== COMPLETE: %d PDFs and %d DOT sources in '%s/' ==\n",
            length(files), length(dots), outdir))
cat(paste(" ", files, collapse = "\n"), "\n")


### * Assertions

## Register expectations so the rendering pass counts as a real test rather
## than an empty one. Grid is the reference engine and must render every
## variant. The DOT engine must render the full core gallery (single-stream
## diagrams) and every styling variant; split-and-recombine is asserted
## separately because it is the most demanding topology for the orthogonal
## router, so a gap there is reported on its own rather than masking the rest.
## When Graphviz is absent the DOT checks fall back to the always-written
## .dot source files.

dot_ext <- if (nzchar(Sys.which("dot"))) "pdf" else "dot"

expect_outputs <- function(stems) {
    produced <- file.path(outdir, paste0(stems, ".", dot_ext))
    missing  <- produced[!file.exists(produced)]
    expect_true(length(missing) == 0,
                info = paste("Missing DOT output:",
                             paste(basename(missing), collapse = ", ")))
    present <- produced[file.exists(produced)]
    expect_true(length(present) > 0)
    expect_true(all(file.size(present) > 0))
}

test_that("all grid diagram variants render to non-empty files", {
    skip_on_cran()
    pdfs <- list.files(outdir, pattern = "\\.pdf$", full.names = TRUE)
    expect_true(length(pdfs) > 0)
    expect_true(all(file.size(pdfs) > 0))
})

test_that("the DOT engine renders the full core gallery", {
    skip_on_cran()
    expect_outputs(paste0("dot_", names(dot_core)))
})

test_that("the DOT engine renders split-and-recombine diagrams", {
    skip_on_cran()
    expect_outputs(paste0("dot_", names(dot_recombine)))
})

test_that("the DOT engine renders factorial (two-level) diagrams", {
    skip_on_cran()
    expect_outputs(paste0("dot_", names(dot_factorial)))
})

test_that("factorial (two-level) diagrams render to non-empty grid files", {
    skip_on_cran()
    ## Grid is the reference engine; every factorial shape -- 2 x 2 (manual and
    ## data-driven), the geometrically limited 3-arm and asymmetric splits, the
    ## single combine peel, and the double recombine to one cohort -- must
    ## produce a file by name. The glob check above only verifies that whatever
    ## PDFs exist are non-empty, not that these specific ones were produced.
    stems <- c("factorial", "factorial_cf", "factorial_combine",
               "factorial_dd", "factorial_3x3", "factorial_3x3_excl",
               "factorial_2x3", "factorial_3x2",
               "factorial_double", "factorial_double_dd")
    pdfs <- file.path(outdir, paste0("grid_", stems, ".pdf"))
    expect_true(all(file.exists(pdfs)),
                info = paste("Missing factorial grid PDF:",
                             paste(basename(pdfs[!file.exists(pdfs)]),
                                   collapse = ", ")))
    expect_true(all(file.size(pdfs[file.exists(pdfs)]) > 0))
})

test_that("DOT styling variants render to non-empty files", {
    skip_on_cran()
    expect_outputs(c("dot_style_plain", "dot_style_rich", "dot_style_ortho",
                     "dot_style_countfirst", "dot_style_times",
                     "dot_style_prisma_rich", "dot_style_prisma_ortho",
                     "dot_style_stard_rich", "dot_style_eu"))
})

test_that("grid and DOT engines cover the same core diagram set", {
    skip_on_cran()
    ## Each core flow produced a grid PDF and a DOT output, so the two engines
    ## are exercised on an identical breadth of diagram types.
    grid_stems <- c("dd_0arm", "dd_2arm", "dd_3arm", "dd_6arm",
                    "man_2arm", "man_3arm", "man_6arm", "man_strobe",
                    "prisma_3col", "prisma_2col", "prisma_1col", "moose",
                    "stard", "split_2group", "split_3group",
                    "split_datadriven", "reasons_nested", "reasons_nested_dd",
                    "reasons_collapse", "edge_minimal", "edge_zero_count",
                    "edge_no_count", "edge_included_labels", "edge_multisite")
    grid_pdfs <- file.path(outdir, paste0("grid_", grid_stems, ".pdf"))
    expect_true(all(file.exists(grid_pdfs)),
                info = paste("Missing grid PDF:",
                             paste(basename(grid_pdfs[!file.exists(grid_pdfs)]),
                                   collapse = ", ")))
    expect_length(dot_core, length(grid_stems))
})
