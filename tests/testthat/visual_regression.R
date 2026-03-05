library(selecta)

outdir <- "test_output"
dir.create(outdir, showWarnings = FALSE)

## ============================================================================
## * CONSORT / STROBE — Data-driven (package datasets)
## ============================================================================

## ** 0-arm (observational, no stratification)

data(rctselect0)

flow0 <- enroll(rctselect0, id = "patient_id") |>
    phase("Screening") |>
    exclude("Duplicate records", expr = is_duplicate == TRUE,
            included_label = "Unique records") |>
    exclude("Failed eligibility",
            expr = eligible == FALSE & is_duplicate == FALSE,
            reasons = "exclusion_reason",
            included_label = "Eligible cohort") |>
    phase("Follow-up") |>
    exclude("Lost to follow-up",
            expr = lost_to_followup == TRUE,
            reasons = "followup_loss_reason") |>
    phase("Study") |>
    endpoint("Analysis cohort")

autoflow(flow0, file.path(outdir, "dd_0arm.pdf"))
autoflow(flow0, file.path(outdir, "dd_0arm_cf.pdf"), count_first = TRUE)

## ** 2-arm (CONSORT RCT)

data(rctselect2)

flow2 <- enroll(rctselect2, id = "patient_id") |>
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

autoflow(flow2, file.path(outdir, "dd_2arm.pdf"))
autoflow(flow2, file.path(outdir, "dd_2arm_cf.pdf"), count_first = TRUE)

## ** 3-arm (CONSORT RCT)

data(rctselect3)

flow3 <- enroll(rctselect3, id = "patient_id") |>
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

autoflow(flow3, file.path(outdir, "dd_3arm.pdf"))
autoflow(flow3, file.path(outdir, "dd_3arm_cf.pdf"), count_first = TRUE)

## ** 6-arm (dose-finding, uses stratify)

data(rctselect6)

flow6 <- enroll(rctselect6, id = "patient_id") |>
    phase("Screening") |>
    exclude("Duplicate records", expr = is_duplicate == TRUE,
            included_label = "Unique records") |>
    exclude("Failed eligibility", expr = eligible == FALSE,
            reasons = "exclusion_reason",
            included_label = "Eligible cohort") |>
    phase("Allocation") |>
    stratify("treatment") |>
    phase("Follow-up") |>
    exclude("Discontinued", expr = discontinued == TRUE,
            reasons = "discontinuation_reason") |>
    phase("Study") |>
    endpoint("Analysis cohort")

autoflow(flow6, file.path(outdir, "dd_6arm.pdf"))
autoflow(flow6, file.path(outdir, "dd_6arm_cf.pdf"), count_first = TRUE)

## ** cohort() extraction

final  <- cohort(flow2)
by_arm <- cohort(flow2, split = TRUE)
arm1   <- cohort(flow2, arm = names(by_arm)[1])
stopifnot(is.data.frame(final))
stopifnot(is.list(by_arm), length(by_arm) == 2)
stopifnot(nrow(final) == sum(vapply(by_arm, nrow, integer(1))))
cat("cohort() extraction: OK\n")

## ============================================================================
## * CONSORT / STROBE — Manual
## ============================================================================

## ** 2-arm manual

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
    endpoint("Analysed")

autoflow(flow_m2, file.path(outdir, "man_2arm.pdf"))
autoflow(flow_m2, file.path(outdir, "man_2arm_cf.pdf"), count_first = TRUE)

## ** 3-arm manual

flow_m3 <- enroll(n = 900, label = "Screened") |>
    phase("Enrollment") |>
    exclude("Excluded", n = 150) |>
    phase("Allocation") |>
    allocate(labels = c("Drug A", "Drug B", "Placebo"),
             n = c(250, 250, 250)) |>
    phase("Follow-up") |>
    exclude("Discontinued", n = c(10, 15, 8), show_count = TRUE) |>
    phase("Analysis") |>
    endpoint("Analysed")

autoflow(flow_m3, file.path(outdir, "man_3arm.pdf"))
autoflow(flow_m3, file.path(outdir, "man_3arm_cf.pdf"), count_first = TRUE)

## ** 6-arm manual (stress test)

flow_m6 <- enroll(n = 1800, label = "Screened") |>
    phase("Enrollment") |>
    exclude("Excluded", n = 300) |>
    phase("Allocation") |>
    allocate(labels = paste("Arm", LETTERS[1:6]),
             n = rep(250, 6)) |>
    phase("Follow-up") |>
    exclude("Lost", n = c(10, 12, 8, 15, 11, 9), show_count = TRUE) |>
    phase("Analysis") |>
    endpoint("Analysed")

autoflow(flow_m6, file.path(outdir, "man_6arm.pdf"))

## ** STROBE per-arm labels

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

autoflow(flow_strobe, file.path(outdir, "man_strobe.pdf"))
autoflow(flow_strobe, file.path(outdir, "man_strobe_cf.pdf"),
            count_first = TRUE)

## ============================================================================
## * PRISMA 2020
## ============================================================================

## ** 3-column (previous + databases + other)

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

autoflow(flow_prisma3, file.path(outdir, "prisma_3col.pdf"))
autoflow(flow_prisma3, file.path(outdir, "prisma_3col_cf.pdf"),
            count_first = TRUE)

## ** 2-column (databases + other)

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

autoflow(flow_prisma2, file.path(outdir, "prisma_2col.pdf"))
autoflow(flow_prisma2, file.path(outdir, "prisma_2col_cf.pdf"),
            count_first = TRUE)

## ** 1-column (flat sources, no grouping)

flow_prisma1 <- sources(PubMed = 1234, Embase = 567, CENTRAL = 89) |>
    phase("Identification") |>
    combine("Records identified") |>
    exclude("Duplicates removed", n = 340,
            included_label = "Records screened") |>
    phase("Screening") |>
    exclude("Records excluded", n = 800) |>
    phase("Included") |>
    endpoint("Studies included")

autoflow(flow_prisma1, file.path(outdir, "prisma_1col.pdf"))
autoflow(flow_prisma1, file.path(outdir, "prisma_1col_cf.pdf"),
            count_first = TRUE)

## ** MOOSE (grey literature variant)

flow_moose <- sources(
    databases = c("MEDLINE" = 2500, "Embase" = 1800, "PsycINFO" = 400),
    grey      = c("Dissertations" = 45, "Conference proceedings" = 120),
    headers   = c(databases = "Electronic databases",
                  grey      = "Grey literature")) |>
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

autoflow(flow_moose, file.path(outdir, "moose.pdf"))
autoflow(flow_moose, file.path(outdir, "moose_cf.pdf"), count_first = TRUE)

## ============================================================================
## * STARD 2015
## ============================================================================

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
             reasons = list(c("Target condition +" = 160, "Target condition -" = 20),
                            c("Target condition +" = 15, "Target condition -" = 225)))

autoflow(flow_stard, file.path(outdir, "stard.pdf"))
autoflow(flow_stard, file.path(outdir, "stard_cf.pdf"), count_first = TRUE)

## ============================================================================
## * Edge cases
## ============================================================================

## ** Minimal (enroll + endpoint only)

flow_min <- enroll(n = 100) |> endpoint("Final")
autoflow(flow_min, file.path(outdir, "edge_minimal.pdf"))

## ** Zero-count exclusion

flow_zero <- enroll(n = 500, label = "Screened") |>
    exclude("Protocol violation", n = 0, show_zero = TRUE, show_count = TRUE) |>
    exclude("Adverse event", n = 10, show_count = TRUE) |>
    endpoint("Analysed")
autoflow(flow_zero, file.path(outdir, "edge_zero_count.pdf"))

## ** show_count=FALSE stacking (4 consecutive suppressed)

flow_nocount <- enroll(n = 2000, label = "Records identified") |>
    phase("Screening") |>
    exclude("Duplicates", n = 300, show_count = FALSE) |>
    exclude("Not in English", n = 50, show_count = FALSE) |>
    exclude("Conference abstracts only", n = 80, show_count = FALSE) |>
    exclude("No full text available", n = 20) |>
    phase("Included") |>
    endpoint("Studies included")

autoflow(flow_nocount, file.path(outdir, "edge_no_count.pdf"))
autoflow(flow_nocount, file.path(outdir, "edge_no_count_cf.pdf"),
            count_first = TRUE)

## ** included_label on every step

flow_inclbl <- enroll(n = 1000, label = "Assessed for eligibility") |>
    phase("Enrollment") |>
    exclude("Did not meet criteria", n = 200,
            included_label = "Eligible participants") |>
    exclude("Declined to participate", n = 100,
            included_label = "Enrolled") |>
    phase("Analysis") |>
    endpoint("Completed study")
autoflow(flow_inclbl, file.path(outdir, "edge_included_labels.pdf"))

## ** Multi-site combine -> allocate

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

autoflow(flow_multi, file.path(outdir, "edge_multisite.pdf"))
autoflow(flow_multi, file.path(outdir, "edge_multisite_cf.pdf"),
            count_first = TRUE)

## ============================================================================
## * Font size variations
## ============================================================================

## ** Small text (cex = 0.65) — compact diagrams

autoflow(flow_m2, file.path(outdir, "font_2arm_small.pdf"),
            cex = 0.65)
autoflow(flow_m2, file.path(outdir, "font_2arm_small_cf.pdf"),
            cex = 0.65, count_first = TRUE)

## ** Large text (cex = 1.1) — presentation-ready

autoflow(flow_m2, file.path(outdir, "font_2arm_large.pdf"),
            cex = 1.1)

## ** Independent side box font (cex_side = 0.65, main stays default)

autoflow(flow_strobe, file.path(outdir, "font_strobe_smallside.pdf"),
            cex_side = 0.65)

## ** Large phase labels (cex_phase = 1.2)

autoflow(flow_m2, file.path(outdir, "font_2arm_bigphase.pdf"),
            cex_phase = 1.2)

## ** Small phase labels (cex_phase = 0.7)

autoflow(flow_m2, file.path(outdir, "font_2arm_smallphase.pdf"),
            cex_phase = 0.7)

## ** Everything large — poster mode

autoflow(flow_prisma3, file.path(outdir, "font_prisma_poster.pdf"),
            cex = 1.2, cex_side = 1.0, cex_phase = 1.3)

## ** Everything small — supplementary figure

autoflow(flow6, file.path(outdir, "font_6arm_supplement.pdf"),
            cex = 0.6, cex_side = 0.55, cex_phase = 0.65)

## ** STARD with adjusted fonts

autoflow(flow_stard, file.path(outdir, "font_stard_large.pdf"),
            cex = 1.0, cex_side = 0.8)

## ============================================================================
## * print() verification
## ============================================================================

cat("\n== print() output ==\n\n")
cat("--- 0-arm data-driven ---\n");  print(flow0)
cat("\n--- 2-arm data-driven ---\n"); print(flow2)
cat("\n--- 3-arm data-driven ---\n"); print(flow3)
cat("\n--- 6-arm data-driven ---\n"); print(flow6)
cat("\n--- STROBE per-arm ---\n");    print(flow_strobe)
cat("\n--- PRISMA 3-col ---\n");      print(flow_prisma3)
cat("\n--- STARD ---\n");             print(flow_stard)
cat("\n--- show_count=FALSE ---\n");  print(flow_nocount)

## ============================================================================
## * Summary
## ============================================================================

files <- list.files(outdir, pattern = "\\.pdf$")
cat(sprintf("\n== COMPLETE: %d PDFs generated in '%s/' ==\n",
            length(files), outdir))
cat(paste(" ", files, collapse = "\n"), "\n")
