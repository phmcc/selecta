### * Main functions

#' Compute Enrollment Counts
#'
#' Walks the step list and resolves all counts, producing a graph of
#' nodes, edges, and phases. Maintains a generalized stream model where
#' parallel tracks (from \code{sources()} or \code{stratify()}) are
#' stored as a list of active streams.
#'
#' @param x A \code{selecta} object.
#' @return A list with components \code{nodes}, \code{edges}, and
#'   \code{phases}, each a \code{data.table}.
#' @keywords internal
compute <- function(x) {

  nodes  <- vector("list", length(x$steps) * 8L)
  edges  <- vector("list", length(x$steps) * 8L)
  phases <- vector("list", length(x$steps))
  nid    <- 0L
  eid    <- 0L
  pid    <- 0L

  add_node <- function(text, n, role, reasons = NULL,
                       arm_id = NA_integer_, phase = NA_integer_,
                       stream_group = NA_character_,
                       sublabel = NA_character_) {
    nid <<- nid + 1L
    ## Defensive: drop NULL scalars (which would create length-0 list
    ## elements and trigger rbindlist warnings during finalization).
    if (is.null(sublabel))     sublabel     <- NA_character_
    if (is.null(stream_group)) stream_group <- NA_character_
    if (is.null(arm_id))       arm_id       <- NA_integer_
    nodes[[nid]] <<- list(
      node_id = nid, text = text, n = n,
      role = role, reasons = list(reasons),
      arm_id = arm_id, phase = phase,
      stream_group = stream_group,
      sublabel = sublabel
    )
    nid
  }

  add_edge <- function(from, to, edge_type = "flow") {
    eid <<- eid + 1L
    edges[[eid]] <<- list(from = from, to = to, edge_type = edge_type)
  }

  add_phase <- function(label, phase_start, phase_end) {
    pid <<- pid + 1L
    phases[[pid]] <<- list(
      label = label, phase_start = phase_start, phase_end = phase_end
    )
  }

  diagram_phase <- 0L

  current_phase_label <- NULL
  current_phase_start <- NA_integer_

  close_phase <- function(end_phase) {
    if (!is.null(current_phase_label) && !is.na(current_phase_start))
      add_phase(current_phase_label, current_phase_start, end_phase)
  }

  ## Record phase start on first node following a phase() step
  try_start_phase <- function() {
    if (!is.null(current_phase_label) && is.na(current_phase_start))
      current_phase_start <<- diagram_phase
  }

  ## ---- Stream state ----
  ## Each stream: list(id, label, group, last_node, data, n)
  streams   <- list()
  n_streams <- 0L
  in_arms   <- FALSE   # TRUE after stratify() specifically

  ## Single-stream tracking
  last_main   <- NULL     # node_id for single-stream mode

  if (x$mode == "data") {
    current_data <- list(.all = copy(x$data))
  } else {
    current_n <- x$n_start
  }

  ## Next-step type lookup (skipping phase steps)
  steps   <- x$steps
  n_steps <- length(steps)
  step_types <- vapply(steps, `[[`, character(1L), "type")
  .next_type_vec <- rep(NA_character_, n_steps)
  last_seen <- NA_character_
  for (.j in rev(seq_len(n_steps))) {
    .next_type_vec[.j] <- last_seen
    if (step_types[.j] != "phase") last_seen <- step_types[.j]
  }
  next_type <- function(idx) .next_type_vec[idx]

  ## Detect multi-source entry
  has_sources <- n_steps > 0L && step_types[1L] == "sources"

  if (!has_sources) {
    ## Standard single-entry flow
    diagram_phase <- diagram_phase + 1L
    start_id <- add_node(
      text = x$label, n = x$n_start, role = "main", phase = diagram_phase
    )
    last_main <- start_id
  }
  ## Multi-source flows: starting nodes created by the sources handler

  ## ---- Step dispatch ----

  ## diagram_phase of the entry node(s). Set here for enroll() (single
  ## entry); for sources() it is set when that step runs. Lets the first
  ## phase encompass the entry box(es).
  entry_phase    <- if (!has_sources) diagram_phase else NA_integer_
  prev_phase_seen <- FALSE

  for (si in seq_len(n_steps)) {
    step <- steps[[si]]

    ## ---- Phase ----
    if (step$type == "phase") {
      close_phase(diagram_phase)
      current_phase_label <- step$label
      ## A phase normally starts at the next diagram phase. The exception is
      ## the first phase declared immediately after the entry step (enroll()
      ## or sources()): it encompasses the entry node(s), detected by the
      ## entry phase still being current with no prior phase opened.
      current_phase_start <- if (!prev_phase_seen &&
                                 !is.na(entry_phase) &&
                                 diagram_phase == entry_phase)
                                 entry_phase
                             else
                                 diagram_phase + 1L
      prev_phase_seen <- TRUE
      next
    }

    ## ---- Sources ----
    if (step$type == "sources") {
      diagram_phase <- diagram_phase + 1L
      ## Record the entry diagram phase so a phase() declared immediately
      ## after sources() encompasses the source row (mirrors the enroll()
      ## case, where entry_phase is set before the loop).
      if (!prev_phase_seen && is.na(entry_phase)) entry_phase <- diagram_phase
      try_start_phase()

      ## One header node and one source node per group
      stream_id <- 0L

      for (gi in seq_along(step$groups)) {
        grp <- step$groups[[gi]]
        stream_id <- stream_id + 1L
        group_n <- sum(grp$counts)

        ## Header node (if header is defined)
        if (!is.null(grp$header)) {
          hdr_id <- add_node(
            text = grp$header, n = 0L, role = "source_header",
            phase = diagram_phase,
            stream_group = grp$group
          )
        }

        ## Source group node: individual sources as sub-items
        src_reasons <- setNames(grp$counts, grp$labels)
        src_id <- add_node(
          text = "Records identified", n = group_n, role = "source",
          reasons = src_reasons,
          phase = diagram_phase,
          stream_group = grp$group
        )

        streams[[stream_id]] <- list(
          id        = stream_id,
          label     = grp$group,
          group     = grp$group,
          last_node = src_id,
          n         = group_n
        )
      }
      n_streams <- stream_id
      next
    }

    ## ---- Combine ----
    if (step$type == "combine") {
      diagram_phase <- diagram_phase + 1L
      try_start_phase()

      total_n <- sum(vapply(streams, function(s) s$n, numeric(1L)))
      merge_n <- if (!is.null(step$n)) step$n else total_n
      ## A manually supplied combine total should match the streams feeding it.
      if (x$mode == "manual" && !is.null(step$n) &&
          length(step$n) == 1L && !is.na(step$n) && step$n != total_n) {
        warn_arithmetic(
          "Combine '%s' is given as %s but the incoming streams sum to %s.",
          step$label, step$n, total_n)
      }

      ## Build node text: label + optional sublabel
      node_text <- step$label
      node_sublabel <- step$sublabel

      ## Merged node
      merge_id <- add_node(
        text = node_text, n = merge_n, role = "main",
        reasons = step$reasons,
        phase = diagram_phase,
        sublabel = node_sublabel
      )

      if (in_arms) {
        ## Post-stratify combine: one converge edge per arm stream
        for (i in seq_len(n_streams)) {
          add_edge(streams[[i]]$last_node, merge_id, edge_type = "converge")
        }

        ## Recombine data in data mode
        if (x$mode == "data") {
          arm_labels_cur <- vapply(streams, function(s) s$label, character(1L))
          current_data <- list(.all = rbindlist(current_data[arm_labels_cur]))
        }

        in_arms <- FALSE
      } else {
        ## Post-sources combine: one converge edge per source group
        group_last <- vapply(
            split(streams, vapply(streams, `[[`, character(1L), "group")),
            function(ss) ss[[length(ss)]]$last_node,
            integer(1L)
        )
        for (g_last_node in group_last) {
          add_edge(g_last_node, merge_id, edge_type = "converge")
        }
      }

      ## Collapse to single stream
      streams <- list()
      n_streams <- 0L
      last_main <- merge_id

      if (x$mode == "manual") {
        current_n <- merge_n
      }

      next
    }

    ## ---- Exclude ----
    if (step$type == "exclude") {
      diagram_phase <- diagram_phase + 1L
      try_start_phase()

      upcoming <- next_type(si)
      has_incl_label <- !is.null(step$included_label)
      skip_count_node <- (!isTRUE(step$show_count) && !has_incl_label) ||
        (!is.na(upcoming) &&
         upcoming %chin% c("stratify", "allocate", "endpoint", "combine"))

      if (!in_arms) {
        res <- resolve_exclusion(
          x$mode, step,
          data      = if (x$mode == "data") current_data$.all else NULL,
          current_n = if (x$mode == "manual") current_n else NULL
        )

        ## Side box label: scalar (pre-stratify, always length 1)
        side_lbl <- step$label[1L]

        side_id <- add_node(
          text = side_lbl, n = res$n_excluded, role = "side",
          reasons = res$reasons, phase = diagram_phase
        )
        add_edge(last_main, side_id, edge_type = "exclude")

        if (!skip_count_node) {
          rlbl <- if (!is.null(step$included_label)) step$included_label[1L] else ""
          main_id <- add_node(
            text = rlbl, n = res$n_included, role = "main",
            phase = diagram_phase
          )
          add_edge(last_main, main_id, edge_type = "flow")
          last_main <- main_id
        }

        if (x$mode == "data") {
          current_data$.all <- res$included_data
        } else {
          current_n <- res$n_included
        }

      } else {
        ## Per-arm exclusions
        arm_labels <- vapply(streams, function(s) s$label, character(1L))
        manual_ns <- step$n

        ## Resolve per-arm side labels and included labels
        ## If scalar, recycle; if vector, index by arm
        side_labels <- step$label
        incl_labels <- step$included_label

        results <- lapply(seq_len(n_streams), function(i) {
          step_i <- step
          if (!is.null(step$reasons) && is.list(step$reasons))
            step_i$reasons <- step$reasons[[i]]
          ni <- if (x$mode == "manual") {
            if (length(manual_ns) == 1L) manual_ns else manual_ns[i]
          } else NULL
          resolve_exclusion(
            x$mode, step_i,
            data              = if (x$mode == "data") current_data[[arm_labels[i]]] else NULL,
            current_n         = if (x$mode == "manual") streams[[i]]$n else NULL,
            manual_n_override = ni
          )
        })

        ## Harmonize reason ordering across arms so categories appear in the
        ## same position in every side box, ordered by total count descending.
        has_any_reasons <- any(vapply(results,
            function(r) !is.null(r$reasons), logical(1L)))
        if (has_any_reasons) {
          ## Collect all reason names and sum counts across arms
          all_names <- unique(unlist(lapply(results,
              function(r) names(r$reasons))))
          totals <- vapply(all_names, function(nm) {
            sum(vapply(results, function(r) {
              rv <- r$reasons[nm]
              if (is.na(rv)) 0L else as.integer(rv)
            }, integer(1L)))
          }, integer(1L))
          global_order <- all_names[order(totals, decreasing = TRUE)]

          ## Reorder each arm's reasons to match
          for (ri in seq_along(results)) {
            r <- results[[ri]]$reasons
            if (!is.null(r)) {
              ordered <- r[intersect(global_order, names(r))]
              results[[ri]]$reasons <- ordered
            }
          }
        }

        for (i in seq_len(n_streams)) {
          res <- results[[i]]

          ## Per-arm side box label
          s_lbl <- if (length(side_labels) >= i) side_labels[i] else side_labels[1L]

          side_id <- add_node(
            text = s_lbl, n = res$n_excluded, role = "side",
            reasons = res$reasons, arm_id = i, phase = diagram_phase
          )
          add_edge(streams[[i]]$last_node, side_id, edge_type = "exclude")

          if (!skip_count_node) {
            ## Per-arm included label
            rlbl <- if (!is.null(incl_labels)) {
              if (length(incl_labels) >= i) incl_labels[i] else incl_labels[1L]
            } else ""
            main_id <- add_node(
              text = rlbl, n = res$n_included, role = "main",
              arm_id = i, phase = diagram_phase
            )
            add_edge(streams[[i]]$last_node, main_id, edge_type = "flow")
            streams[[i]]$last_node <- main_id
          }

          if (x$mode == "data") {
            current_data[[arm_labels[i]]] <- res$included_data
          }
          ## Track the post-exclusion count for this stream in BOTH modes.
          ## In manual mode this is the authoritative count; in data mode it
          ## mirrors nrow(included_data) and is what a later combine() sums,
          ## so omitting it here made combine() report the pre-exclusion
          ## total even though cohorts() (which recomputes from the data)
          ## reported the correct figure.
          streams[[i]]$n <- res$n_included
        }
      }
    }

    ## ---- Stratify ----
    if (step$type == "stratify") {
      diagram_phase <- diagram_phase + 1L
      in_arms <- TRUE

      alloc_label <- step$label %||% "Stratified"

      if (x$mode == "data") {
        var <- step$variable
        if (!var %chin% names(current_data$.all))
          stop(sprintf("Column '%s' not found in data", var), call. = FALSE)
        all_data <- current_data$.all
        alloc_n  <- .row_count(all_data)
        split_result <- split_by_var(all_data, var, step$labels)
        arm_labels   <- split_result$labels
        n_arms       <- length(arm_labels)
        current_data <- split_result$data
      } else {
        arm_labels <- step$labels
        n_arms     <- length(arm_labels)
        alloc_n    <- sum(step$n)
        ## Arm counts should account for everyone entering the split.
        if (length(current_n) == 1L && !is.na(current_n) &&
            alloc_n != current_n) {
          warn_arithmetic(
            "Split '%s' arm counts sum to %s but %s entered (a difference of %s).",
            alloc_label, alloc_n, current_n, abs(current_n - alloc_n))
        }
      }

      ## Allocation box on its own row
      alloc_id <- add_node(
        text = alloc_label, n = alloc_n, role = "alloc",
        phase = diagram_phase
      )
      add_edge(last_main, alloc_id, edge_type = "flow")

      ## Arm nodes on their own row
      diagram_phase <- diagram_phase + 1L

      arm_ns <- if (x$mode == "data") {
        vapply(current_data, .row_count, integer(1L))
      } else {
        step$n
      }

      ## Create per-arm streams
      streams <- lapply(seq_len(n_arms), function(i) {
        arm_node <- add_node(
          text = arm_labels[i], n = arm_ns[i], role = "arm",
          arm_id = i, phase = diagram_phase
        )
        add_edge(alloc_id, arm_node, edge_type = "split")
        list(
          id        = i,
          label     = arm_labels[i],
          group     = NA_character_,
          last_node = arm_node,
          n         = arm_ns[i]
        )
      })
      n_streams <- n_arms
    }

    ## ---- Endpoint ----
    if (step$type == "endpoint") {
      diagram_phase <- diagram_phase + 1L

      if (!in_arms) {
        ni <- if (x$mode == "data") .row_count(current_data$.all) else current_n
        ep_reasons <- if (is.list(step$reasons)) step$reasons[[1L]] else step$reasons
        eid_node <- add_node(
          text = step$label, n = ni, role = "endpoint",
          reasons = ep_reasons, phase = diagram_phase
        )
        add_edge(last_main, eid_node, edge_type = "flow")
      } else {
        ep_ns <- if (x$mode == "data") {
          arm_labels <- vapply(streams, function(s) s$label, character(1L))
          vapply(current_data[arm_labels], .row_count, integer(1L))
        } else {
          vapply(streams, function(s) s$n, numeric(1L))
        }
        for (i in seq_len(n_streams)) {
          ep_reasons_i <- if (is.list(step$reasons) && length(step$reasons) >= i) {
            step$reasons[[i]]
          } else if (!is.list(step$reasons)) {
            step$reasons
          } else NULL
          eid_node <- add_node(
            text = step$label, n = ep_ns[i], role = "endpoint",
            reasons = ep_reasons_i, arm_id = i, phase = diagram_phase
          )
          add_edge(streams[[i]]$last_node, eid_node, edge_type = "flow")
        }
      }
    }
  }

  close_phase(diagram_phase)

  nodes_dt  <- rbindlist(nodes[seq_len(nid)])
  edges_dt  <- if (eid > 0L) rbindlist(edges[seq_len(eid)]) else {
    data.table(from = integer(), to = integer(), edge_type = character())
  }
  phases_dt <- if (pid > 0L) rbindlist(phases[seq_len(pid)]) else {
    data.table(label = character(), phase_start = integer(),
               phase_end = integer())
  }

  ## Optional debug: the constructed graph (nodes, edges, phase ranges).
  if (isTRUE(getOption("selecta.debug_layout", FALSE))) {
    node_cols <- intersect(c("node_id", "row", "role", "phase", "arm_id",
                             "stream_group", "text", "n"), names(nodes_dt))
    debug_emit("compute() graph",
               n_nodes = nrow(nodes_dt), n_edges = nrow(edges_dt),
               n_phases = nrow(phases_dt),
               nodes = nodes_dt[, ..node_cols],
               edges = edges_dt,
               phases = phases_dt)
  }

  list(nodes = nodes_dt, edges = edges_dt, phases = phases_dt)
}


### * Exclusion resolution

#' Resolve an Exclusion Step
#'
#' Evaluates a single exclusion step in either data or manual mode and
#' returns the excluded and remaining counts, the remaining data (data
#' mode), and any tabulated sub-reasons.
#'
#' @param mode Character, either \code{"data"} or \code{"manual"}.
#' @param step The exclusion step (list) from the pipeline.
#' @param data A \code{data.table} of current participants (data mode).
#' @param current_n Integer current count (manual mode).
#' @param manual_n_override Optional integer overriding \code{step$n}.
#' @return A list with \code{n_excluded}, \code{n_included},
#'   \code{included_data}, and \code{reasons}.
#' @keywords internal
resolve_exclusion <- function(mode, step, data = NULL, current_n = NULL,
                              manual_n_override = NULL) {
  show_zero <- isTRUE(step$show_zero)

  if (mode == "data") {
    mask <- tryCatch(
      eval(step$expr_call, envir = data, enclos = parent.frame(3L)),
      error = function(e) {
        stop(sprintf("Error evaluating exclusion '%s': %s",
                     step$label, conditionMessage(e)), call. = FALSE)
      }
    )
    if (!is.logical(mask))
      stop(sprintf("Exclusion '%s' must evaluate to logical", step$label),
           call. = FALSE)
    mask[is.na(mask)] <- FALSE
    ## Only the included rows are needed downstream (included_data). The
    ## excluded count comes from the mask directly, and reason tabulation
    ## needs only the reason column, so the full excluded subset is never
    ## materialized.
    included <- data[!mask]
    n_excl <- sum(mask)
    n_keep <- nrow(remaining)

    reasons <- NULL
    if (!is.null(step$reasons_var) && n_excl > 0L) {
      rvar <- step$reasons_var
      if (rvar %chin% names(data))
        reasons <- tabulate_reasons(data[[rvar]][mask])
    }
    if (!is.null(reasons) && !show_zero) {
      reasons <- reasons[reasons > 0L]
      if (length(reasons) == 0L) reasons <- NULL
    }

    list(n_excluded = n_excl, n_included = n_keep,
         included_data = included, reasons = reasons)
  } else {
    n_exc <- if (!is.null(manual_n_override)) manual_n_override else step$n

    ## Advisory arithmetic checks (manual mode). Counts are not altered.
    ## All manual-mode checks are evaluated here at compute time so the audit
    ## is complete and consistent: an over-exclusion, and sub-reasons that do
    ## not sum to the exclusion total. (For per-arm exclusions this function
    ## is called once per arm with arm-resolved values, so the scalar checks
    ## below cover each arm.)
    if (length(current_n) == 1L && !is.na(current_n) &&
        length(n_exc) == 1L && !is.na(n_exc) && n_exc > current_n) {
      warn_arithmetic(
        "Exclusion '%s' removes %s but only %s are available (remaining would be %s).",
        step$label, n_exc, current_n, current_n - n_exc)
    }
    raw_reasons <- step$reasons
    if (!is.null(raw_reasons) && !is.list(raw_reasons) &&
        is.numeric(raw_reasons) && length(n_exc) == 1L && !is.na(n_exc) &&
        sum(raw_reasons) != n_exc) {
      warn_arithmetic(
        "Exclusion '%s' sub-reasons sum to %s but the exclusion total is %s.",
        step$label, sum(raw_reasons), n_exc)
    }

    reasons <- step$reasons
    if (!is.null(reasons) && !is.list(reasons) && !show_zero) {
      reasons <- reasons[reasons > 0L]
      if (length(reasons) == 0L) reasons <- NULL
    }
    list(n_excluded = n_exc, n_included = current_n - n_exc,
         included_data = NULL, reasons = reasons)
  }
}


### * Helper functions

#' Tabulate Exclusion Sub-Reasons
#'
#' Counts occurrences of each reason category in a vector, treating
#' \code{NA} as \code{"Other"}, and returns counts sorted descending.
#'
#' @param reason_col A vector of reason values for the excluded participants.
#' @return A named integer vector of counts, ordered by descending count.
#' @keywords internal
tabulate_reasons <- function(reason_col) {
  reason_col[is.na(reason_col)] <- "Other"
  tbl <- data.table(r = reason_col)[, .(n = .N), by = r]
  setorderv(tbl, "n", order = -1L)
  setNames(tbl$n, tbl$r)
}

#' Split a Dataset into Arm Streams by a Variable
#'
#' Partitions a \code{data.table} by the levels of a splitting variable,
#' optionally relabelling levels, and returns the per-arm data and labels.
#'
#' @param dt A \code{data.table} to partition.
#' @param var Character name of the splitting variable.
#' @param labels Optional character vector of arm labels; may be named to
#'   relabel specific factor levels.
#' @return A list with \code{data} (named list of per-arm
#'   \code{data.table}s) and \code{labels} (character arm labels).
#' @keywords internal
split_by_var <- function(dt, var, labels = NULL) {
  split_col <- dt[[var]]
  lvls <- if (is.factor(split_col)) levels(split_col) else sort(unique(split_col))
  if (!is.null(labels)) {
    if (!is.null(names(labels))) {
      arm_labels <- as.character(labels)
      arm_keys   <- names(labels)
    } else {
      arm_labels <- as.character(labels)
      arm_keys   <- lvls
    }
  } else {
    arm_labels <- as.character(lvls)
    arm_keys   <- lvls
  }
  splits <- split(dt, by = var, drop = TRUE)
  data_list <- setNames(
    lapply(arm_keys, function(k) {
      if (k %chin% names(splits)) splits[[k]] else dt[0L]
    }), arm_labels
  )
  list(labels = arm_labels, data = data_list)
}

#' @keywords internal
.row_count <- nrow
