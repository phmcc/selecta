#' Compute Enrollment Counts
#'
#' @param x A \code{selecta} object.
#' @return A list with \code{nodes}, \code{edges}, and \code{phases} data.tables.
#' @keywords internal
compute <- function(x) {

  nodes  <- vector("list", length(x$steps) * 4L)
  edges  <- vector("list", length(x$steps) * 4L)
  phases <- vector("list", length(x$steps))
  nid    <- 0L
  eid    <- 0L
  pid    <- 0L

  add_node <- function(text, n, role, reasons = NULL,
                       arm_id = NA_integer_, phase = NA_integer_) {
    nid <<- nid + 1L
    nodes[[nid]] <<- list(
      node_id = nid, text = text, n = n,
      role = role, reasons = list(reasons),
      arm_id = arm_id, phase = phase
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
  in_arms       <- FALSE
  arm_labels    <- NULL
  n_arms        <- 0L

  current_phase_label <- NULL
  current_phase_start <- NA_integer_

  close_phase <- function(end_phase) {
    if (!is.null(current_phase_label) && !is.na(current_phase_start))
      add_phase(current_phase_label, current_phase_start, end_phase)
  }

  if (x$mode == "data") {
    current_data <- list(.all = copy(x$data))
  } else {
    current_n <- x$n_start
  }

  ## Look-ahead helper: precompute next non-phase type for each step
  steps   <- x$steps
  n_steps <- length(steps)
  step_types <- vapply(steps, `[[`, character(1L), "type")
  ## Single reverse pass: next_type[i] = first non-phase type after position i
  .next_type_vec <- rep(NA_character_, n_steps)
  last_seen <- NA_character_
  for (.j in rev(seq_len(n_steps))) {
    .next_type_vec[.j] <- last_seen
    if (step_types[.j] != "phase") last_seen <- step_types[.j]
  }
  next_type <- function(idx) .next_type_vec[idx]

  ## Starting node
  diagram_phase <- diagram_phase + 1L
  start_id <- add_node(
    text = x$label, n = x$n_start, role = "main", phase = diagram_phase
  )
  last_main <- start_id

  ## ---- Walk steps ----

  for (si in seq_len(n_steps)) {
    step <- steps[[si]]

    if (step$type == "phase") {
      close_phase(diagram_phase)
      current_phase_label <- step$label
      current_phase_start <- diagram_phase + 1L
      next
    }

    if (step$type == "exclude") {
      diagram_phase <- diagram_phase + 1L
      if (!is.null(current_phase_label) && is.na(current_phase_start))
        current_phase_start <- diagram_phase

      upcoming <- next_type(si)
      skip_count_node <- !is.na(upcoming) && upcoming %chin% c("allocate", "endpoint")

      if (!in_arms) {
        res <- resolve_exclusion(
          x$mode, step,
          data      = if (x$mode == "data") current_data$.all else NULL,
          current_n = if (x$mode == "manual") current_n else NULL
        )

        side_id <- add_node(
          text = step$label, n = res$n_excluded, role = "side",
          reasons = res$reasons, phase = diagram_phase
        )
        add_edge(last_main, side_id, edge_type = "exclude")

        if (!skip_count_node) {
          rlbl <- if (!is.null(step$remaining_label)) step$remaining_label else ""
          main_id <- add_node(
            text = rlbl, n = res$n_remaining, role = "main",
            phase = diagram_phase
          )
          add_edge(last_main, main_id, edge_type = "flow")
          last_main <- main_id
        }

        if (x$mode == "data") {
          current_data$.all <- res$remaining_data
        } else {
          current_n <- res$n_remaining
        }

      } else {
        manual_ns <- step$n
        results <- lapply(seq_len(n_arms), function(i) {
          step_i <- step
          if (!is.null(step$reasons) && is.list(step$reasons))
            step_i$reasons <- step$reasons[[i]]
          ni <- if (x$mode == "manual") {
            if (length(manual_ns) == 1L) manual_ns else manual_ns[i]
          } else NULL
          resolve_exclusion(
            x$mode, step_i,
            data              = if (x$mode == "data") current_data[[arm_labels[i]]] else NULL,
            current_n         = if (x$mode == "manual") arm_current_n[i] else NULL,
            manual_n_override = ni
          )
        })

        for (i in seq_len(n_arms)) {
          res <- results[[i]]
          side_id <- add_node(
            text = step$label, n = res$n_excluded, role = "side",
            reasons = res$reasons, arm_id = i, phase = diagram_phase
          )
          add_edge(arm_last_main[i], side_id, edge_type = "exclude")

          if (!skip_count_node) {
            rlbl <- if (!is.null(step$remaining_label)) step$remaining_label else ""
            main_id <- add_node(
              text = rlbl, n = res$n_remaining, role = "main",
              arm_id = i, phase = diagram_phase
            )
            add_edge(arm_last_main[i], main_id, edge_type = "flow")
            arm_last_main[i] <- main_id
          }

          if (x$mode == "data") {
            current_data[[arm_labels[i]]] <- res$remaining_data
          } else {
            arm_current_n[i] <- res$n_remaining
          }
        }
      }

    } else if (step$type == "allocate") {
      diagram_phase <- diagram_phase + 1L
      in_arms <- TRUE

      alloc_label <- step$label %||% "Randomized"

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
        arm_labels    <- step$labels
        n_arms        <- length(arm_labels)
        arm_current_n <- step$n
        alloc_n       <- sum(arm_current_n)
      }

      ## Allocation box on its own row
      alloc_id <- add_node(
        text = alloc_label, n = alloc_n, role = "alloc",
        phase = diagram_phase
      )
      add_edge(last_main, alloc_id, edge_type = "flow")

      ## Arm nodes on a SEPARATE row below
      diagram_phase <- diagram_phase + 1L

      arm_ns <- if (x$mode == "data") {
        vapply(current_data, .row_count, integer(1L))
      } else {
        arm_current_n
      }

      arm_last_main <- vapply(seq_len(n_arms), function(i) {
        add_node(
          text = arm_labels[i], n = arm_ns[i], role = "arm",
          arm_id = i, phase = diagram_phase
        )
      }, integer(1L))

      for (i in seq_len(n_arms))
        add_edge(alloc_id, arm_last_main[i], edge_type = "split")

    } else if (step$type == "endpoint") {
      diagram_phase <- diagram_phase + 1L

      if (!in_arms) {
        ni <- if (x$mode == "data") .row_count(current_data$.all) else current_n
        eid_node <- add_node(
          text = step$label, n = ni, role = "endpoint",
          phase = diagram_phase
        )
        add_edge(last_main, eid_node, edge_type = "flow")
      } else {
        ep_ns <- if (x$mode == "data") {
          vapply(current_data, .row_count, integer(1L))
        } else {
          arm_current_n
        }
        for (i in seq_len(n_arms)) {
          eid_node <- add_node(
            text = step$label, n = ep_ns[i], role = "endpoint",
            arm_id = i, phase = diagram_phase
          )
          add_edge(arm_last_main[i], eid_node, edge_type = "flow")
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

  list(nodes = nodes_dt, edges = edges_dt, phases = phases_dt)
}


## ---- Resolve a single exclusion step ----

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
    idx_excl <- which(mask)
    idx_keep <- which(!mask)
    excluded  <- data[idx_excl]
    remaining <- data[idx_keep]

    reasons <- NULL
    if (!is.null(step$reasons_var) && length(idx_excl) > 0L) {
      rvar <- step$reasons_var
      if (rvar %chin% names(excluded))
        reasons <- tabulate_reasons(excluded, rvar)
    }
    if (!is.null(reasons) && !show_zero) {
      reasons <- reasons[reasons > 0L]
      if (length(reasons) == 0L) reasons <- NULL
    }

    list(n_excluded = length(idx_excl), n_remaining = length(idx_keep),
         remaining_data = remaining, reasons = reasons)
  } else {
    n_exc <- if (!is.null(manual_n_override)) manual_n_override else step$n
    reasons <- step$reasons
    if (!is.null(reasons) && !is.list(reasons) && !show_zero) {
      reasons <- reasons[reasons > 0L]
      if (length(reasons) == 0L) reasons <- NULL
    }
    list(n_excluded = n_exc, n_remaining = current_n - n_exc,
         remaining_data = NULL, reasons = reasons)
  }
}


## ---- Helpers ----

#' @keywords internal
tabulate_reasons <- function(dt, rvar) {
  reason_col <- dt[[rvar]]
  reason_col[is.na(reason_col)] <- "Other"
  tbl <- data.table(r = reason_col)[, .(n = .N), by = r]
  setorderv(tbl, "n", order = -1L)
  setNames(tbl$n, tbl$r)
}

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

fmt_n <- function(n) formatC(n, format = "d", big.mark = ",")

`%||%` <- function(a, b) if (is.null(a)) b else a
