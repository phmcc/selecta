### * Main functions

#' Layout Nodes for Grid Rendering
#'
#' Assigns row (vertical position) and preliminary x (horizontal) positions
#' to all nodes. Handles multi-source streams (from \code{sources()}),
#' arm splits (from \code{stratify()}), and classification grids.
#'
#' @param graph List from \code{compute()}.
#' @return The graph with \code{x} and \code{row} columns on \code{nodes}.
#' @keywords internal
layout_nodes <- function(graph) {

  nodes <- graph$nodes

  ## ---- Row assignment ----
  phases <- sort(unique(nodes$phase))
  row_map <- setNames(seq_along(phases), as.character(phases))
  nodes[, row := row_map[as.character(phase)]]

  ## ---- X positions ----
  ## Preliminary defaults, overridden by data-driven positioning in draw.R
  ## once box widths are measured; set here so structure is inspectable.
  arm_ids <- sort(unique(nodes[!is.na(arm_id), arm_id]))
  n_arms  <- length(arm_ids)

  ## Detect source nodes
  has_sources <- nrow(nodes[role == "source"]) > 0L

  nodes[, x := 0.5]

  if (has_sources) {
    ## Multi-source layout: position source nodes by group
    src_nodes <- nodes[role == "source"]
    groups <- unique(src_nodes$stream_group)
    n_groups <- length(groups)

    if (n_groups == 1L) {
      ## Single group: centered
      nodes[role %chin% c("source", "source_header"), x := 0.35]
    } else {
      ## Multiple groups: columns
      group_x <- seq(1 / (n_groups + 1), n_groups / (n_groups + 1),
                     length.out = n_groups)
      for (gi in seq_along(groups)) {
        nodes[role %chin% c("source", "source_header") &
              stream_group == groups[gi], x := group_x[gi]]
      }
    }
    ## Non-source, non-arm nodes centered
    nodes[role %chin% c("main", "alloc", "endpoint") & is.na(arm_id), x := 0.5]
    nodes[role == "side" & is.na(arm_id), x := 0.80]
  }

  if (n_arms == 0L && !has_sources) {
    nodes[role != "side", x := 0.35]
    nodes[role == "side", x := 0.75]

  } else if (n_arms == 0L && has_sources) {
    ## Sources already positioned above; main flow below is centered
    nodes[role %chin% c("main", "endpoint") & is.na(arm_id), x := 0.35]
    nodes[role == "side" & is.na(arm_id), x := 0.75]

  } else if (n_arms == 2L) {
    ## 2-arm symmetric: left arm side to left, right arm side to right
    arm_x <- c(0.35, 0.65)
    nodes[is.na(arm_id) & role == "side", x := 0.80]
    arm_x_map  <- setNames(arm_x, as.character(arm_ids))
    side_x_map <- setNames(c(0.13, 0.87), as.character(arm_ids))
    nodes[!is.na(arm_id) & role %chin% c("arm", "main", "endpoint"),
          x := arm_x_map[as.character(arm_id)]]
    nodes[!is.na(arm_id) & role == "side",
          x := side_x_map[as.character(arm_id)]]

  } else if (n_arms > 0L) {
    ## 3+ arms: side boxes to the right of each arm
    arm_x <- seq(1 / (n_arms + 1), n_arms / (n_arms + 1), length.out = n_arms)
    arm_spacing <- diff(arm_x)[1L]

    nodes[is.na(arm_id) & role == "side", x := 0.85]
    arm_x_map <- setNames(arm_x, as.character(arm_ids))
    side_x <- arm_x + arm_spacing * 0.35
    side_x_map <- setNames(side_x, as.character(arm_ids))

    nodes[!is.na(arm_id) & role %chin% c("arm", "main", "endpoint"),
          x := arm_x_map[as.character(arm_id)]]
    nodes[!is.na(arm_id) & role == "side",
          x := side_x_map[as.character(arm_id)]]
  }

  graph$nodes <- nodes

  ## Optional debug: preliminary x and row assignments before draw-time
  ## data-driven positioning.
  if (isTRUE(getOption("selecta.debug_layout", FALSE))) {
    cols <- intersect(c("node_id", "row", "role", "arm_id", "x"), names(nodes))
    debug_emit("layout_nodes() positions", positions = nodes[, ..cols])
  }

  graph
}
