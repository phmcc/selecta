#' Layout Nodes for Grid Rendering
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
  ## Initial x values; these are overridden by data-driven positioning
  ## in draw.R after box widths are measured.  We set reasonable defaults
  ## here so the graph structure is inspectable before rendering.
  arm_ids <- sort(unique(nodes[!is.na(arm_id), arm_id]))
  n_arms  <- length(arm_ids)

  nodes[, x := 0.5]

  if (n_arms == 0L) {
    nodes[role != "side", x := 0.35]
    nodes[role == "side", x := 0.75]

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

  } else {
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
  graph
}
