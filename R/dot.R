#' Convert Graph to Graphviz DOT String
#'
#' @param graph A laid-out graph from \code{layout_nodes()}.
#' @return A character string containing the DOT specification.
#' @keywords internal
to_dot <- function(graph) {

  nodes <- graph$nodes
  edges <- graph$edges

  ## ---- Header ----

  header <- c(
    "digraph selecta {",
    "  graph [rankdir=TB, splines=ortho, nodesep=0.5, ranksep=0.6];",
    "  node [shape=box, style=filled, fontsize=10, fontname=Helvetica, margin=\"0.15,0.08\"];",
    "  edge [arrowsize=0.7];",
    ""
  )

  ## ---- Nodes (vectorized) ----

  ## Build display labels from structured text + n columns
  labs <- fifelse(
    nodes$role == "side",
    sprintf("%s (n = %s)", nodes$text, fmt_n(nodes$n)),
    fifelse(
      nchar(nodes$text) > 0L,
      sprintf("%s\\n(n = %s)", nodes$text, fmt_n(nodes$n)),
      sprintf("(n = %s)", fmt_n(nodes$n))
    )
  )
  labs <- gsub('"', '\\\\"', labs)
  fills <- fifelse(nodes$role == "side", "#f7f7f7", "#ffffff")
  node_lines <- sprintf('  n%d [label="%s", fillcolor="%s"];',
                        nodes$node_id, labs, fills)

  ## ---- Rank alignment (vectorized per phase) ----

  phase_ids <- split(nodes$node_id, nodes$phase)
  rank_lines <- vapply(phase_ids, function(ids) {
    if (length(ids) > 1L) {
      sprintf("  { rank=same; %s }",
              paste(sprintf("n%d", ids), collapse = "; "))
    } else {
      NA_character_
    }
  }, character(1L))
  rank_lines <- rank_lines[!is.na(rank_lines)]

  ## ---- Edges (vectorized) ----

  styles <- fifelse(edges$edge_type == "exclude", " [style=dashed]", "")
  edge_lines <- sprintf("  n%d -> n%d%s;", edges$from, edges$to, styles)

  ## ---- Assemble ----

  paste(c(header, node_lines, "", rank_lines, "", edge_lines, "}"),
        collapse = "\n")
}
