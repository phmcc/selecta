#' Convert Graph to Graphviz DOT String
#'
#' @param graph A computed and laid-out graph.
#' @return A character string in DOT format.
#' @keywords internal
to_dot <- function(graph) {

    nodes <- graph$nodes
    edges <- graph$edges

    lines <- character()
    lines <- c(lines, "digraph selecta {")
    lines <- c(lines, '  rankdir=TB;')
    lines <- c(lines, '  node [shape=box, style=filled, fontname="Helvetica"];')

    for (i in seq_len(nrow(nodes))) {
        nd <- nodes[i]
        lbl <- if (nchar(nd$text) > 0L) {
                   sprintf("%s\\nn = %s", nd$text, fmt_n(nd$n))
               } else {
                   sprintf("n = %s", fmt_n(nd$n))
               }

        fill <- switch(nd$role,
                       side          = "#f0f0f0",
                       source        = "#e8f4e8",
                       source_header = "#d4e8d4",
                       cell          = "#e8e8f4",
                       alloc         = "#ffffff",
                       "#ffffff"
                       )

        lines <- c(lines, sprintf('  n%d [label="%s", fillcolor="%s"];',
                                  nd$node_id, lbl, fill))
    }

    for (i in seq_len(nrow(edges))) {
        e <- edges[i]
        style <- switch(e$edge_type,
                        exclude  = ' [style=dashed]',
                        converge = ' [style=bold]',
                        classify = ' [arrowhead=normal]',
                        ''
                        )
        lines <- c(lines, sprintf("  n%d -> n%d%s;", e$from, e$to, style))
    }

    lines <- c(lines, "}")
    paste(lines, collapse = "\n")
}
