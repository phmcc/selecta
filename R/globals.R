#' @keywords internal
"_PACKAGE"

#' @import data.table
#' @import grid
#' @importFrom grDevices dev.off pdf png svg tiff
#' @importFrom stats setNames
NULL

## Suppress R CMD check notes for data.table NSE column references
## and join column prefixes used throughout the package.
utils::globalVariables(c(
           ".",
           "arm_id",
           "bh_inches",
           "box_h",
           "bw",
           "bw_inches",
           "edge_type",
           "fill_col",
           "from",
           "from_bot",
           "from_row",
           "fr",
           "from_arm",
           "grid_col",
           "grid_row",
           "hdr_h",
           "i.bh_inches",
           "i.box_h",
           "i.hdr_h",
           "i.row",
           "i.y",
           "n",
           "n_reason",
           "n_sides",
           "needed",
           "node_id",
           "r",
           "reasons",
           "role",
           "src_h",
           "stack_h",
           "stream_group",
           "sublabel",
           "text",
           "to",
           "to_top",
           "to_row",
           "total",
           "x",
           "x_in",
           "y"
       ))
