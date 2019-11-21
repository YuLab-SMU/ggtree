##' @importFrom tidytree offspring
##' @method offspring ggtree
##' @export
offspring.ggtree <- function(.data, .node, tiponly = FALSE, self_include = FALSE, ...) {
    offspring.tbl_tree(.data$data, .node = .node,
                       tiponly = tiponly,
                       self_include = self_include,...)
}
