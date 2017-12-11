##' @method groupOTU ggtree
##' @export
##' @importFrom tidytree groupOTU
groupOTU.ggtree <- function(.data, .node, group_name = "group", ...) {
    .data$data <- groupOTU(.data$data, .node, group_name, ...)
    return(.data)
}
