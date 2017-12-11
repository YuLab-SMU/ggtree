##' @importFrom tidytree groupClade
##' @method groupClade ggtree
##' @export
groupClade.ggtree <- function(.data, .node, group_name = "group", ...) {
    .data$data <- groupClade(.data$data, .node, group_name, ...)
}
