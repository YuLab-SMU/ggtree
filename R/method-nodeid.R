##' @method nodeid ggtree
##' @export
nodeid.ggtree <- function(tree, label) {
    nodeid(tree$data, label)
}

##' @method nodelab ggtree
##' @export
nodelab.ggtree <- function(tree, id) {
    nodelab(tree$data, id)
}

