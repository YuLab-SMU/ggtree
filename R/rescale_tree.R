##' rescale branch length of tree object
##'
##' 
##' @title rescale_tree
##' @param tree_object tree object
##' @param branch.length numerical features (e.g. dN/dS)
##' @return update tree object
##' @export
##' @author Guangchuang Yu
rescale_tree <- function(tree_object, branch.length) {
    tree_object <- set_branch_length(tree_object, branch.length)
    return(tree_object)
}
