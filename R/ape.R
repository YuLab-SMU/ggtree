##' merge phylo and output of boot.phylo to 'apeBootstrap' object
##'
##'
##' @title apeBoot
##' @param phylo phylo
##' @param boot bootstrap values
##' @return an instance of 'apeBootstrap'
##' @importFrom treeio as.treedata
##' @export
##' @author Guangchuang Yu
apeBoot <- function(phylo, boot) {
    message("this function was deprecated, please use treeio::as.treedata")
    as.treedata(phylo, boot)
}

