##' convert raxml bootstrap tree to newick format
##'
##' 
##' @title raxml2nwk
##' @param infile input file
##' @param outfile output file
##' @return newick file
##' @export
##' @importFrom ape write.tree
##' @author Guangchuang Yu
raxml2nwk <- function(infile, outfile="raxml.tree") {
    raxml <- read.raxml(infile)
    nlabel <- raxml@bootstrap[,2]
    nlabel[is.na(nlabel)] <- ""
    raxml@phylo$node.label <- nlabel
    write.tree(raxml@phylo, file=outfile)
}

