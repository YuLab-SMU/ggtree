##' read newick tree
##' @export
##' @rdname read.tree
##' @param file file name
##' @param text alternatively, using newick text
##' @param tree.names if read several trees, specify their names
##' @param skip number of lines of the input file to skip
##' @param comment.char a single character,
##'                     the remaining of the line after this character is ignored.
##' @param keep.multi if 'TRUE' and 'tree.names = NULL'
##'                   then single trees are returned in 'multiPhylo' format
##'                   with any name that is present. Default is 'FALSE'
##' @param ... further arguments to be passed to 'scan()'.
##' @source
##' This is just the imported function
##' from the ape package. The documentation you should
##' read for the read.tree function can be found here: \link[ape]{read.tree}
##'
##' @seealso
##' \link[ape]{read.tree}
read.tree <- ape::read.tree


##' generate random tree
##' @export
##' @rdname rtree
##' @param n number of tips in the tree
##' @param rooted logcial
##' @param tip.label tip label
##' @param br one of the following: (i) an R function used to generate the
##'           branch lengths ('rtree'; use 'NULL' to simulate only a
##'           topology), or the coalescence times ('rcoal'); (ii) a
##'           character to simulate a genuine coalescent tree for 'rcoal'
##'           (the default); or (iii) a numeric vector for the branch
##'           lengths or the coalescence times.
##' @param ... additional parameters to be passed to 'br'
##' @source
##' This is just the imported function
##' from the ape package. The documentation you should
##' read for the rtree function can be found here: \link[ape]{rtree}
##'
##' @seealso
##' \link[ape]{rtree}
rtree <- ape::rtree

