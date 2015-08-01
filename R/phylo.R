##' @rdname get.tree-methods
##' @exportMethod get.tree
setMethod("get.tree", signature(object="phylo"),
          function(object, ...) {
              return(object)
          })

##' @rdname scale_color-methods
##' @exportMethod scale_color
setMethod("scale_color", signature(object="phylo"),
          function(object, by, ...) {
              scale_color_(object, by, ...)
          })


##' @rdname groupOTU-methods
##' @exportMethod groupOTU
setMethod("groupOTU", signature(object="phylo"),
          function(object, focus, group_name="group") {
              groupOTU.phylo(object, focus, group_name)
          })


##' group OTU
##'
##' 
##' @title groupOTU.phylo
##' @param phy tree object
##' @param focus tip list
##' @param group_name name of the group
##' @return phylo object
##' @author ygc
groupOTU.phylo <- function(phy, focus, group_name="group") {
    attr(phy, group_name) <- NULL
    if ( is(focus, "list") ) {
        for (i in 1:length(focus)) {
            phy <- gfocus(phy, focus[[i]], group_name)
        } 
    } else {
        phy <- gfocus(phy, focus, group_name)
    }
    attr(phy, group_name) <- factor(attr(phy, group_name))
    return(phy)
}

##' @rdname groupClade-methods
##' @exportMethod groupClade
setMethod("groupClade", signature(object="phylo"),
          function(object, node, group_name="group") {
              groupClade.phylo(object, node, group_name)
          })

groupClade.phylo <- function(object, node, group_name) {
    if (length(node) == 1) {
        clade <- extract.clade(object, node)
        tips <- clade$tip.label
    } else {
        tips <- lapply(node, function(x) {
            clade <- extract.clade(object, x)
            clade$tip.label
        })
    }
    
    groupOTU.phylo(object, tips, group_name)
}


##' @rdname gzoom-methods
##' @exportMethod gzoom
setMethod("gzoom", signature(object="phylo"),
          function(object, focus, subtree=FALSE, widths=c(.3, .7)) {
              gzoom.phylo(object, focus, subtree, widths)
          })
