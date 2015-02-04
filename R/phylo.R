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
          function(object, focus) {
              groupOTU.phylo(object, focus)
          })


##' group OTU
##'
##' 
##' @title groupOTU.phylo
##' @param phy tree object
##' @param focus tip list
##' @return cluster index
##' @author ygc
groupOTU.phylo <- function(phy, focus) {
    if ( is(focus, "list") ) {
        for (i in 1:length(focus)) {
            phy <- gfocus(phy, focus[[i]])
        } 
    } else {
        phy <- gfocus(phy, focus)
    }
    attr(phy, "focus")
}


##' @rdname gzoom-methods
##' @exportMethod gzoom
setMethod("gzoom", signature(object="phylo"),
          function(object, focus, subtree=FALSE, widths=c(.3, .7)) {
              gzoom.phylo(object, focus, subtree, widths)
          })
