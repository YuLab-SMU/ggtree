##' zoom selected subtree
##'
##' 
##' @rdname gzoom-methods
##' @exportMethod gzoom
setMethod("gzoom", signature(object="beast"),
          function(object, focus, subtree=FALSE, widths=c(.3, .7)) {
              gzoom.phylo(get.tree(object), focus, subtree, widths)
          })


##' @rdname gzoom-methods
##' @exportMethod gzoom
setMethod("gzoom", signature(object="nhx"),
          function(object, focus, subtree=FALSE, widths=c(.3, .7)) {
              gzoom.phylo(get.tree(object), focus, subtree, widths)
          })


##' @rdname gzoom-methods
##' @exportMethod gzoom
setMethod("gzoom", signature(object="paml_rst"),
          function(object, focus, subtree=FALSE, widths=c(.3, .7)) {
              gzoom.phylo(get.tree(object), focus, subtree, widths)
          })


