
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





##' group selected clade
##'
##' 
##' @rdname groupClade-methods
##' @exportMethod groupClade
setMethod("groupClade", signature(object="nhx"),
          function(object, node, group_name="group") {
              groupClade_(object, node, group_name)
          })


##' @rdname groupClade-methods
##' @exportMethod groupClade
setMethod("groupClade", signature(object="ggplot"),
          function(object, node, group_name) {
              groupClade.ggplot(object, node, group_name)
          })


##' @rdname groupClade-methods
##' @exportMethod groupClade
setMethod("groupClade", signature(object="gg"),
          function(object, node, group_name) {
              groupClade.ggplot(object, node, group_name)
          })

