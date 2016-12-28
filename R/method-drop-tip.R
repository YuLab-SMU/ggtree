## ##' drop.tip method
## ##'
## ##'
## ##' @rdname drop.tip-methods
## ##' @aliases drop.tip,nhx
## ##' @exportMethod drop.tip
## ##' @author Casey Dunn \url{http://dunnlab.org}  and Guangchuang Yu \url{https://guangchuangyu.github.io}
## ##' @usage drop.tip(object, tip, ...)
## setMethod("drop.tip", signature(object="nhx"),
##           function(object, tip, ...) {

##               ## label the internal tree nodes by their number
##               no_node_label <- FALSE
##               if (is.null(object@phylo$node.label)) {
##                   object@phylo$node.label <- Ntip(object) + (1:Nnode(object))
##                   no_node_label <- TRUE
##               }

##               ## Prepare the nhx object for subsampling
##               object@nhx_tags$node <- as.numeric(object@nhx_tags$node)
##               object@nhx_tags <- object@nhx_tags[order(object@nhx_tags$node),]

##               ## add a colmn that has labels for both tips and internal nodes
##               object@nhx_tags$node.label <- c(object@phylo$tip.label, as.character(object@phylo$node.label))

##               ## Will need to take different approaches for subsampling tips
##               ## and internal nodes, add a column to make it easy to tell them apart
##               object@nhx_tags$is_tip <- object@nhx_tags$node <= Ntip(object)

##               ## Remove tips
##               object@phylo = ape::drop.tip( object@phylo, tip )

##               ## Subsample the tags
##               object@nhx_tags = object@nhx_tags[object@nhx_tags$node.label %in% (c(object@phylo$tip.label, as.character(object@phylo$node.label))),]

##               ## Update tip node numbers
##               tip_nodes <- object@nhx_tags$node.label[ object@nhx_tags$is_tip ]
##               object@nhx_tags$node[ object@nhx_tags$is_tip ] = match(object@phylo$tip.label, tip_nodes)

##               internal_nodes <- object@nhx_tags$node.label[ !object@nhx_tags$is_tip ]
##               object@nhx_tags$node[ !object@nhx_tags$is_tip ] = match(object@phylo$node.label, internal_nodes) + length(object@phylo$tip.label)

##               ## Clean up
##               object@nhx_tags$node.label = NULL
##               object@nhx_tags$is_tip = NULL
##               if (no_node_label) {
##                   object@phylo$node.label <- NULL
##               }

##               return(object)
##           })





## ##' @rdname drop.tip-methods
## ##' @exportMethod drop.tip
## ##' @aliases drop.tip,phylo
## ##' @source
## ##' drop.tip for phylo object is a wrapper method of ape::drop.tip
## ##' from the ape package. The documentation you should
## ##' read for the drop.tip function can be found here: \link[ape]{drop.tip}
## ##'
## ##' @seealso
## ##' \link[ape]{drop.tip}
## setMethod("drop.tip", signature(object="phylo"),
##           function(object, tip, ...){
##               ape::drop.tip(object, tip, ...)
##           })
