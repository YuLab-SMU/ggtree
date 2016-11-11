#' Drop a tip
#' 
#' @param object An nhx object
#' @return An nhx object
#' @export
setGeneric (
	name = "drop.tip", 
	def = function( object, ... ) 
		{ standardGeneric("drop.tip") }
)


##' drop.tip method
##'
##'
##' @docType methods
##' @name drop.tip
##' @rdname drop.tip-methods
##' @aliases drop.tip,nhx
##' @exportMethod drop.tip
##' @author Casey Dunn \url{http://dunnlab.org}
##' @usage drop.tip(object, tip...)
setMethod("drop.tip", signature(object="nhx"),
		function(object, tip) {

			# label the internal tree nodes by their number
			object@phylo$node.label = NULL
			object@phylo$node.label = (length(object@phylo$tip.label)+1):max(object@phylo$edge)

			# Prepare the nhx object for subsampling
			object@nhx_tags$node = as.numeric(object@nhx_tags$node)
			object@nhx_tags = object@nhx_tags[order(object@nhx_tags$node),]

			# add a colmn that has labels for both tips and internal nodes
			object@nhx_tags$node.label = c(object@phylo$tip.label, as.character(object@phylo$node.label))

			# Will need to take different approaches for subsampling tips 
			# and internal nodes, add a column to make it easy to tell them apart
			object@nhx_tags$is_tip = object@nhx_tags$node <= length(object@phylo$tip.label)

			# Remove tips
			object@phylo = ape::drop.tip( object@phylo, tip )

			# Subsample the tags
			object@nhx_tags = object@nhx_tags[object@nhx_tags$node.label %in% (c(object@phylo$tip.label, as.character(object@phylo$node.label))),]

			# Update tip node numbers
			tip_nodes = object@nhx_tags$node.label[ object@nhx_tags$is_tip ]
			object@nhx_tags$node[ object@nhx_tags$is_tip ] = match(object@phylo$tip.label, tip_nodes)

			# Clean up
			object@nhx_tags$node.label = NULL
			object@nhx_tags$is_tip = NULL


            return(object)
		})
