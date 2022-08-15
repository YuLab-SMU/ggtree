

calculate_angle <- function(data) {
    data$angle <- 360/(diff(range(data$y)) + 1) * data$y
    return(data)
}



scaleY <- function(phylo, df, yscale, layout, ...) {
    if (yscale == "none") {
        return(df)
    }
    if (! yscale %in% colnames(df)) {
        warning("yscale is not available...\n")
        return(df)
    }
    if (is.numeric(df[[yscale]])) {
        y <- getYcoord_scale_numeric(phylo, df, yscale, ...)
        ## if (order.y) {
        ##     y <- getYcoord_scale2(phylo, df, yscale)
        ## } else {
        ##     y <- getYcoord_scale(phylo, df, yscale)
        ## }
    } else {
        y <- getYcoord_scale_category(phylo, df, yscale, ...)
    }

    df[, "y"] <- y

    return(df)
}


adjust_hclust_tip.edge.len <- function(df, phylo){
    tip.edge.len <- attr(phylo, 'tip.edge.len')
    if (!is.null(tip.edge.len)){
        mx <- max(df$x, na.rm=TRUE)
        df$x <- df$x - mx
        df$branch <- df$branch - mx
        df[df$isTip, "x", drop=TRUE] <- tip.edge.len
        attr(df, 'revts.done') = TRUE
    }                       
    return(df)
}

##
##
## old version of fortify.phylo
## now use utilities from tidytree
##
##
## ##' fortify a phylo to data.frame
## ##'
## ##'
## ##' @rdname fortify
## ##' @title fortify
## ##' @param model phylo object
## ##' @param data not use here
## ##' @param layout layout
## ##' @param ladderize ladderize, logical
## ##' @param right logical
## ##' @param mrsd most recent sampling date
## ##' @param as.Date logical whether using Date class in time tree
## ##' @param ... additional parameter
## ##' @return data.frame
## ##' @importFrom ape ladderize
## ##' @importFrom ape reorder.phylo
## ##' @importFrom ggplot2 fortify
## ##' @method fortify phylo
## ##' @export
## ##' @author Yu Guangchuang
## fortify.phylo <- function(model, data,
##                           layout    = "rectangular",
##                           ladderize = TRUE,
##                           right     = FALSE,
##                           mrsd      = NULL,
##                           as.Date   = FALSE, ...) {
##     ## tree <- reorder.phylo(model, 'postorder')
##     tree <- model
##     if (ladderize == TRUE) {
##         tree <- ladderize(tree, right=right)
##     }
##     if (! is.null(tree$edge.length)) {
##         if (anyNA(tree$edge.length)) {
##             warning("'edge.length' contains NA values...\n## setting 'edge.length' to NULL automatically when plotting the tree...")
##             tree$edge.length <- NULL
##         }
##     }
##     df <- as.data.frame(tree, layout=layout, ...)
##     idx <- is.na(df$parent)
##     df$parent[idx] <- df$node[idx]
##     rownames(df) <- df$node
##     cn <- colnames(df)
##     colnames(df)[grep("length", cn)] <- "branch.length"
##     if(layout == "slanted") {
##         df <- add_angle_slanted(df)
##     }
##     aa <- names(attributes(tree))
##     group <- aa[ ! aa %in% c("names", "class", "order", "reroot", "node_map")]
##     if (length(group) > 0) {
##         for (group_ in group) {
##             ## groupOTU & groupClade
##             group_info <- attr(tree, group_)
##             if (length(group_info) == nrow(df)) {
##                 df[, group_] <- group_info
##             }
##         }
##     }
##     if (!is.null(mrsd)) {
##         df <- scaleX_by_time_from_mrsd(df, mrsd, as.Date)
##     }
##     return(df)
## }

## ##' convert phylo to data.frame
## ##'
## ##'
## ##' @title as.data.frame
## ##' @param x phylo object
## ##' @param row.names omitted here
## ##' @param optional omitted here
## ##' @param layout layout
## ##' @param ... additional parameter
## ##' @return data.frame
## ##' @method as.data.frame phylo
## ##' @export
## ##' @author Yu Guangchuang
## as.data.frame.phylo <- function(x, row.names, optional,
##                                 layout="rectangular", ...) {
##     if (layout %in% c("equal_angle", "daylight")) {
##         return(layout.unrooted(x, layout.method = layout, ...))
##     }
##     as.data.frame.phylo_(x, layout, ...)
## }


## ## used by layoutEqualAngle
## ## will change to tidytree::as_tibble in future
## as.data.frame.phylo_ <- function(x, layout="rectangular",
##                                  branch.length="branch.length", ...) {
##     if (branch.length != 'none') {
##         branch.length = "branch.length"
##     }
##     tip.label <- x[["tip.label"]]
##     Ntip <- length(tip.label)
##     N <- getNodeNum(x)
##     edge <- as.data.frame(x[["edge"]])
##     colnames(edge) <- c("parent", "node")
##     if (! is.null(x$edge.length)) {
##         edge$length <- x$edge.length
##         if (branch.length == "none") {
##             xpos <- getXcoord_no_length(x)
##             ypos <- getYcoord(x)
##         } else {
##             xpos <- getXcoord(x)
##             ypos <- getYcoord(x)
##         }
##         ## } else  if (layout != "cladogram") {
##         ##     xpos <- getXcoord(x)
##         ##     ypos <- getYcoord(x)
##         ## } else {
##         ##     ## layout == "cladogram" && branch.length != "none"
##         ##     xy <- getXYcoord_cladogram(x)
##         ##     xpos <- xy$x
##         ##     ypos <- xy$y
##         ## }
##     } else {
##         xpos <- getXcoord_no_length(x)
##         ypos <- getYcoord(x)
##     }
##     xypos <- data.frame(node=1:N, x=xpos, y=ypos)
##     res <- merge(edge, xypos, by.x="node", by.y="node", all.y=TRUE)
##     label <- rep(NA, N)
##     label[1:Ntip] <- tip.label
##     if ( !is.null(x$node.label) ) {
##         label[(Ntip+1):N] <- x$node.label
##     }
##     res$label <- label
##     isTip <- rep(FALSE, N)
##     isTip[1:Ntip] <- TRUE
##     res$isTip <- isTip
##     ## add branch mid position
##     res <- calculate_branch_mid(res)
##     ## ## angle for all layout, if 'rectangular', user use coord_polar, can still use angle
##     res <- calculate_angle(res)
##     return(res)
## }
