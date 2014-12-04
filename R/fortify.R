##' @title fortify
##' @param model phylo object
##' @param data not use here
##' @param ... additional parameter
##' @return data.frame
##' @importFrom ggplot2 fortify
##' @method fortify phylo
##' @export
##' @author Yu Guangchuang
fortify.phylo <- function(model, data, ...) {
    df <- as.data.frame(model)
    idx <- is.na(df$parent)
    df$parent[idx] <- df$node[idx]
    rownames(df) <- df$node
    return(df)
}

##' @title as.data.frame
##' @param x phylo object
##' @param ... additional parameter
##' @return data.frame
##' @method as.data.frame phylo
##' @export
##' @author Yu Guangchuang
as.data.frame.phylo <- function(x, ...) {
    tip.label <- x[["tip.label"]]
    Ntip <- length(tip.label)
    N <- getNodeNum(x)
    
    edge <- as.data.frame(x[["edge"]])
    colnames(edge) <- c("parent", "node")
    if (is.null(x$edge.length)) {
        x$edge.length <- rep(1, nrow(edge))
    }
    
    edge$length <- x$edge.length
    
    xpos <- getXcoord(x)
    ypos <- getYcoord(x)
    xypos <- data.frame(node=1:N, x=xpos, y=ypos)

    res <- merge(edge, xypos, by.x="node", by.y="node", all.y=TRUE)
    label <- rep(NA, N)
    label[1:Ntip] <- tip.label
    if ( !is.null(x$node.label) ) {
        label[(Ntip+1):N] <- x$node.label
    }
    res$label <- label
    isTip <- rep(FALSE, N)
    isTip[1:Ntip] <- TRUE
    res$isTip <- isTip

    return(res)
}
