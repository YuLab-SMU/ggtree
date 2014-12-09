##' convert polytomy to binary tree
##'
##' as.binary method for \code{phylo} object
##' @title as.binary
##' @param tree phylo, object
##' @param ... additional parameter
##' @return binary tree
##' @method as.binary phylo
##' @importFrom ape is.binary.tree
##' @export
##' @author Guangchuang Yu \url{http://ygc.name}
as.binary.phylo <- function(tree, ...) {
    if(is.binary.tree(tree)) {
        cat("The input tree is already binary...\n")
        invisible(tree)
    }
    
    polyNode <- tree$edge[,1] %>% table %>% '>'(2) %>%
        which %>% names %>% as.numeric

    N <- getNodeNum(tree)
    ii <- 0
    for (pn in polyNode) {
        idx <- which(tree$edge[,1] == pn)
        while(length(idx) >2) {
            ii <- ii + 1
            newNode <- N+ii
            tree$edge[idx[-1],1] <- newNode
            newEdge <- matrix(c(tree$edge[idx[1],1], newNode), ncol=2)
            tree$edge <- rbind(tree$edge, newEdge)
            idx <- idx[-1]
        }
    }
        
    tree$Nnode <- tree$Nnode+ii
    tree$edge.length <- c(tree$edge.length, rep(0, ii))
    return(tree)
}




##' @title fortify
##' @param model phylo object
##' @param data not use here
##' @param ladderize ladderize, logical
##' @param right logical
##' @param ... additional parameter
##' @return data.frame
##' @importFrom ape ladderize
##' @importFrom ggplot2 fortify
##' @method fortify phylo
##' @export
##' @author Yu Guangchuang
fortify.phylo <- function(model, data, ladderize=TRUE, right=FALSE, ...) {
    if (ladderize == TRUE) {
        tree <- ladderize(model, right=right)
    } else {
        tree <- model
    }
    
    df <- as.data.frame(tree)
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

