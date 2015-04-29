##' merge two tree object
##'
##' 
##' @title merge_tree
##' @param obj1 tree object 1
##' @param obj2 tree object 2
##' @return tree object
##' @importFrom magrittr %<>%
##' @importFrom ape Ntip
##' @export
##' @author Guangchuang Yu
merge_tree <- function(obj1, obj2) {
    
    if (has.slot(obj1, "extraInfo") == FALSE) {
        stop("input tree object is not supported...")
    }
    
    if ((is.tree(obj1) & is.tree(obj2)) == FALSE) {
        stop("input should be tree objects...")
    }

    tr1 <- get.tree(obj1)
    tr2 <- get.tree(obj2)

    if (getNodeNum(tr1) != getNodeNum(tr2)) {
        stop("number of nodes not equals...")
    }

    if (Ntip(tr1) != Ntip(tr2)) {
        stop("number of tips not equals...")
    }
    
    if (all(tr1$tip.label %in% tr2$tip.label) == FALSE) {
        stop("tip names not match...")
    }


    ## order tip.label in tr2 as in tr1
    ## mapping corresponding ID
    idx <- match(tr2$tip.label, tr1$tip.label)
    tr2$edge[match(1:Ntip(tr2), tr2$edge[,2]), 2] <- idx
    tr2$tip.label <- tr1$tip.label

    node_map <- list()
    node_map$from %<>% c(1:Ntip(tr2))
    node_map$to %<>% c(idx)

    root <- getRoot(tr1)
    root.2 <- getRoot(tr2)
    tr2$edge[tr2$edge[,1] == root.2, 1] <-  root

    node_map$from %<>% c(root.2)
    node_map$to %<>% c(root)

    
    currentNode <- 1:Ntip(tr1)
    while(length(currentNode)) {
        p1 <- sapply(currentNode, getParent, tr=tr1)
        p2 <- sapply(currentNode, getParent, tr=tr2)

        if (length(p1) != length(p2)) {
            stop("trees are not identical...")
        }

        jj <- match(p2, tr2$edge[,1])
        if (length(jj)) {
            notNA <- which(!is.na(jj))
            jj <- jj[notNA]
        }
        if (length(jj)) {
            tr2$edge[jj,1] <- p1[notNA]
        }

        
        ii <- match(p2, tr2$edge[,2])
        if (length(ii)) {
            notNA <- which(!is.na(ii))
            ii <- ii[notNA]
        }
        if (length(ii)) {
            tr2$edge[ii,2] <- p1[notNA]
        }

        node_map$from %<>% c(p2)
        node_map$to %<>% c(p1)
        
        ## parent of root will return 0, which is in-valid node ID
        currentNode <- unique(p1[p1 != 0])
    }

    if ( any(tr2$edge != tr2$edge) ) {
        stop("trees are not identical...")
    }
    
    node_map.df <- do.call("cbind", node_map)
    node_map.df <- unique(node_map.df)
    node_map.df <- node_map.df[node_map.df[,1] != 0,]
    i <- order(node_map.df[,1], decreasing = FALSE)
    node_map.df <- node_map.df[i,]

    info2 <- fortify(obj2)
    info2$node <- node_map.df[info2$node, 2]
    info2$parent <- node_map.df[info2$parent, 2]

    cn <- colnames(info2)
    i <- match(c("x", "y", "isTip", "label", "branch", "branch.length"), cn)
    i <- i[!is.na(i)]
    info2 <- info2[, -i]

    extraInfo <- obj1@extraInfo
    if (nrow(extraInfo) == 0) {
        obj1@extraInfo <- info2
    } else {
        info <- merge(extraInfo, info2, by.x =c("node", "parent"), by.y = c("node", "parent"))
        obj1@extraInfo <- info
    }
    
    return(obj1)
}
