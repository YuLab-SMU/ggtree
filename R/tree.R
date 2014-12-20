
layout.unrooted <- function(tree) {
    df <- fortify(tree)
    N <- getNodeNum(tree)
    nb.sp <- sapply(1:N, function(i) length(get.offspring.tip(tree, i)))
    layout.unrooted_<- function(curNode, start, end) {
        curNtip <- nb.sp[curNode]
        child <- getChild(tree, curNode)
        if (length(child) > 0) {
            for (i in seq_along(child)){
                iChild <- child[i]
                ntip.child <- nb.sp[iChild]
                ratio.child <- ntip.child/curNtip
                length.child <- df[df$node == iChild, "length"]
                
                alpha <- (end - start) * ratio.child
                beta <- start + alpha / 2

                x.child <- df[df$node == curNode, "x"] + cospi(beta) * length.child
                y.child <- df[df$node == curNode, "y"] + sinpi(beta) * length.child
                
                df$x[df$node == iChild] <<- x.child
                df$y[df$node == iChild] <<- y.child

                layout.unrooted_(iChild, start, start+alpha)
                start <- start + alpha
            }
        }
    }
     
    layout.unrooted_(getRoot(tree), -1, 1)

    return(df)
}

##' @importFrom ape extract.clade
get.offspring.tip <- function(tr, node) {
    if ( ! node %in% tr$edge[,1]) {
        ## return itself
        return(tr$tip.label[node])
    }
    clade <- extract.clade(tr, node)
    clade$tip.label
}

getNodeNum <- function(tr) {
    Ntip <- length(tr[["tip.label"]])
    Nnode <- tr[["Nnode"]]
    ## total nodes
    N <- Ntip + Nnode 
    return(N)
}

getParent <- function(tr, node) {
    if ( node == getRoot(tr) )
        return(0)
    edge <- tr[["edge"]]
    parent <- edge[,1]
    child <- edge[,2]
    res <- parent[child == node]
    if (length(res) == 0) {
        stop("cannot found parent node...")
    }
    if (length(res) > 1) {
        stop("multiple parent found...")
    }
    return(res)
}

getChild <- function(tr, node) {
    edge <- tr[["edge"]]
    res <- edge[edge[,1] == node, 2]
    ## if (length(res) == 0) {
    ##     ## is a tip
    ##     return(NA)
    ## }
    return(res)
}

isRoot <- function(tr, node) {
    getRoot(tr) == node
}



getRoot <- function(tr) {
    edge <- tr[["edge"]]
    ## 1st col is parent,
    ## 2nd col is child,
    parent <- unique(edge[,1])
    child <- unique(edge[,2])
    ## the node that has no parent should be the root
    root <- parent[ ! parent %in% child ]
    if (length(root) > 1) {
        stop("multiple roots founded...")
    }
    return(root)
}


getXcoord <- function(tr) {
    edge <- tr$edge
    parent <- edge[,1]
    child <- edge[,2]
    root <- getRoot(tr)

    len <- tr$edge.length

    N <- getNodeNum(tr)
    x <- numeric(N)
    x[-root] <- NA  ## only root is set to 0
    currentNode <- root
    while(any(is.na(x))) {
        idx <- which(parent %in% currentNode)
        newNode <- child[idx]
        x[newNode] <- x[parent[idx]]+len[idx]
        currentNode <- newNode
    }

    return(x)
}

##' @importFrom magrittr %>%
##' @importFrom magrittr equals
getYcoord <- function(tr) {
    Ntip <- length(tr[["tip.label"]])
    N <- getNodeNum(tr)

    edge <- tr[["edge"]]
    parent <- edge[,1]
    child <- edge[,2]
   
    y <- numeric(N)
    tip.idx <- child[child <= Ntip]
    y[tip.idx] <- 1:Ntip
    y[-tip.idx] <- NA

    currentNode <- 1:Ntip
    while(any(is.na(y))) {
        pNode <- child %in% currentNode %>% which %>%
            parent[.] %>% unique
        idx <- sapply(pNode, function(i) all(child[parent == i] %in% currentNode))
        newNode <- pNode[idx]
        
        y[newNode] <- sapply(newNode, function(i) {
            child[parent == i] %>% y[.] %>% mean(na.rm=TRUE)           
        })
        
        currentNode <- parent %in% newNode %>% child[.] %>%
            `%in%`(currentNode, .) %>% `!` %>%
                currentNode[.] %>% c(., newNode)
    }
    
    return(y)
}
