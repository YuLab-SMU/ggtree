
##' update tree 
##'
##'
##' @rdname update.TREE
##' @title \%<\%
##' @param pg ggplot2 object
##' @param x update by x
##' @return updated ggplot object
##' @export
##' @author Yu Guangchuang
##' @examples
##' nwk <- system.file("extdata", "sample.nwk", package="ggtree")
##' tree <- read.tree(nwk)
##' p <- ggtree(tree) + geom_point(subset=.(!isTip), 
##'         	       color="#b5e521", alpha=1/4, size=10)
##' p %<% rtree(30)
`%<%` <- function(pg, x) {
    if (! is.tree(x)) {
        stop("input should be a tree object...")
    }
    pg %place% x
}

##' add annotation data to a tree
##'
##'
##' @rdname add.TREEDATA
##' @title \%<+\%
##' @param pg ggplot2 object
##' @param data annotation data
##' @return ggplot object with annotation data added
##' @export
##' @author Yu Guangchuang
##' @examples
##' nwk <- system.file("extdata", "sample.nwk", package="ggtree")
##' tree <- read.tree(nwk)
##' p <- ggtree(tree) 
##' dd <- data.frame(taxa=LETTERS[1:13], 
##'    		 place=c(rep("GZ", 5), rep("HK", 3), rep("CZ", 4), NA),
##'              value=round(abs(rnorm(13, mean=70, sd=10)), digits=1))
##' row.names(dd) <- NULL
##' p %<+% dd + geom_text(aes(color=place, label=label), hjust=-0.5)
`%<+%` <- function(pg, data) {
    if (! is.data.frame(data)) {
        stop("input should be a data.frame...")
    }
    pg %add% data
}


##' @importFrom ape reorder.phylo
layout.unrooted <- function(tree) {
    N <- getNodeNum(tree)
    root <- getRoot(tree)

    df <- as.data.frame.phylo_(tree)
    df$x <- NA
    df$y <- NA
    df$start <- NA
    df$end <- NA
    df[root, "x"] <- 0
    df[root, "y"] <- 0
    df[root, "start"] <- 0
    df[root, "end"] <- 2
    
    nb.sp <- sapply(1:N, function(i) length(get.offspring.tip(tree, i)))

    tree <- reorder.phylo(tree, "postorder")
    nodes <- tree$edge[,c(2,1)] %>% t %>%
        as.vector %>% rev %>% unique

    for(curNode in nodes) {
        curNtip <- nb.sp[curNode]
        children <- getChild(tree, curNode)
        
        start <- df[curNode, "start"]
        end <- df[curNode, "end"]
        
        if (length(children) == 0) {
            ## is a tip
            next
        }
        
        for (i in seq_along(children)) {
            child <- children[i]
            ntip.child <- nb.sp[child]
            
            alpha <- (end - start) * ntip.child/curNtip
            beta <- start + alpha / 2
            
            length.child <- df[child, "length"]
            df[child, "x"] <- df[curNode, "x"] + cospi(beta) * length.child
            df[child, "y"] <- df[curNode, "y"] + sinpi(beta) * length.child
            df[child, "start"] <- start
            df[child, "end"] <- start + alpha
            start <- start + alpha
        }
        
    }
    
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

getNodeName <- function(tr) {
    if (is.null(tr$node.label)) {
        n <- length(tr$tip.label)
        nl <- (n + 1):(2 * n - 2)
        nl <- as.character(nl)
    }
    else {
        nl <- tr$node.label
    }
    nodeName <- c(tr$tip.label, nl)
    return(nodeName)
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

getXcoord2 <- function(x, root, parent, child, len, start=0, rev=FALSE) {
    x[root] <- start
    x[-root] <- NA  ## only root is set to start, by default 0
    len[root] <- 0
    
    currentNode <- root
    direction <- 1
    if (rev == TRUE) {
        direction <- -1
    }
    while(any(is.na(x))) {
        idx <- which(parent %in% currentNode)
        newNode <- child[idx]
        x[newNode] <- x[parent[idx]]+len[idx] * direction
        currentNode <- newNode
    }

    return(x)
}

getXcoord_no_length <- function(tr) {
    edge <- tr$edge
    parent <- edge[,1]
    child <- edge[,2]
    root <- getRoot(tr)

    len <- tr$edge.length

    N <- getNodeNum(tr)
    x <- numeric(N)
    ntip <- Ntip(tr)
    currentNode <- 1:ntip
    x[-currentNode] <- NA

    while(any(is.na(x))) {
        idx <- match(currentNode, child)
        pNode <- parent[idx]
        ## child number table
        p1 <- table(parent[parent %in% pNode]) 
        p2 <- table(pNode)
        np <- names(p2)
        i <- p1[np] == p2
        newNode <- as.numeric(np[i])
        exclude <- c()
        for (j in newNode) {
            jj <- which(parent == j)
            x[j] <- min(x[child[jj]]) - 1
            exclude %<>% c(., child[jj])
        }
        
        currentNode %<>% `[`(!(. %in% exclude))
        currentNode %<>% c(., newNode) %>% unique
    }
    x <- x - min(x) 
    return(x)    
}


getXcoord <- function(tr) {
    edge <- tr$edge
    parent <- edge[,1]
    child <- edge[,2]
    root <- getRoot(tr)

    len <- tr$edge.length

    N <- getNodeNum(tr)
    x <- numeric(N)
    x <- getXcoord2(x, root, parent, child, len)
    return(x)
}

getXYcoord_cladogram <- function(tr) {
    
    edge <- tr$edge
    parent <- edge[,1]
    child <- edge[,2]
    root <- getRoot(tr)

    N <- getNodeNum(tr)
    len <- tr$edge.length
    y <- getYcoord(tr, step=min(len)/2)

    len <- sqrt(len^2 - (y[parent]-y[child])^2)
    x <- numeric(N)
    x <- getXcoord2(x, root, parent, child, len)
    res <- data.frame(x=x, y=y)
    return(res)
}


##' @importFrom magrittr %>%
##' @importFrom magrittr equals
getYcoord <- function(tr, step=1) {
    Ntip <- length(tr[["tip.label"]])
    N <- getNodeNum(tr)

    edge <- tr[["edge"]]
    parent <- edge[,1]
    child <- edge[,2]
   
    y <- numeric(N)
    tip.idx <- child[child <= Ntip]
    y[tip.idx] <- 1:Ntip * step
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
