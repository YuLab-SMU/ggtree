##' @method fortify jplace
##' @importFrom ape read.tree
##' @export
fortify.jplace <- function(model, data, ladderize=TRUE, right=FALSE, ...) {
    tree.text <- get.tree(model)
    ## move edge label to node label separate by @
    tr <- gsub('(:[0-9.e-]+)\\{(\\d+)\\}', '\\@\\2\\1', tree.text)
    tree <- read.tree(text=tr)
    if (ladderize == TRUE) {
        tree <- ladderize(tree, right = right)
    }
    df <- fortify.phylo(tree)
    df$edge <- as.numeric(gsub("[^@]*@(\\d*)", "\\1",df$label))
    ## remove edge label from node label
    df$label <- gsub("@\\d*", "", df$label)
    df$label[df$label == ""] <- NA
    
    return(df)
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
        newNode <-
            which(child %in% currentNode) %>% 
                parent[.] %>%
                    table %>% equals(2) %>%  ## has 2 children
                        which %>%
                            names %>% 
                                as.numeric

       y[newNode] <- sapply(newNode, function(i) {
            child[parent == i] %>% y[.] %>% mean(na.rm=T)           
        })

        currentNode <- parent %in% newNode %>% child[.] %>%
            `%in%`(currentNode, .) %>% `!` %>%
                currentNode[.] %>% c(., newNode)
    }
    
    return(y)
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

## . function was from plyr package
. <- function (..., .env = parent.frame()) {
    structure(as.list(match.call()[-1]), env = .env, class = "quoted")
}

roundDigit <- function(d) {
    i <- 0
    while(d < 1) {
        d <- d * 10
        i <- i + 1
    }
    round(d)/10^i
}

get.tree.jplace <- function(object, ...) {
    object@tree
}
