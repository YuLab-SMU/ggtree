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
   
    y <- 1:N
    y[y>Ntip] <- NA

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
            child[parent == i] %>% y[.] %>% mean           
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
    N <- Ntip + Nnode ## 2 * Ntip - 1
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

## from plyr package
. <- function (..., .env = parent.frame()) {
    structure(as.list(match.call()[-1]), env = .env, class = "quoted")
}
