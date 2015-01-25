##' convert polytomy to binary tree
##'
##' as.binary method for \code{phylo} object
##' @rdname as.binary
##' @return binary tree
##' @method as.binary phylo
##' @importFrom ape is.binary.tree
##' @export
##' @author Guangchuang Yu \url{http://ygc.name}
##' @examples
##' require(ape)
##' tr <- read.tree(text="((A, B, C), D);")
##' is.binary.tree(tr)
##' tr2 <- as.binary(tr)
##' is.binary.tree(tr2)
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


##' remove singleton
##'
##' 
##' @title rm.singleton.newick
##' @param nwk newick file
##' @param outfile output newick file 
##' @return tree text
##' @importFrom magrittr %<>%
##' @importFrom magrittr add
##' @importFrom ape write.tree
##' @author Guangchuang Yu \url{http://ygc.name}
rm.singleton.newick <- function(nwk, outfile = NULL) {    
    tree <- readLines(nwk)

    ## remove singleton of tips
    nodePattern <- "\\w+:[\\.0-9]+"
    singletonPattern.with.nodename <- paste0(".*(\\(", nodePattern, "\\)\\w+:[\\.0-9]+).*")
    singletonPattern.wo.nodename <- paste0(".*(\\(", nodePattern, "\\)[\\.0-9]+).*")
    
    while(length(grep("\\([^,]+\\)", tree)) > 0) {
        singleton <- gsub(singletonPattern.with.nodename, "\\1", tree)
        if (singleton == tree) {
            singleton <- gsub(singletonPattern.wo.nodename, "\\1", tree)
        }
        if (singleton == tree) {
            stop("can't parse singleton node...")
        }

        tip <- gsub("\\((\\w+).*", "\\1", singleton)
        
        len1 <- gsub(".*[^\\.0-9]+([\\.0-9]+)", "\\1", singleton)
        len2 <- gsub(".*:([.0-9]+)\\).*", "\\1", singleton)
        len <- as.numeric(len1) + as.numeric(len2)
        
        tree <- gsub(singleton, paste0(tip, ":", len), tree, fixed = TRUE)
    }

    tree <- read.tree(text=tree)

    ### remove singleton of internal nodes
    p.singleton <- which(table(tree$edge[,1]) == 1)
    if (length(p.singleton) > 0) {
        p.singleton %<>% names %>% as.numeric
        edge <- tree$edge
        idx <- which(edge[,1] == p.singleton)
        singleton <- edge[idx, 2]
        sidx <- which(edge[,1] == singleton)
        edge[sidx,1] <- p.singleton
        edge <- edge[-idx,]
        tree$edge <- edge
        tree$edge.length[sidx] %<>% add(., tree$edge.length[idx])
        tree$edge.length <- tree$edge.length[-idx]
    }
    
    if (!is.null(outfile)) {
        write.tree(tree, file=outfile)
    }
    invisible(tree)
}

##' @method fortify beast
##' @export
fortify.beast <- function(model, data,
                          layout    = "phylogram",
                          yscale    = "none",
                          ladderize = TRUE,
                          right     =FALSE,
                          ndigits   = NULL, ...) {

    phylo <- get.tree(model)
    df    <- fortify(phylo, layout=layout,
                     ladderize=ladderize, right=right, ...)
    
    stats <- model@stats

    idx <- which(colnames(stats) != "node")
    for (ii in idx) {
        if (is.character_beast(stats, ii)) {
            len <- sapply(stats[,ii], length)
            if (any(len > 1)) {
                stats[,ii] %<>% sapply(., function(x) {
                    y <- unlist(x) %>% as.character %>% gsub("\"", "", .)
                    if (length(y) == 1) {
                        return(y)
                    } else {
                        return(paste0('{', paste0(y, collapse = ','), '}'))
                    }
                })
            } else {
                stats[,ii] %<>% unlist %>% as.character %>% gsub("\"", "", .)
            }
            next
        }
        
        len <- sapply(stats[,ii], length)
        if ( all(len == 1) ) {
            stats[, ii] %<>% unlist %>% as.character %>% as.numeric
            if (!is.null(ndigits)) {
                stats[, ii] %<>% round(., ndigits)
            }
        } else if (all(len <= 2)) {
            stats[, ii] %<>% sapply(., function(x) {
                y <- unlist(x) %>% as.character %>% as.numeric
                if (!is.null(ndigits)) {
                    y %<>% round(., ndigits)
                }
                if (length(y) == 1) {
                    return(y)
                } else {
                    return(paste0('[', paste0(y, collapse = ','), ']'))
                }
            })
        } else {
            stats[,ii] %<>% sapply(., function(x) {
                y <- unlist(x) %>% as.character %>% as.numeric
                if (!is.null(ndigits)) {
                    y %<>% round(., ndigits)
                }
                if (length(y) == 1) {
                    return(y)
                } else {
                    return(paste0('{', paste0(y, collapse = ','), '}'))
                }
            })  
        }
    }
            
      
    cn <- colnames(stats)
    lo <- cn[grep("_lower", cn)]
    hi <- gsub("lower$", "upper", lo)
    rid <- gsub("_lower$", "", lo)
    
    for (i in seq_along(rid)) {
        stats[, rid[i]] <- paste0("[", stats[, lo[i]], ",", stats[, hi[i]], "]")
        stats[is.na(stats[, lo[i]]), rid[i]] <- NA
    }
    
    idx   <- match(df$node, stats$node)
    stats <- stats[idx,]
    stats <- stats[,colnames(stats) != "node"]
    
    df <- cbind(df, stats)
    scaleY(phylo, df, yscale)
}



##' @method fortify codeml
##' @export
fortify.codeml <- function(model, data,
                           layout        = "phylogram",
                           yscale        = "none",
                           ladderize     = TRUE,
                           right         = FALSE,
                           branch.length = "mlc.branch.length",
                           ndigits       = NULL, 
                           ...) {

    dNdS <- model@mlc@dNdS
    length <- match.arg(branch.length,
                        c("none",
                          "mlc.branch.length",
                          "rst.branch.length",
                          colnames(dNdS)[-c(1,2)])
                        )
    
    if (length == "rst.branch.length") {
        phylo <- get.tree(model@rst)
    } else {
        if (length == "mlc.branch.length") {
            length = "branch.length"
        }
        phylo <- fortify.codeml_mlc_(model@mlc, data, layout,
                                     ladderize, right,
                                     branch.length = length, ...)
    }
    
    df <- fortify(phylo, data, layout, ladderize, right,
                  branch.length=length, ...)
    
    res <- merge_phylo_anno.codeml_mlc(df, dNdS, ndigits)
    df <- merge_phylo_anno.paml_rst(res, model@rst)
    scaleY(phylo, df, yscale)
}


##' @method fortify codeml_mlc
##' @export
fortify.codeml_mlc <- function(model, data,
                               layout        = "phylogram",
                               yscale        = "none",
                               ladderize     = TRUE,
                               right         = FALSE,
                               branch.length = "branch.length",
                               ndigits       = NULL,
                               ...) {
        
    phylo <- fortify.codeml_mlc_(model, data, layout,
                                 ladderize, right,
                                 branch.length, ...)
    df <- fortify(phylo, data, layout, ladderize, right, branch.length=branch.length, ...)
    
    dNdS <- model@dNdS

    df <- merge_phylo_anno.codeml_mlc(df, dNdS, ndigits)
    scaleY(phylo, df, yscale)
}

merge_phylo_anno.codeml_mlc <- function(df, dNdS, ndigits = NULL) {
    if (!is.null(ndigits)) {
        idx <- which(! colnames(dNdS) %in% c("node", "parent"))
        for (ii in idx) {
            if (is.numeric(dNdS[, ii])) {
                dNdS[, ii] <- round(dNdS[,ii], ndigits)
            }
        }
    }
    
    res <- merge(df, dNdS,
                 by.x  = c("node", "parent"),
                 by.y  = c("node", "parent"),
                 all.x = TRUE)
    
    res[match(df$node, res$node),]
}

fortify.codeml_mlc_ <- function(model, data,
                                layout        = "phylogram",
                                ladderize     = TRUE,
                                right         = FALSE,
                                branch.length = "branch.length",                           
                                ...) {
    dNdS <- model@dNdS
    length <- match.arg(branch.length, c("none", "branch.length",
                                         colnames(dNdS)[-c(1,2)]))
    phylo <- get.tree(model)

    if (! length %in%  c("branch.length", "none")) {
        edge <- as.data.frame(phylo$edge)
        colnames(edge) <- c("parent", "node")
        
        dd <- merge(edge, dNdS,
                    by.x  = c("node", "parent"),
                    by.y  = c("node", "parent"),
                    all.x = TRUE)
        dd <- dd[match(edge$node, dd$node),]
        phylo$edge.length <- dd[, length]
    }
    
    return(phylo)
}

    
##' @method fortify paml_rst
##' @export
fortify.paml_rst <- function(model, data, layout = "phylogram", yscale="none",
                             ladderize=TRUE, right=FALSE, ...) {
    df <- fortify.phylo(model@phylo, data, layout, ladderize, right, ...)
    df <- merge_phylo_anno.paml_rst(df, model)
    scaleY(model@phylo, df, yscale)
}

merge_phylo_anno.paml_rst <- function(df, model) {
    for (type in get.fields(model)) {
        anno <- get.subs(model, type=type)
        colnames(anno)[2] <- type
        df <- df %add2% anno
    }
    return(df)
}


##' @method fortify hyphy
##' @export
fortify.hyphy <- fortify.paml_rst


##' @method fortify jplace
##' @importFrom ape read.tree
##' @export
fortify.jplace <- function(model, data,
                           layout="phylogram", yscale="none",
                           ladderize=TRUE, right=FALSE, ...) {
    df <- get.treeinfo(model, layout, ladderize, right, ...)
    place <- get.placements(model, by="best")
    df <- df %add2% place
    scaleY(model@phylo, df, yscale)
}

scaleY <- function(phylo, df, yscale) {
    if (yscale == "none") {
        return(df)
    }
    if (! yscale %in% colnames(df)) {
        warning("yscale is not available...\n")
        return(df)
    }
    if (! is.numeric(df[, yscale])) {
        warning("yscale should be numeric...\n")
        return(df)
    }
    y <- getYcoord_scale(phylo, df[, yscale])
    df[, "y"] <- y
    return(df)
}


##' @method fortify phylo4
##' @export
fortify.phylo4 <- function(model, data, layout="phylogram", yscale="none",
                           ladderize=TRUE, right=FALSE, ...) {
    fortify.phylo(as.phylo.phylo4(model), data,
                  layout, yscale, ladderize, right, ...)
}

as.phylo.phylo4 <- function(phylo4) {
    edge <- phylo4@edge
    edge <- edge[edge[,1] != 0, ]
    edge.length <- phylo4@edge.length
    edge.length <- edge.length[!is.na(edge.length)]
    tip.id <- sort(setdiff(edge[,2], edge[,1]))
    tip.label <- phylo4@label[tip.id]
    phylo <- list(edge = edge,
                  edge.length = edge.length,
                  tip.label = tip.label)
    
    node.id <- sort(unique(edge[,1]))
    node.id <- node.id[node.id != 0]
    node.label <- phylo4@label[node.id]
    if (!all(is.na(node.label))) {
        phylo$node.label <- node.label
    }
    phylo$Nnode <- length(node.id)
    class(phylo) <- "phylo"
    return(phylo)
}


##' @rdname fortify
##' @title fortify
##' @param model phylo object
##' @param data not use here
##' @param layout layout
##' @param ladderize ladderize, logical
##' @param right logical
##' @param ... additional parameter
##' @return data.frame
##' @importFrom ape ladderize
##' @importFrom ggplot2 fortify
##' @method fortify phylo
##' @export
##' @author Yu Guangchuang
fortify.phylo <- function(model, data, layout="phylogram", 
                          ladderize=TRUE, right=FALSE, ...) {
    if (ladderize == TRUE) {
        tree <- ladderize(model, right=right)
    } else {
        tree <- model
    }
    
    df <- as.data.frame(tree, layout=layout, ...)
    idx <- is.na(df$parent)
    df$parent[idx] <- df$node[idx]
    rownames(df) <- df$node
    cn <- colnames(df)
    colnames(df)[grep("length", cn)] <- "branch.length"
    return(df)
}

##' @title as.data.frame
##' @param x phylo object
##' @param row.names omitted here
##' @param optional omitted here
##' @param layout layout
##' @param ... additional parameter
##' @return data.frame
##' @method as.data.frame phylo
##' @export
##' @author Yu Guangchuang
as.data.frame.phylo <- function(x, row.names, optional,
                                layout="phylogram", ...) {
    if (layout == "unrooted") {
        return(layout.unrooted(x))
    } 
    as.data.frame.phylo_(x, layout, ...)
}

as.data.frame.phylo_ <- function(x, layout="phylogram",
                                 branch.length="branch.length", ...) {
    tip.label <- x[["tip.label"]]
    Ntip <- length(tip.label)
    N <- getNodeNum(x)
    
    edge <- as.data.frame(x[["edge"]])
    colnames(edge) <- c("parent", "node")
    if (! is.null(x$edge.length)) {
        edge$length <- x$edge.length
        if (branch.length == "none") {
            xpos <- getXcoord_no_length(x)
            ypos <- getYcoord(x)
        } else  if (layout != "cladogram") {
            xpos <- getXcoord(x)
            ypos <- getYcoord(x)
        } else {
            ## layout == "cladogram" && branch.length != "none"
            xy <- getXYcoord_cladogram(x)
            xpos <- xy$x
            ypos <- xy$y
        }
    } else {
        xpos <- getXcoord_no_length(x)
        ypos <- getYcoord(x)
    }
    
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
    res$branch <- (res$x[res$parent] + res$x)/2
    if (!is.null(res$length)) {
        res$length[is.na(res$length)] <- 0
    }
    res$branch[is.na(res$branch)] <- 0
    return(res)
}


