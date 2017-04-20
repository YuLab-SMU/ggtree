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
        message("The input tree is already binary...")
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
##' @importFrom ape read.tree
##' @author Guangchuang Yu \url{http://ygc.name}
rm.singleton.newick <- function(nwk, outfile = NULL) {
    tree <- readLines(nwk)

    ## remove singleton of tips
    nodePattern <- "\\w+:[\\.0-9Ee\\+\\-]+"
    singletonPattern.with.nodename <- paste0(".*(\\(", nodePattern, "\\)\\w+:[\\.0-9Ee\\+\\-]+).*")
    singletonPattern.wo.nodename <- paste0(".*(\\(", nodePattern, "\\):[\\.0-9Ee\\+\\-]+).*")

    while(length(grep("\\([^,]+\\)", tree)) > 0) {
        singleton <- gsub(singletonPattern.with.nodename, "\\1", tree)
        if (singleton == tree) {
            singleton <- gsub(singletonPattern.wo.nodename, "\\1", tree)
        }
        if (singleton == tree) {
            stop("can't parse singleton node...")
        }

        tip <- gsub("\\((\\w+).*", "\\1", singleton)

        len1 <- gsub(".*[^\\.0-9Ee\\+\\-]+([\\.0-9Ee\\+\\-]+)", "\\1", singleton)
        len2 <- gsub(".*:([\\.0-9Ee\\+\\-]+)\\).*", "\\1", singleton)
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
        sidx <- which(edge[,2] == p.singleton)
        edge[sidx,2] <- edge[idx, 2]
        edge <- edge[-idx,]
        tree$edge <- edge
        tree$edge.length[sidx] %<>% add(., tree$edge.length[idx])
        tree$edge.length <- tree$edge.length[-idx]
        tree$Nnode <- tree$Nnode - 1
        if (!is.null(tree$node.label)) {
            tree$node.label <- tree$node.label[-(p.singleton - Ntip(tree))]
        }
    }

    if (!is.null(outfile)) {
        write.tree(tree, file=outfile)
    }
    invisible(tree)
}

##' @method fortify beast
##' @export
fortify.beast <- function(model, data,
                          layout        = "rectangular",
                          yscale        = "none",
                          ladderize     = TRUE,
                          right         = FALSE,
                          branch.length = "branch.length",
                          ndigits       = NULL,
                          mrsd = NULL, ...) {

    model <- set_branch_length(model, branch.length)
    phylo <- model@phylo
    df    <- fortify(phylo,
                     layout = layout,
                     branch.length = branch.length,
                     ladderize = ladderize,
                     right = right,
                     mrsd = mrsd, ...)

    stats <- model@stats

    scn <- colnames(stats)
    scn <- scn[scn != 'node']

    for (cn in scn) {
        if (cn %in% colnames(df)) {
            colnames(stats)[colnames(stats) == cn] <- paste0(cn, "_")
            msg <- paste("feature", cn, "was renamed to", paste0(cn, "_"), "due to name conflict...")
            warning(msg)
        }
    }

    idx <- which(colnames(stats) != "node")
    for (ii in idx) {
        if (is.character_beast(stats, ii)) {
            len <- sapply(stats[,ii], length)
            if (any(len > 1)) {
                stats[,ii] %<>% sapply(., function(x) {
                    y <- unlist(x) %>% as.character %>%
                        gsub("\"", "", .) %>% gsub("'", "", .)
                    if (length(y) == 1) {
                        return(y)
                    } else {
                        return(paste0('{', paste0(y, collapse = ','), '}'))
                    }
                })
            } else {
                stats[,ii] %<>% unlist %>% as.character %>%
                    gsub("\"", "", .) %>% gsub("'", "", .)
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
    cn_stats <- colnames(stats)
    stats <- stats[, cn_stats != "node"]

    df <- cbind(df, stats)
    if (is(stats, "data.frame") == FALSE) {
        colnames(df)[colnames(df) == "stats"] <- cn_stats[cn_stats != "node"]
    }

    df <- scaleY(phylo, df, yscale, layout, ...)

    append_extraInfo(df, model)
}


##' @method fortify codeml
##' @export
fortify.codeml <- function(model, data,
                           layout        = "rectangular",
                           yscale        = "none",
                           ladderize     = TRUE,
                           right         = FALSE,
                           branch.length = "mlc.branch.length",
                           ndigits       = NULL,
                           mrsd          = NULL,
                           ...) {

    dNdS <- model@mlc@dNdS
    if (branch.length == "branch.length") {
        message("branch.length setting to mlc.branch.length by default...")
        branch.length <- "mlc.branch.length"
    }
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
            length <- "branch.length"
        }
        mlc <- set_branch_length(model@mlc, length)
        phylo <- get.tree(mlc)
    }

    df <- fortify(phylo, data, layout, ladderize, right,
                  branch.length=length, mrsd=mrsd, ...)

    res <- merge_phylo_anno.codeml_mlc(df, dNdS, ndigits)
    df <- merge_phylo_anno.paml_rst(res, model@rst)
    df <- scaleY(phylo, df, yscale, layout, ...)

    append_extraInfo(df, model)
}


##' @method fortify codeml_mlc
##' @export
fortify.codeml_mlc <- function(model, data,
                               layout        = "rectangular",
                               yscale        = "none",
                               ladderize     = TRUE,
                               right         = FALSE,
                               branch.length = "branch.length",
                               ndigits       = NULL,
                               mrsd          = NULL,
                               ...) {

    model <- set_branch_length(model, branch.length)
    phylo <- get.tree(model)
    df <- fortify(phylo, data, layout, ladderize, right,
                  branch.length=branch.length, mrsd=mrsd, ...)

    dNdS <- model@dNdS

    df <- merge_phylo_anno.codeml_mlc(df, dNdS, ndigits)
    df <- scaleY(phylo, df, yscale, layout, ...)

    append_extraInfo(df, model)
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
                                layout        = "rectangular",
                                ladderize     = TRUE,
                                right         = FALSE,
                                branch.length = "branch.length",
                                ...) {

}



##' @method fortify paml_rst
##' @export
fortify.paml_rst <- function(model, data,
                             layout    = "rectangular",
                             yscale    = "none",
                             ladderize = TRUE,
                             right     = FALSE,
                             mrsd      = NULL,
                             ...) {
    df <- fortify.phylo(model@phylo, data, layout, ladderize, right, mrsd=mrsd, ...)
    df <- merge_phylo_anno.paml_rst(df, model)
    df <- scaleY(model@phylo, df, yscale, layout, ...)

    append_extraInfo(df, model)
}

merge_phylo_anno.paml_rst <- function(df, model) {
    types <- get.fields(model)
    types <- types[grepl('subs', types)]
    for (type in types) {
        anno <- get.subs(model, type=type)
        colnames(anno)[2] <- type
        df <- df %add2% anno
    }
    return(df)
}


##' @method fortify phangorn
##' @export
fortify.phangorn <- fortify.paml_rst


##' @method fortify hyphy
##' @export
fortify.hyphy <- fortify.paml_rst


##' @method fortify jplace
##' @importFrom ape read.tree
##' @importFrom treeio get.placements
##' @export
fortify.jplace <- function(model, data,
                           layout    = "rectangular",
                           yscale    = "none",
                           ladderize = TRUE,
                           right     = FALSE,
                           mrsd      = NULL,
                           ...) {
    df <- extract.treeinfo.jplace(model, layout, ladderize, right, mrsd=mrsd, ...)
    place <- get.placements(model, by="best")

    df <- df %add2% place

    df <- scaleY(model@phylo, df, yscale, layout, ...)

    append_extraInfo(df, model)
}

scaleY <- function(phylo, df, yscale, layout, ...) {
    if (yscale == "none") {
        return(df)
    }
    if (! yscale %in% colnames(df)) {
        warning("yscale is not available...\n")
        return(df)
    }
    if (is.numeric(df[, yscale])) {
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


##' @method fortify phylo4
##' @importFrom treeio as.phylo
##' @export
fortify.phylo4 <- function(model, data,
                           layout    = "rectangular",
                           yscale    = "none",
                           ladderize = TRUE,
                           right     = FALSE,
                           mrsd      = NULL,
                           ...) {
    phylo <- as.phylo(model)
    df <- fortify.phylo(phylo, data,
                        layout, ladderize, right, mrsd=mrsd, ...)
    scaleY(phylo, df, yscale, layout, ...)
}

##' @method fortify phylo4d
##' @export
fortify.phylo4d <- function(model, data,
                            layout        = "rectangular",
                            yscale        = "none",
                            ladderize     = TRUE,
                            right         = FALSE,
                            branch.length = "branch.length",
                            mrsd          = NULL,
                            ...) {
    ## model <- set_branch_length(model, branch.length)
    ## phylo <- as.phylo.phylo4(model)
    ## res <- fortify(phylo, data, layout, branch.length=branch.length,
    ##                ladderize, right, mrsd, ...)
    ## tdata <- model@data[match(res$node, rownames(model@data)), , drop=FALSE]
    ## df <- cbind(res, tdata)
    ## scaleY(as.phylo.phylo4(model), df, yscale, layout, ...)
    fortify(as.treedata(model), data, layout, yscale, ladderize, right, branch.length, mrsd, ...)
}



##' fortify a phylo to data.frame
##'
##'
##' @rdname fortify
##' @title fortify
##' @param model phylo object
##' @param data not use here
##' @param layout layout
##' @param ladderize ladderize, logical
##' @param right logical
##' @param mrsd most recent sampling date
##' @param as.Date logical whether using Date class in time tree
##' @param ... additional parameter
##' @return data.frame
##' @importFrom ape ladderize
##' @importFrom ape reorder.phylo
##' @importFrom ggplot2 fortify
##' @method fortify phylo
##' @export
##' @author Yu Guangchuang
fortify.phylo <- function(model, data,
                          layout    = "rectangular",
                          ladderize = TRUE,
                          right     = FALSE,
                          mrsd      = NULL,
                          as.Date   = FALSE, ...) {
    tree <- reorder.phylo(model, 'postorder')

    if (ladderize == TRUE) {
        tree <- ladderize(tree, right=right)
    }

    if (! is.null(tree$edge.length)) {
        if (anyNA(tree$edge.length)) {
            warning("'edge.length' contains NA values...\n## setting 'edge.length' to NULL automatically when plotting the tree...")
            tree$edge.length <- NULL
        }
    }

    df <- as.data.frame(tree, layout=layout, ...)
    idx <- is.na(df$parent)
    df$parent[idx] <- df$node[idx]
    rownames(df) <- df$node
    cn <- colnames(df)
    colnames(df)[grep("length", cn)] <- "branch.length"
    if(layout == "slanted") {
        df <- add_angle_slanted(df)
    }
    aa <- names(attributes(tree))
    group <- aa[ ! aa %in% c("names", "class", "order", "reroot", "node_map")]
    if (length(group) > 0) {
        for (group_ in group) {
            ## groupOTU & groupClade
            group_info <- attr(tree, group_)
            if (length(group_info) == nrow(df)) {
                df[, group_] <- group_info
            }
        }
    }

    if (!is.null(mrsd)) {
        df <- scaleX_by_time_from_mrsd(df, mrsd, as.Date)
    }
    return(df)
}

##' convert phylo to data.frame
##'
##'
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
                                layout="rectangular", ...) {
    if (layout %in% c("equal_angle", "daylight")) {
        return(layout.unrooted(x, layout.method = layout, ...))
    }
    as.data.frame.phylo_(x, layout, ...)
}

as.data.frame.phylo_ <- function(x, layout="rectangular",
                                 branch.length="branch.length", ...) {
    if (branch.length != 'none') {
        branch.length = "branch.length"
    }

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
        } else {
            xpos <- getXcoord(x)
            ypos <- getYcoord(x)
        }
        ## } else  if (layout != "cladogram") {
        ##     xpos <- getXcoord(x)
        ##     ypos <- getYcoord(x)
        ## } else {
        ##     ## layout == "cladogram" && branch.length != "none"
        ##     xy <- getXYcoord_cladogram(x)
        ##     xpos <- xy$x
        ##     ypos <- xy$y
        ## }
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

    ## add branch mid position
    res <- calculate_branch_mid(res)

    ## ## angle for all layout, if 'rectangular', user use coord_polar, can still use angle
    res <- calculate_angle(res)
    return(res)
}

## ##' @method fortify nhx
## ##' @export
## fortify.nhx <- function(model, data, layout= "rectangular",
##                         ladderize=TRUE, right=FALSE, mrsd=NULL, ...) {
##     df <- fortify(get.tree(model), layout=layout, ladderize=ladderize, right=right, mrsd=mrsd, ...)
##     df <- merge(df, model@nhx_tags, by.x="node", by.y="node", all.x=TRUE)
##     append_extraInfo(df, model)
## }


## ##' @method fortify raxml
## ##' @export
## fortify.raxml <- function(model, data, layout= "rectangular",
##                           ladderize=TRUE, right=FALSE, mrsd=NULL, ...) {
##     df <- fortify(get.tree(model), layout=layout, ladderize=ladderize, right=right, mrsd=mrsd, ...)
##     df <- merge(df, model@bootstrap, by.x="node", by.y="node", all.x=TRUE)
##     append_extraInfo(df, model)
## }

## ##' @method fortify apeBootstrap
## ##' @export
## fortify.apeBootstrap <- fortify.raxml


##' @method fortify multiPhylo
##' @export
fortify.multiPhylo <-  function(model, data,
                                layout    = "rectangular",
                                ladderize = TRUE,
                                right     = FALSE,
                                mrsd      = NULL, ...) {

    df.list <- lapply(model, function(x) fortify(x, layout=layout, ladderize=ladderize, right=right, mrsd=mrsd, ...))
    if (is.null(names(model))) {
        names(df.list) <- paste0("Tree ", "#", seq_along(model))
    } else {
        names(df.list) <- names(model)
    }
    df <- do.call("rbind", df.list)
    df$.id <- rep(names(df.list), times=sapply(df.list, nrow))
    df$.id <- factor(df$.id, levels=names(df.list))

    ## nNode <- sapply(df.list, nrow)
    ## nNode2 <- cumsum(c(0, nNode[-length(nNode)]))
    ## df$parent <- df$parent + rep(nNode2, times=nNode)
    return(df)
}

##' @method fortify phylip
##' @export
fortify.phylip <- function(model, data,
                           layout        = "rectangular",
                           ladderize     = TRUE,
                           right         = FALSE,
                           branch.length = "TREE",
                           mrsd          = NULL, ...) {
    trees <- get.tree(model)
    fortify(trees, layout=layout, ladderize = ladderize, right=right, mrsd=mrsd, ...)
}

##' @method fortify r8s
##' @export
fortify.r8s <- function(model, data,
                        layout        = "rectangular",
                        ladderize     = TRUE,
                        right         = FALSE,
                        branch.length = "TREE",
                        mrsd          = NULL, ...) {
    trees <- get.tree(model)
    branch.length %<>% match.arg(names(trees))
    phylo <- trees[[branch.length]]
    fortify(phylo, layout=layout, ladderize = ladderize, right=right, mrsd=mrsd, ...)
}

##' @method fortify obkData
##' @export
fortify.obkData <- function(model, data,
                            layout    = "rectangular",
                            ladderize = TRUE,
                            right     = FALSE,
                            mrsd      = NULL, ...) {

    df <- fortify(model@trees[[1]], layout=layout, ladderize=ladderize, right=right, mrsd=mrsd, ...)

    meta.df <- model@dna@meta
    meta.df <- data.frame(taxa=rownames(meta.df), meta.df)
    loc <- model@individuals
    loc <- data.frame(individualID=rownames(loc), loc)
    meta_loc <- merge(meta.df, loc, by="individualID")
    meta_loc <- meta_loc[,-1]

    df <- merge(df, meta_loc, by.x="label", by.y="taxa", all.x=TRUE)
    df <- df[order(df$node, decreasing = FALSE),]
    return(df)
}

##' @method fortify phyloseq
##' @export
fortify.phyloseq <- function(model, data,
                             layout    = "rectangular",
                             ladderize = TRUE,
                             right     = FALSE,
                             mrsd      = NULL, ...) {

    df <- fortify(model@phy_tree, layout=layout, ladderize=ladderize, right=right, mrsd=mrsd, ...)
    phyloseq <- "phyloseq"
    require(phyloseq, character.only=TRUE)
    psmelt <- eval(parse(text="psmelt"))
    dd <- psmelt(model)
    if ('Abundance' %in% colnames(dd)) {
        dd <- dd[dd$Abundance > 0, ]
    }

    data <- merge(df, dd, by.x="label", by.y="OTU", all.x=TRUE)
    spacing <- 0.02
    idx <- with(data, sapply(table(node)[unique(node)], function(i) 1:i)) %>% unlist
    data$hjust <- spacing * idx * max(data$x)
    ## data$hjust <- data$x + hjust

    data[order(data$node, decreasing = FALSE), ]
}


## fortify.cophylo <- function(model, data, layout="rectangular",
##                             ladderize=TRUE, right=FALSE, mrsd = NULL, ...) {
##     trees <- model$trees
##     df.list <- lapply(trees, function(x) fortify(x, layout=layout, ladderize=ladderize, right=right, mrsd=mrsd, ...))
##     df1 <- df.list[[1]]
##     df2 <- df.list[[2]]

##     df2$x <- max(df2$x) + df2$x * -1 + max(df1$x) * 1.1
##     df2$parent <- df2$parent+nrow(df1)
##     df <- rbind(df1, df2)

##     ggplot(df) + geom_tree()

## }



has.extraInfo <- treeio:::has.extraInfo
