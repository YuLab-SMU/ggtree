##' Fortify tree objects for plotting
##'
##' Coordinates are computed by `treelayout::tree_layout()`; the layout
##' algorithms themselves live in that package. This method only wires the
##' result into the `ggplot2` `fortify()` interface.
##'
##' @inheritParams treelayout::tree_layout
##' @importFrom treelayout tree_layout
##' @method fortify phylo
##' @export
fortify.phylo <- function(model, data,
                          layout        = "rectangular",
                          ladderize     = TRUE,
                          right         = FALSE,
                          branch.length = "branch.length",
                          mrsd          = NULL,
                          as.Date       = FALSE,
                          yscale        = "none",
                          root.position = 0,
                          ...) {

    tree_layout(model, data, layout = layout, ladderize = ladderize,
                right = right, branch.length = branch.length, mrsd = mrsd,
                as.Date = as.Date, yscale = yscale, root.position = root.position,
                ...)
}


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
    attr(df, "layout") <- layout
    return(df)
}

##' @method fortify treedataList
##' @export
fortify.treedataList <- fortify.multiPhylo

##' @importFrom ggplot2 fortify
##' @method fortify treedata
##' @export
fortify.treedata <- function(model, data,
                             layout        = "rectangular",
                             yscale        = "none",
                             ladderize     = TRUE,
                             right         = FALSE,
                             branch.length = "branch.length",
                             mrsd          = NULL,
                             as.Date       = FALSE, ...) {

    ## `set_branch_length(., "none")` drops the edge lengths, and with them the
    ## `branch.length` column. `fortify.phylo()` *keeps* that column for a plain
    ## `phylo` under `branch.length = "none"`, so the two disagreed, and any
    ## geom that maps `branch.length` (`geom_rootedge()`, `geom_balance()`, ...)
    ## aborted with "object 'branch.length' not found" for a `treedata` input
    ## while working fine for a `phylo` one. Keep them consistent. #648
    branch_length_orig <- NULL
    if (identical(branch.length, "none")) {
        tbl <- as_tibble(model)
        if (all(c("node", "branch.length") %in% names(tbl))) {
            branch_length_orig <- tbl[, c("node", "branch.length")]
        }
    }

    model <- set_branch_length(model, branch.length)

    res <- fortify.phylo(model, data,
                  layout        = layout,
                  yscale        = yscale,
                  ladderize     = ladderize,
                  right         = right,
                  branch.length = branch.length,
                  mrsd          = mrsd,
                  as.Date       = as.Date, ...)

    if (!is.null(branch_length_orig) &&
        !"branch.length" %in% names(res) && "node" %in% names(res)) {
        res$branch.length <-
            branch_length_orig$branch.length[match(res$node, branch_length_orig$node)]
    }
    res
}


##' Fortify a phylo4 object
##'
##' hclust-like objects are converted by [treelayout::tree_layout()]; this
##' method only adapts the argument names.
##'
##' @param model a tree object.
##' @param data unused, kept for the `fortify()` signature.
##' @param layout layout name, see [treelayout::layout_names()].
##' @param yscale variable used to scale tips along y.
##' @param ladderize logical, ladderize before layout.
##' @param right logical, ladderize to the right.
##' @param mrsd most recent sampling date.
##' @param hang fraction of the plot height by which labels hang below the
##'   tree, for `dendrogram` layout of `hclust`-like input.
##' @param ... passed to the layout.
##'
##' @importFrom treelayout tree_layout
##' @method fortify phylo4
##' @export
fortify.phylo4 <- function(model, data,
                           layout    = "rectangular",
                           yscale    = "none",
                           ladderize = TRUE,
                           right     = FALSE,
                           mrsd      = NULL,
                           hang      = .1,
                           ...) {
    tree_layout(model, data, layout = layout, ladderize = ladderize,
                right = right, mrsd = mrsd, yscale = yscale, hang = hang, ...)
}


## `ape::as.phylo` (for `hclust`)

##' @method fortify hclust
##' @export
fortify.hclust <- fortify.phylo4

## `phylogram::as.phylo` (for `dendrogram`).

##' @method fortify dendrogram
##' @export
fortify.dendrogram <- fortify.phylo4

##' @method fortify agnes
##' @export
fortify.agnes <- fortify.phylo4

##' @method fortify diana
##' @export
fortify.diana <- fortify.phylo4

##' @method fortify twins
##' @export
fortify.twins <- fortify.phylo4

##' @method fortify phylog
##' @export
fortify.phylog <- fortify.phylo4

##' @method fortify igraph
##' @export
fortify.igraph <- fortify.phylo4

##' @method fortify linkage
##' @export
fortify.linkage <- fortify.phylo4

##' @method fortify dendro
##' @export
fortify.dendro <- fortify.phylo4

##' @method fortify phylo4d
##' @importFrom treeio as.treedata
##' @export
fortify.phylo4d <- function(model, data,
                            layout        = "rectangular",
                            yscale        = "none",
                            ladderize     = TRUE,
                            right         = FALSE,
                            branch.length = "branch.length",
                            mrsd          = NULL,
							hang          = 0.1,
                            ...) {
    model <- as.treedata(model, hang = hang)
    df <- fortify(model, data, layout, yscale, ladderize, right, branch.length, mrsd, ...)
    return (df)
}

##' @method fortify pvclust
##' @export
fortify.pvclust <- fortify.phylo4d

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

